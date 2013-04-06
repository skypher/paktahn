(in-package :pak)

(define-foreign-library libalpm
  (:unix (:or "libalpm.so.8" "libalpm.so.7" "libalpm.so.6" "libalpm.so.5" "libalpm.so.3" "libalpm.so"))
  (t (:default "libalpm")))

(use-foreign-library libalpm)

(defcfun ("alpm_version" alpm-version) :string)

(defvar *alpm-version*
  (mapcar #'parse-integer (split-sequence:split-sequence #\. (alpm-version)))
  "A list of the form (Major minor point) representing the libalmp version.")

;;; versioning
(defcfun "alpm_pkg_vercmp" :int (v1 :string) (v2 :string))

(defun version= (v1 v2)
  (zerop (alpm-pkg-vercmp v1 v2)))

(defun version< (v1 v2)
  (eql -1 (alpm-pkg-vercmp v1 v2)))

(defun version<= (v1 v2)
  (or (version< v1 v2)
      (version= v1 v2)))

(defun version> (v1 v2)
  (not (version<= v1 v2)))

(defun version>= (v1 v2)
  (not (version< v1 v2)))

(defun version-spec-satisfied-p (relation actual-ver demanded-ver)
  (declare (string relation actual-ver demanded-ver))
  (unless (member relation '("<" "<=" "=" ">=" ">") :test #'equal)
    (error "Bogus version relation specifier ~S" relation))
  (let ((relation-fn-name (intern (concatenate 'string "VERSION" relation)
                                  #.*package*)))
    (assert (fboundp relation-fn-name))
    (funcall relation-fn-name actual-ver demanded-ver)))

;;; lists helper
(defcfun "alpm_list_next" :pointer (pkg-iterator :pointer))
(defcstruct alpm-list-struct
    (data :pointer)
    (next :pointer)
    (prev :pointer))
(defctype alpm-list alpm-list-struct)

(defun alpm-list-getdata (item) (foreign-slot-value item 'alpm-list 'data))

(defun alpm-list->lisp (alpm-list)
  (loop for iter = alpm-list
        then (alpm-list-next iter)
        until (null-pointer-p iter)
          collect (alpm-list-getdata iter)))

;;; main alpm
(defcfun "alpm_initialize" :pointer (root :string) (dbpath :string) (err :pointer))

(defcfun ("alpm_get_localdb" alpm-option-get-localdb) :pointer (handle :pointer))

(defcfun "alpm_register_syncdb" :pointer (handle :pointer) (name :string) (siglevel :int))

(defcfun "alpm_unregister_all_syncdbs" :int)
(defcfun "alpm_db_unregister" :int (db :pointer))

(defvar *alpm-errno-ptr* (foreign-alloc :int :initial-element 0))

(defcfun "alpm_strerror" :string (errno :int))


(defun alpm-last-error ()
  (values (alpm-strerror (mem-ref *alpm-errno-ptr* :int)) *alpm-errno-ptr*))

(defvar *alpm-handle* nil)

(defun init-alpm ()
  (setf *alpm-handle* (alpm-initialize "/" "/var/lib/pacman" *alpm-errno-ptr*)))

(init-alpm)
(format t "ALPM initialized.~%")

(defun get-pacman-config ()
  (py-configparser:read-files
    (py-configparser:make-config) '("/etc/pacman.conf")))

(defun get-ignorepkg (&optional (config (get-pacman-config)))
  ;; God this was unpleasant. Is there an alternative to py-configparser?
  ;; Dare I fork it and add a restart to make this unnecessary?
  (let ((conf-options (py-configparser::section-options
                        (py-configparser::%get-section config "options"))))
    (split-sequence #\Space
                    (rest (assoc "IgnorePkg" conf-options :test #'equal)))))

(defun get-enabled-repositories (&optional (config (get-pacman-config)))
  (remove "options" (reverse (py-configparser:sections config))
                :test #'equalp))

(defun init-local-db ()
  (unless *alpm-handle* (error "No ALPM handle!"))
  (cons "local" (alpm-option-get-localdb *alpm-handle*)))

(defun init-sync-dbs ()
  (unless *alpm-handle* (error "No ALPM handle!"))
  (mapcar (lambda (name)
            (cons name (alpm-register-syncdb *alpm-handle* name (logior 1 2 5 6)))) ; FIXME enum grovel
          (get-enabled-repositories)))

(defparameter *local-db* nil)
(defparameter *sync-dbs* nil)

(defun init-dbs ()
  (setf *local-db* (init-local-db))
  (setf *sync-dbs* (init-sync-dbs)))

(init-dbs)
(format t "Databases initialized.~%")

(defun alpm-db-unregister-sync-dbs ()
  (every #'zerop
         (mapcar (lambda (db-spec)
                   (alpm-db-unregister (cdr db-spec)))
                 *sync-dbs*)))

(defun db-name->db-spec (db-name)
  (let ((db-spec (assoc db-name (cons *local-db* *sync-dbs*)
                        :test #'equalp)))
    (assert db-spec)
    db-spec))

;;;; packages
(defcfun "alpm_db_get_pkgcache" :pointer (db :pointer))

(defcfun "alpm_pkg_get_name" safe-string (pkg :pointer))
(defcfun "alpm_pkg_get_version" safe-string (pkg :pointer))
(defcfun "alpm_pkg_get_desc" safe-string (pkg :pointer))

(defcfun "alpm_dep_compute_string" safe-string (dep :pointer))

(defun map-db-packages (fn &key (db-list *sync-dbs*))
  "Search a database for packages. FN will be called for each
matching package object. DB-LIST must be a list of database
objects."
  (flet ((map-db (db-spec)
           (loop for pkg-iter = (alpm-db-get-pkgcache (cdr db-spec))
                 then (alpm-list-next pkg-iter)
                 until (null-pointer-p pkg-iter)
                 do (let ((pkg (alpm-list-getdata pkg-iter)))
                      (funcall fn db-spec pkg)))))
    (dolist (db-spec db-list)
      (map-db db-spec))))

;;;; groups
(defcfun "alpm_db_get_groupcache" :pointer (db :pointer))

(defun map-groups (fn &key (db-list *sync-dbs*))
  "Search a database for groups. FN will be called for each
matching package group object. DB-LIST must be a list of database
objects."
  (flet ((map-db (db-spec)
           (loop for grp-iter = (alpm-db-get-groupcache (cdr db-spec))
                 then (alpm-list-next grp-iter)
                 until (null-pointer-p grp-iter)
                 do (let ((grp (alpm-list-getdata grp-iter)))
                      (funcall fn db-spec (mem-ref grp 'safe-string))))))
    (dolist (db-spec db-list)
      (map-db db-spec))))

;;;; Pacman
(defparameter *pacman-lock* "/var/lib/pacman/db.lck")
(defparameter *pacman-binary* "pacman")

(defmacro with-pacman-lock (&body body)
  `(let (notified)
     (retrying
       (when (probe-file *pacman-lock*)
         (unless notified
           (info "Pacman is currently in use, waiting for it to finish...")
           (setf notified t))
         (sleep 1) ; maybe there's a better way? some ioctl?
         (retry)))
     ,@body))


(defun run-pacman (args &key (sudo-p t) capture-output-p force)
  (let ((args (append args (unless force (list "--needed")))))
    (with-pacman-lock
        (if sudo-p
            (run-program "sudo" (append (list *pacman-binary*) args)
                         :capture-output-p capture-output-p)
            (run-program *pacman-binary* args
                         :capture-output-p capture-output-p)))))

(defun remove-command (args)
  (etypecase args
    (string (run-pacman `("-R" ,args) :force t))
    (list (run-pacman args :force t)))
  (reset-cache))

(defun sync-command (&optional (args '("-Sy")))
  (run-pacman args)
  (reset-cache))

;;;; Lisp interface
(defun install-binary-package (db-name pkg-name &key dep-of force)
  "Use Pacman to install a package."
  ;; TODO: check whether it's installed already
  ;; TODO: take versions into account
  (when (equalp db-name "local")
    ;; TODO offer restarts: skip, reinstall from elsewhere
    (error "Can't install an already installed package."))
  (flet ((check-return-value (value &key update-cache)
           (unless (zerop value)
             (error "Pacman exited with non-zero status ~D" value))
           (when (and update-cache (zerop value))
             (update-local-cache pkg-name (package-remote-version pkg-name)))))
    (cond
      ;; For installing multiple binary dependencies of AUR packages.
      ((and (null db-name) (listp pkg-name))
       (let ((return-value (run-pacman (append '("-S" "--asdeps") pkg-name))))
         (check-return-value return-value)
         (when (zerop return-value)
           (mapcar #'(lambda (name)
                       (update-local-cache name (package-remote-version name)))
                   pkg-name))))
      ;; TODO: Ensure install-pkg-tarball handles dependencies.
      ;; TODO: Ensure local cache is updated after successful tarball install.
      ((customize-p pkg-name)
       (unwind-protect
            (progn
              (get-pkgbuild pkg-name)
              (setf (current-directory) pkg-name)
              (apply-customizations)
              (run-makepkg)
              (install-pkg-tarball :as-dep dep-of))
         (cleanup-temp-files pkg-name)))
      (dep-of
       (info "Installing binary package ~S from ~S as a dependency for ~S.~%"
             pkg-name db-name dep-of)
       (let ((return-value (run-pacman (list "-S" "--asdeps" pkg-name))))
         (check-return-value return-value :update-cache t)))
      ((eq db-name 'group)
       (info "Installing group ~S.~%" pkg-name)
       (let ((return-value (run-pacman (list "-S" pkg-name))))
         (check-return-value return-value)))
      (t
       (info "Installing binary package ~S from ~S.~%"
             pkg-name db-name)
       (let* ((fully-qualified-pkg-name (format nil "~A/~A" db-name pkg-name))
              (return-value (run-pacman (list "-S" fully-qualified-pkg-name)
                                        :force force)))
         (check-return-value return-value :update-cache t))))
    t))

(defcfun "alpm_pkg_get_provides" :pointer (pkg :pointer))

(defun find-providing-packages (provides-name)
  (maybe-refresh-cache)
  (let (providers)
    (map-cached-packages
      (lambda (db-name pkg-spec)
        (destructuring-bind (name version desc provides) pkg-spec
          (declare (ignore version desc))
          (when (member provides-name provides :test #'equalp
                        :key (compose #'first #'parse-dep))
            (push (cons db-name name) providers))))
      :include-groups nil)
      (loop for (pkg-name local-version . rest) in (installed-aur-packages) do
         (when (string= provides-name (get-pkg-provides pkg-name local-version))
            (push (cons "aur" pkg-name) providers)))
    providers))

