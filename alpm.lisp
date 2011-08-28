(in-package :pak)

(define-foreign-library libalpm
  (:unix (:or "libalpm.so.6" "libalpm.so.5" "libalpm.so.3" "libalpm.so"))
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
(defcfun "alpm_list_getdata" :pointer (pkg-iterator :pointer))

(defun alpm-list->lisp (alpm-list)
  (loop for iter = alpm-list
        then (alpm-list-next iter)
        until (null-pointer-p iter)
          collect (alpm-list-getdata iter)))

;;; main alpm
(defcfun "alpm_initialize" :int)
(defcfun "alpm_option_set_root" :int (root :string))
(defcfun "alpm_option_set_dbpath" :int (root :string))

(if (>= (car *alpm-version*) 6)
    (defcfun ("alpm_option_get_localdb" alpm-option-get-localdb) :pointer)
    (defcfun ("alpm_db_register_local" alpm-db-register-local) :pointer))

(defcfun "alpm_db_register_sync" :pointer (name :string))

(defcfun "alpm_db_unregister_all" :int)
(defcfun "alpm_db_unregister" :int (db :pointer))

(defun alpm-db-unregister-sync-dbs ()
  (every #'zerop
         (mapcar (lambda (db-spec)
                   (alpm-db-unregister (cdr db-spec)))
                 *sync-dbs*)))

(defun init-alpm ()
  (alpm-initialize)
  (alpm-option-set-root "/")
  (alpm-option-set-dbpath "/var/lib/pacman"))

(init-alpm)

(defun get-pacman-config ()
  (py-configparser:read-files
    (py-configparser:make-config) '("/etc/pacman.conf")))

(defun get-enabled-repositories (&optional (config (get-pacman-config)))
  (remove "options" (reverse (py-configparser:sections config))
                :test #'equalp))

(defun init-local-db ()
  (if (>= (car *alpm-version*) 6)
      (cons "local" (alpm-option-get-localdb))
      (cons "local" (alpm-db-register-local))))

(defun init-sync-dbs ()
  (mapcar (lambda (name)
            (cons name (alpm-db-register-sync name)))
          (get-enabled-repositories)))

(defparameter *local-db* nil)
(defparameter *sync-dbs* nil)

(defun init-dbs ()
  (setf *local-db* (init-local-db))
  (setf *sync-dbs* (init-sync-dbs)))

(init-dbs)

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
(defcfun "alpm_db_get_grpcache" :pointer (db :pointer))
(defcfun "alpm_grp_get_name" safe-string (grp :pointer))

(defun map-groups (fn &key (db-list *sync-dbs*))
  "Search a database for groups. FN will be called for each
matching package group object. DB-LIST must be a list of database
objects."
  (flet ((map-db (db-spec)
           (loop for grp-iter = (alpm-db-get-grpcache (cdr db-spec))
                 then (alpm-list-next grp-iter)
                 until (null-pointer-p grp-iter)
                 do (let ((grp (alpm-list-getdata grp-iter)))
                      (funcall fn db-spec grp)))))
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
