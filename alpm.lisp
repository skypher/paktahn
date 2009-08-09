
(in-package :pak)


(defparameter *pacman-lock* "/var/lib/pacman/db.lck")

(defmacro with-pacman-lock (&body body)
  `(progn
     (when (probe-file *pacman-lock*)
       (format t "Pacman is currently in use, waiting for it to finish..."))
     ,@body))


(define-foreign-library libalpm
  (:unix (:or "libalpm.so.3" "libalpm.so"))
  (t (:default "libalpm")))

(use-foreign-library libalpm)

(defcfun "alpm_initialize" :int)
(defcfun "alpm_option_set_root" :int (root :string))
(defcfun "alpm_option_set_dbpath" :int (root :string))

(defcfun "alpm_db_register_local" :pointer)
(defcfun "alpm_db_register_sync" :pointer (name :string))
(defcfun "alpm_db_get_pkgcache" :pointer (db :pointer))

(defcfun "alpm_list_next" :pointer (pkg-iterator :pointer))
(defcfun "alpm_list_getdata" :pointer (pkg-iterator :pointer))

(defcfun "alpm_pkg_get_name" :string (pkg :pointer))
(defcfun "alpm_pkg_get_version" :string (pkg :pointer))
(defcfun "alpm_pkg_get_desc" :string (pkg :pointer))

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
  (cons "local" (alpm-db-register-local)))

(defun init-sync-dbs ()
  (mapcar (lambda (name)
            (cons name (alpm-db-register-sync name)))
          (get-enabled-repositories)))

(defparameter *local-db* (init-local-db))
(defparameter *sync-dbs* (init-sync-dbs))

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

(defun package-installed-p (pkg-name)
  ;; TODO: cache names
  (map-db-packages (lambda (db-spec pkg)
                     (declare (ignore db-spec))
                     (when (equalp (alpm-pkg-get-name pkg) pkg-name)
                       (return-from package-installed-p t)))
                   :db-list (list *local-db*))
  nil)

(defun run-pacman (args &key capture-output-p)
  (with-pacman-lock
    ;; --noconfirm is a kludge because of SBCL's run-program bug
    ;; later offer interactive control of Pacman when this is fixed.
    (run-program "sudo" (append (list "pacman" "--noconfirm" "--needed")
                                args)
                 :capture-output-p capture-output-p)))
                 

(defun install-binary-package (db-name pkg-name)
  "Use Pacman to install a package."
  ;; TODO: check whether it's installed already
  ;; TODO: take versions into account
  (when (equalp db-name "local")
    ;; TODO offer restarts: skip, reinstall from elsewhere
    (error "Can't install an already installed package."))
  (format t "Installing binary package ~S from repository ~S.~%"
          pkg-name db-name)
    (let* ((fully-qualified-pkg-name (format nil "~A/~A" db-name pkg-name))
           (return-value (run-pacman (list "-S" fully-qualified-pkg-name))))
      (unless (zerop return-value)
        (warn "Pacman exited with non-zero status ~D" return-value))))

