
(in-package :pacman)

(define-foreign-library libalpm
  (:unix (:or "libalpm.so.3" "libalpm.so"))
  (t (:default "libalpm")))

(use-foreign-library libalpm)

(defcfun "alpm_initialize" :int)
(defcfun "alpm_option_set_root" :int (root :string))
(defcfun "alpm_option_set_dbpath" :int (root :string))

(defcfun "alpm_db_register_local" :pointer)
(defcfun "alpm_db_register_sync" :pointer (name :string))
(defcfun "alpm_db_getpkgcache" :pointer (db :pointer))

(defcfun "alpm_list_next" :pointer (pkg-iterator :pointer))
(defcfun "alpm_list_getdata" :pointer (pkg-iterator :pointer))

(defcfun "alpm_pkg_get_name" :string (pkg :pointer))
(defcfun "alpm_pkg_get_version" :string (pkg :pointer))
(defcfun "alpm_pkg_get_desc" :string (pkg :pointer))

(alpm-initialize)
(alpm-option-set-root "/")
(alpm-option-set-dbpath "/var/lib/pacman")

(defun get-pacman-config ()
  (py-configparser:read-files
    (py-configparser:make-config) '("/etc/pacman.conf")))

(defun get-enabled-repositories (&optional (config (get-pacman-config)))
  (remove "options" (py-configparser:sections config)
          :test #'equalp))

(defparameter *local-db* (cons "local" (alpm-db-register-local)))
(defparameter *sync-dbs* (mapcar (lambda (name)
                                   (cons name (alpm-db-register-sync name)))
                                 (get-enabled-repositories)))

(defun map-db-packages (fn &key (db-list *sync-dbs*))
  "Search a database for packages. FN will be called for each
matching package object. DB-LIST may be an atom or a list of database
objects."
  (flet ((map-db (db-spec)
           (loop for pkg-iter = (alpm-db-getpkgcache (cdr db-spec))
                 then (alpm-list-next pkg-iter)
                 until (null-pointer-p pkg-iter)
                 do (let ((pkg (alpm-list-getdata pkg-iter)))
                      (funcall fn db-spec pkg)))))
    (dolist (db-spec (ensure-list db-list))
      (map-db db-spec))))

