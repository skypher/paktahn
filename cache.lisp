
(in-package :pak)

(setf cl-store:*check-for-circs* nil)

(defparameter *cache-format-version* 0)

(defparameter *cache-meta-file* nil)
(defparameter *cache-contents-file* nil)

(defun init-cache-paths ()
  (flet ((conf (name)
           (config-file (make-pathname :directory '(:relative "cache")
                                       :name name))))
  (setf *cache-meta-file* (conf "meta"))
  (setf *cache-contents-file* (conf "contents"))))

(defvar *cache-meta* nil
  "Cache meta information, a plist of the cache format version and the
date this cache was built.")

(defvar *cache-contents* nil
  "Table of cached packages. The key is the repo name, the value
contains a list of sublists (PKGNAME VERSION DESC).")

(defun load-cache-meta ()
  (when (probe-file *cache-meta-file*)
    (setf *cache-meta* (cl-store:restore *cache-meta-file*))))

(defun load-cache-contents ()
  (when (probe-file *cache-contents-file*)
    (setf *cache-contents* (cl-store:restore *cache-contents-file*))))

(defun update-cache (&key memory-only-p)
  (handler-case
    (progn
      ;; first set up our in-memory cache
      (setf *cache-contents* (make-hash-table :test #'equalp))
      (dolist (db-spec (cons *local-db* *sync-dbs*))
        (let ((db-name (car db-spec)))
          (with-progress-info ("Building cache for db ~S" db-name)
            (map-db-packages (lambda (db-spec pkg)
                               (declare (ignore db-spec))
                               (push (list (alpm-pkg-get-name pkg)
                                           (alpm-pkg-get-version pkg)
                                           (alpm-pkg-get-desc pkg))
                                     (gethash db-name *cache-contents*)))
                             :db-list (list db-spec)))))
      (setf *cache-meta* (list :last-update (get-universal-time)
                               :version *cache-format-version*))

      ;; now write it to disk
      (unless memory-only-p
        (with-progress-info ("Writing cache meta data")
          (cl-store:store *cache-meta* *cache-meta-file*))
        (with-progress-info ("Writing cache contents")
          (cl-store:store *cache-contents* *cache-contents-file*))))
  ((or error cl-store-error) (c)
   (setf *cache-meta* nil)
   (signal c))))

(defun get-alpm-last-update-time ()
  "Get the date of the last ALPM db update, in universal time."
  #+sbcl
    (+ (sb-posix:stat-mtime
         (sb-posix:stat "/var/lib/pacman"))
       sb-impl::unix-to-universal-time)
  #-sbcl(error "no get-alpm-last-update-time"))

(defun init-cache ()
  (init-cache-paths)
  (load-cache-meta)
  (cond
    ((null *cache-meta*)
     (info "Cache not found, building it.")
     (update-cache)
     (load-cache-meta))
    ((> (get-alpm-last-update-time) (getf *cache-meta* :last-update -1))
     (info "Cache is out of date, refreshing.")
     (update-cache)
     (load-cache-meta))
    ((not (eql *cache-format-version* (getf *cache-meta* :version -1)))
     (info "Cache is not compatible, rebuilding.")
     (update-cache)
     (load-cache-meta)))
  (load-cache-contents))

(defun map-cached-packages (fn &key (db-list *sync-dbs*))
  (init-cache)
  (dolist (db-spec db-list)
    (let ((db-name (car db-spec)))
      (dolist (pkg-spec (gethash db-name *cache-contents*))
        (funcall fn (car db-spec) pkg-spec)))))
  
