(in-package :pak)

(setf cl-store:*check-for-circs* nil)

(defparameter *cache-format-version* 3)

(flet ((conf (type name)
         (config-file (make-pathname :directory '(:relative "cache")
                                     :name (format nil "~(~A.~A~)" name type)))))
  (defun cache-meta-file (db-name)
    (conf 'meta db-name))
  (defun cache-contents-file (db-name)
    (conf 'contents db-name))
  (defun cache-lock-file (db-name)
    (conf 'lock db-name)))

(defun pid-active-p (pid)
  (probe-file (format nil "/proc/~D/" pid)))

(defun grab-cache-lock (db-name)
  (let ((lock-file (cache-lock-file db-name))
        feedback-given-p)
    (retrying
      (when (probe-file lock-file)
        (with-open-file (lf lock-file :direction :input
                                      :if-does-not-exist :error)
          (let ((pid (read lf)))
            (cond
              ((pid-active-p pid)
               (unless feedback-given-p
                 (setf feedback-given-p t)
                 (info "Another Paktahn instance (pid ~D) is currently accessing the ~
                       disk cache for ~S, please wait..." pid db-name))
               (sleep 0.5)
               (retry))
              (t
               (note "Cache lock ~S grabbed by pid ~D is stale, removing it."
                     lock-file pid)
               (delete-file lock-file)))))))
    (with-open-file (lf lock-file :direction :output
                                  :if-exists :error
                                  :if-does-not-exist :create)
      (print (getpid) lf))))

(defun release-cache-lock (db-name)
  (when (probe-file (cache-lock-file db-name))
    (delete-file (cache-lock-file db-name))))

(defmacro with-cache-lock (db-name &body body)
  `(locally
     (declare (special *cache-lock-grabbed*))
     (if (boundp '*cache-lock-grabbed*)
       (progn
         ,@body)
       (unwind-protect
         (let ((*cache-lock-grabbed* t))
           (declare (special *cache-lock-grabbed*))
           (grab-cache-lock ,db-name)
           ,@body)
         (release-cache-lock ,db-name)))))

(defvar *cache-meta* nil
  "Cache meta information hash table. The key is the repo name,
the value is a plist of the cache format version and the date
this cache was built. Initially NIL.")

(defvar *cache-contents* nil
  "Table of cached packages. The key is the repo name, the value
contains a list of sublists (PKGNAME VERSION DESC). Initially NIL.")

(defvar *cache-loaded-p* nil)

(defun reset-cache ()
  "Re-register all databases, flush the memory cache and refresh
the memory and disk caches as needed."
  (alpm-db-unregister-sync-dbs)
  (init-dbs)
  (init-cache-vars t)
  (maybe-refresh-cache))

(defun init-cache-vars (&optional force)
  (values
    (when (or force (null (hash-table-p *cache-meta*)))
      (setf *cache-meta* (make-hash-table :test #'equalp)))
    (when (or force (null (hash-table-p *cache-contents*)))
      (setf *cache-contents* (make-hash-table :test #'equalp)))))

(defun load-cache-meta (db-name)
  (setf (gethash db-name *cache-meta*)
        (cl-store:restore (cache-meta-file db-name))))

(defun load-cache-contents (db-name)
  (setf (gethash db-name *cache-contents*)
        (cl-store:restore (cache-contents-file db-name))))

(defun load-memory-cache-from-disk (db-name)
  (setf (gethash db-name *cache-meta*) nil)
  (setf (gethash db-name *cache-contents*) nil)
  (load-cache-meta db-name)
  (load-cache-contents db-name))

(defun build-memory-cache (db-name)
  "Update/build *cache-meta* and *cache-contents*."
  (let ((db-spec (db-name->db-spec db-name)))
    (setf (gethash db-name *cache-meta*) nil)
    (setf (gethash db-name *cache-contents*) nil)
    (handler-case
      (progn
        ;; packages
        (map-db-packages (lambda (db-spec pkg)
                           (declare (ignore db-spec))
                           ;(format t "~A ~A~%" (alpm-pkg-get-name pkg) (alpm-pkg-get-version pkg))
                           (push (list (alpm-pkg-get-name pkg)
                                       (alpm-pkg-get-version pkg)
                                       (alpm-pkg-get-desc pkg)
                                       (mapcar #'safe-foreign-string-to-lisp (alpm-list->lisp (alpm-pkg-get-provides pkg))))
                                 (gethash db-name *cache-contents*)))
                         :db-list (list db-spec))

        ;; groups
        (map-groups (lambda (db-spec grp)
                      (declare (ignore db-spec))
                      (push (alpm-grp-get-name grp) (gethash db-name *cache-contents*)))
                    :db-list (list db-spec))

        ;; update time
        (setf (gethash db-name *cache-meta*)
              (list :last-update (get-universal-time)
                    :version *cache-format-version*)))
      (error (c)
        (setf (gethash db-name *cache-meta*) nil
              (gethash db-name *cache-contents*) nil)
        (signal c)))))

(defun sync-disk-cache (db-name)
  "Write the in-memory cache (which must exist at this point in time)
for a db to disk."
  (assert (and *cache-meta* *cache-contents*))
  (check-type *cache-meta* hash-table)
  (check-type *cache-contents* hash-table)
  (with-cache-lock db-name
    (handler-case
      (progn
        (cl-store:store (gethash db-name *cache-meta*)
                        (cache-meta-file db-name))
        (cl-store:store (gethash db-name *cache-contents*)
                        (cache-contents-file db-name)))
      ((or error cl-store-error) (c)
       (dolist (file (list (cache-meta-file db-name) (cache-contents-file db-name)))
         (when (probe-file file)
         (delete-file file)))
       (signal c)))))

(defun alpm-db-folder (db-name)
  (if (member db-name '("local" "local.db") :test #'string=)
    "/var/lib/pacman/local"
    (format nil "/var/lib/pacman/sync/~(~A~)" db-name)))

(defun get-alpm-last-update-time (db-name)
  "Get the date of the last ALPM db update, in universal time."
  (if (>= (car *alpm-version*) 6)
      (file-write-date (alpm-db-folder (concatenate 'string db-name ".db")))
      (file-write-date (alpm-db-folder db-name))))

(defun maybe-refresh-cache ()
  ;; TODO: this will produce incorrect results when called for the
  ;; second time. detect this case and call reset-cache.
  (dolist (db-spec (cons *local-db* *sync-dbs*))
    (let ((db-name (car db-spec)))
      (with-cache-lock db-name
        ;; first ensure that the disk cache is synced to dbs
        (maybe-update-disk-cache db-name)
        ;; now we can be sure to have a valid disk cache. Use it
        ;; to update the memory cache unless it is already synced
        ;; to the disk cache.
        (when (or (null *cache-meta*)
                  (null (nth-value 1 (gethash db-name *cache-meta*))))
          (init-cache-vars)
          (load-memory-cache-from-disk db-name))))))

(defun ensure-initial-cache ()
  (unless *cache-loaded-p*
    (maybe-refresh-cache)
    (setf *cache-loaded-p* t)))

(defun rebuild-disk-cache (db-name)
  "Update the in-memory cache and write it to disk."
  (init-cache-vars)
  (build-memory-cache db-name)
  (sync-disk-cache db-name))

(defun maybe-update-disk-cache (db-name)
  "Check if the disk cache is stale and update it if necessary."
  (let* ((meta-present-p (probe-file (cache-meta-file db-name)))
         (content-present-p (probe-file (cache-contents-file db-name)))
         (needs-update-p
           (or (when (or (not meta-present-p) (not content-present-p))
                 (note "Cache ~S not found, building it." db-name)
                 t)
               ;; TODO check file mod times to avoid restoring the metadata
               ;;      every time.
               ;; TODO: don't update when Pacman is running
               (let* ((meta (cl-store:restore (cache-meta-file db-name)))
                      (meta-version (getf meta :version -1))
                      (last-update (getf meta :last-update -1)))
                 (cond
                   ;; stale
                   ((> (get-alpm-last-update-time db-name)
                       last-update)
                    (note "Cache ~S is out of date, refreshing." db-name)
                    t)
                   ;; old format denoted by version
                   ((not (eql meta-version *cache-format-version*))
                    (note "Cache ~S is not compatible, rebuilding." db-name)
                    t))))))
    (when needs-update-p
      (rebuild-disk-cache db-name)
      t)))

;;;; accessing the cache
(defun map-cached-packages (fn &key (db-list *sync-dbs*) (include-groups t))
  (dolist (db-spec db-list)
    (let ((db-name (car db-spec)))
      (dolist (pkg-spec (gethash db-name *cache-contents*))
        (when (or include-groups (consp pkg-spec))
          (funcall fn (car db-spec) pkg-spec))))))

(defun update-local-cache (pkg-name version)
  (flet ((name-or-nil (pkg)
           (etypecase pkg
             (string '())
             (list (car pkg)))))
    (with-cache-lock "local"
      (let ((metadata (find pkg-name (gethash "local" *cache-contents*)
                            :key #'name-or-nil :test #'equal)))
        (if metadata
            (setf (second metadata) version)
            ;; TODO: Keep it alphabetized?
            ;; TODO: Figure out a better way than find-package-by-name + find.
            (let* ((pkg (find-package-by-name pkg-name)))
              (if (string= (car pkg) "aur")
                  (push-end (cdr pkg) (gethash "local" *cache-contents*))
                  (push-end (find pkg-name (gethash (car pkg) *cache-contents*)
                                  :key #'name-or-nil :test #'equal)
                            (gethash "local" *cache-contents*)))))))))
