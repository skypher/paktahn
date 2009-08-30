
(in-package :pak)


(setf cl-store:*check-for-circs* nil)

(defparameter *cache-format-version* 1)

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
    (tagbody retry
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
               (go retry))
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
this cache was built.")

(defvar *cache-contents* nil
  "Table of cached packages. The key is the repo name, the value
contains a list of sublists (PKGNAME VERSION DESC).")

(defun load-cache-meta (db-name)
  (unless *cache-meta*
    (setf *cache-meta* (make-hash-table :test #'equalp)))
  (when (probe-file (cache-meta-file db-name))
    (setf (gethash db-name *cache-meta*)
          (cl-store:restore (cache-meta-file db-name)))))

(defun load-cache-contents (db-name)
  (unless *cache-contents*
    (setf *cache-contents* (make-hash-table :test #'equalp)))
  (when (probe-file (cache-contents-file db-name))
    (setf (gethash db-name *cache-contents*)
          (cl-store:restore (cache-contents-file db-name)))))

(defun update-cache (db-spec)
  "Update/build *cache-meta* and *cache-contents*."
  (unless *cache-meta*
    (setf *cache-meta* (make-hash-table :test #'equalp)))
  (unless *cache-contents*
    (setf *cache-contents* (make-hash-table :test #'equalp)))
  (let ((db-name (car db-spec)))
    (setf (gethash db-name *cache-meta*) nil)
    (setf (gethash db-name *cache-contents*) nil)
    (handler-case
      (progn
        ;; packages
        (map-db-packages (lambda (db-spec pkg)
                           (declare (ignore db-spec))
                           (push (list (alpm-pkg-get-name pkg)
                                       (alpm-pkg-get-version pkg)
                                       (alpm-pkg-get-desc pkg))
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
  "Write the in-memory cache for a db to disk."
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
  (if (equalp db-name "local")
    "/var/lib/pacman/local"
    (format nil "/var/lib/pacman/sync/~(~A~)" db-name)))

(defun get-alpm-last-update-time (db-name)
  "Get the date of the last ALPM db update, in universal time."
  #+sbcl
  (flet ((mod-time/ut (file)
           (+ (sb-posix:stat-mtime
                (sb-posix:stat file))
              sb-impl::unix-to-universal-time)))
    (mod-time/ut (alpm-db-folder db-name)))
  #-sbcl(error "no get-alpm-last-update-time"))

(defun init-cache ()
  (dolist (db-spec (cons *local-db* *sync-dbs*))
    (let ((db-name (car db-spec)))
      (with-cache-lock db-name
        ;; load metadata
        (load-cache-meta db-name)
        ;; check meta information
        (let ((needs-update-p
                (cond
                  ;; old meta format
                  ((not (hash-table-p *cache-meta*))
                   (setf *cache-meta* nil)
                   (note "Cache ~S not in proper format, rebuilding it from scratch."
                         db-name)
                   t)
                  ;; no meta info found
                  ((or (null *cache-meta*)
                       (null (gethash db-name *cache-meta*)))
                   (note "Cache ~S not found, building it." db-name)
                   t)
                  ;; stale
                  ((> (get-alpm-last-update-time db-name)
                      (getf (gethash db-name *cache-meta*) :last-update -1))
                   (note "Cache ~S is out of date, refreshing." db-name)
                   t)
                  ;; old format denoted by version
                  ((not (eql *cache-format-version*
                             (getf (gethash db-name *cache-meta*) :version -1)))
                   (note "Cache is not compatible, rebuilding.")
                   t))))
          (when needs-update-p
            (update-cache db-spec)
            (sync-disk-cache db-name)
            (load-cache-meta db-name)))
        ;; load contents
        (load-cache-contents db-name)))))

(defun map-cached-packages (fn &key (db-list *sync-dbs*))
  (init-cache)
  (dolist (db-spec db-list)
    (let ((db-name (car db-spec)))
      (dolist (pkg-spec (gethash db-name *cache-contents*))
        (funcall fn (car db-spec) pkg-spec)))))
  
