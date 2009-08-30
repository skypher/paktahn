
#+sbcl(require :sb-posix)
(require :trivial-backtrace)
(require :cl-store)
(require :cl-json)
(require :drakma)
(require :cffi)
(require :alexandria)
(require :metatilities)
(require :getopt)
(require :split-sequence)
(require :cl-ppcre)
(require :py-configparser)

(defmacro without-package-variance-warnings (&body body)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (handler-bind (#+sbcl(sb-int:package-at-variance #'muffle-warning))
       ,@body)))

(without-package-variance-warnings
    (defpackage paktahn
      (:nicknames :pak)
      (:use :cl :cffi)
      (:import-from :alexandria :compose :curry :rcurry :ensure-list)
      (:import-from :metatilities :push-end)
      (:import-from :split-sequence :split-sequence)))

(in-package :pak)

(declaim (optimize (debug 3) (safety 2) (speed 1) (space 1)))


(load "pyconfig-fix.lisp")
(load "readline.lisp")
(load "term.lisp")
(load "util.lisp")
(load "alpm.lisp")
(load "aur.lisp")
(load "cache.lisp")

(defun package-installed-p (pkg-name &optional pkg-version) ; TODO groups
  (map-cached-packages (lambda (db-name pkg)
                         (declare (ignore db-name))
                         (unless (stringp pkg) ; ignore groups
                             (destructuring-bind (name version desc) pkg
                               (declare (ignore desc))
                               (when (and (equalp name pkg-name)
                                          (or (null pkg-version)
                                              (equalp pkg-version version)))
                                 (return-from package-installed-p t)))))
                       :db-list (list *local-db*))
  nil)

(defun print-group (id db-name name &key (stream *standard-output*))
  (let ((*standard-output* stream))
    ;; id
    (with-term-colors (:fg 'yellow :invertp t)
      (format t "~D" id))
    (format t " ")
    ;; db
    (with-term-colors (:fg (or (cdr (assoc db-name *db-colors*
                                           :test #'string-equal))
                               'magenta))
      (format t "~A/" db-name))
    ;; name
    (with-term-colors (:fg 'white)
      (format t "~A" name))
    (format t " ")
    (with-term-colors (:fg 'green)
      (format t "[group]~%")))) ; TODO: show group members

(defun print-package (id db-name name version description &key (stream *standard-output*) out-of-date-p)
  ;; TODO: votes
  (let ((*standard-output* stream))
    ;; id
    (with-term-colors (:fg 'yellow :invertp t)
      (format t "~D" id))
    (format t " ")
    ;; db
    (with-term-colors (:fg (or (cdr (assoc db-name *db-colors*
                                           :test #'string-equal))
                               'magenta))
      (format t "~A/" db-name))
    ;; name
    (with-term-colors (:fg 'white)
      (format t "~A" name))
    ;; version
    (format t " ")
    (with-term-colors (:fg 'green)
      (format t "~A" version))
    ;; installation status
    (when (package-installed-p name) ; TODO: version
      (format t " ")
      (with-term-colors (:fg 'green :invertp t)
        (format t "[installed]")))
    ;; out of date? (aur-only)
    (when out-of-date-p
      (format t " ")
      (with-term-colors (:fg 'red)
          (format t "(out of date)")))
    ;; description
    (format t "~%    ~A~%" description)))

(defun get-package-results (query &key (quiet t) exact (stream *standard-output*))
  (declare (string query))
  (let* ((*print-pretty* nil)
         (i 0)
         packages ; (ID REPO NAME)
         (db-pkg-and-grp-fn (lambda (db-name pkg)
                              (etypecase pkg
                                (string
                                  (let ((name pkg))
                                    (when (or (and exact (equalp query name)) ; TODO we can return immediately on an exact result.
                                              (and (not exact)
                                                   (search query name :test #'equalp)))
                                      (incf i)
                                      (push-end (list i 'group name) packages)
                                      (unless quiet
                                        (print-group i db-name name :stream stream)))))
                                (cons
                                  (destructuring-bind (name version desc) pkg
                                    ;; TODO: search in desc, use regex
                                    (when (or (and exact (equalp query name)) ; TODO we can return immediately on an exact result.
                                              (and (not exact)
                                                   (or (search query name :test #'equalp)
                                                       (search query desc :test #'equalp))))
                                      (incf i)
                                      (push-end (list i db-name name) packages)
                                      (unless quiet
                                        (print-package i db-name name version desc :stream stream))))))))
         (aur-pkg-fn (lambda (match)
                       (incf i)
                       (with-slots (id name version description out-of-date) match
                         (push-end (list i "aur" name) packages)
                         (unless quiet
                           (print-package i "aur" name version description
                                          :stream stream
                                          :out-of-date-p (equal out-of-date "1")))))))
    (map-cached-packages db-pkg-and-grp-fn)
    (map-aur-packages aur-pkg-fn query)
    packages))


;;; user interface
#+(or) ; not used right now
(defun valid-range-input-p (s)
  (loop for c across s
        unless (or (digit-char-p c)
                   (member c '(#\, #\Space #\-)))
          return nil
        finally (return t)))

(defun parse-ranges (string start max)
  "Parse comma delimited numeric ranges, returning a list of min/max
pairs as cons cells."
  ;; This function isn't very good. Curiously, using parse-integer
  ;; actually made it harder, I suspect.
  (when (or (>= start (length string))
            (char= #\- (aref string start)))
    (return-from parse-ranges nil))  
  (labels ((clamp (x) (max 0 (min x max)))
           (range (x y) (cons (clamp (min x y)) (clamp (max x y)))))
    (multiple-value-bind (min idx)
        (parse-integer string :junk-allowed t :start start)      
      (cond
        ((null min) nil)
        ((= idx (length string)) (list (range min min)))
        ((or (char= #\, (aref string idx))
             (char= #\  (aref string idx)))
         (list* (range min min)
                (parse-ranges string (1+ idx) max)))
      ((char= #\- (aref string idx))
       (multiple-value-bind (parsed-max idx)
           (parse-integer string :junk-allowed t :start (1+ idx))
         (list* (range min (or parsed-max max))
                (parse-ranges string (1+ idx) max))))
      (t (format t "???~%") nil)))))

(defun expand-ranges (ranges)
  (loop for (min . max) in ranges
        nconcing (loop for i from min upto max collect i)))

(defun extract-ranges (vector rangespec-string)
  (map 'vector (lambda (i) (aref vector i))
       (expand-ranges (parse-ranges rangespec-string 0 (1- (length vector))))))

(defun find-package-by-name (pkg-name)
  ;; TODO multiple hit handling
  (cdr (first (get-package-results pkg-name :exact t))))

(defun install-package (pkg-name &key (db-name
                                        (first (find-package-by-name pkg-name)))
                                      force)
  (labels ((do-install ()
             (cond
               ((and (package-installed-p pkg-name) (not force))
                (info "Package ~S is already installed, skipping." pkg-name))
               ((not db-name)
                (restart-case
                    (error "Couldn't find package ~S anywhere" pkg-name)
                  (resync-db ()
                    :report "Resync pacman databases (-Sy) and try again"
                    (run-pacman '("-Sy"))
                    (do-install))))
               ((equalp db-name "local")
                (error "BUG: trying to install a package from local"))
               ((equalp db-name "aur")
                (install-aur-package pkg-name))
               ((or (eq db-name 'group)
                    (member db-name (mapcar #'car *sync-dbs*) :test #'equalp))
                (install-binary-package db-name pkg-name)))))
    ;; either we're installing a root package and need to set up our
    ;; environment to reflect this, or we're installing a dep and
    ;; should check that the environment has been set up properly.
    (declare (special *root-package*))
    (cond
      ;; installing a dep
      ((boundp '*root-package*)
       (assert *root-package*)
       (check-type *root-package* string)
       (tagbody retry
         (restart-case
             (do-install)
           (retry ()
             :report (lambda (s)
                       (format s "Retry installation of package ~S" pkg-name))
             (go retry)))))
      ;; installing an explicitly requested package
      (t
       (let ((*root-package* pkg-name))
         (declare (special *root-package*))
         (restart-case
             (do-install)
           (skip-package ()
             :report (lambda (s)
                       (format s "Skip package ~S and continue" *root-package*))
             (values nil 'skipped))))))))

(defun search-and-install-packages (query)
  (let* ((pkglist (make-string-output-stream))
         (bstream (make-broadcast-stream *standard-output* pkglist))
         (packages (get-package-results query :quiet nil
                                              :stream bstream))
         (pkglist (get-output-stream-string pkglist))
         (total (length packages)))
    (if (null packages)
      (info "Sorry, no packages matched ~S~%" query)
      (flet ((print-list ()
               (format *standard-output* "~A" pkglist)) ; TODO: use write
             (make-prompt ()
               (with-term-colors (:fg 'white)
                 (format nil "[1-~D] => " total))))
        (format t "=>  -----------------------------------------------------------~%~
                   =>  Enter numbers (e.g. '1,2-5,6') of packages to be installed.~%~
                   =>  Empty line prints the package list again. Hit Ctrl+C to abort.~%~
                   =>  -----------------------------------------------------------~%")
        (let* ((choices (loop for input = (getline (make-prompt))
                              for got-input-p = (and input (not (equal input "")))
                              for ranges = (when got-input-p
                                             (expand-ranges (parse-ranges input 0 total)))
                              until ranges
                              unless got-input-p
                                do (print-list)
                              finally (progn
                                        (add-history input)
                                        (return ranges))))
               (chosen-packages (remove-if-not (lambda (id)
                                                 (member id choices))
                                               packages :key #'first))
               (chosen-packages (sort chosen-packages #'< :key #'first)))
          ;(format t "chosen: ~S~%" chosen-packages)
          (mapcar (lambda (pkgspec)
                    (funcall #'install-package (third pkgspec)
                             :db-name (second pkgspec)))
                  chosen-packages))))))

(defun parse-options (argv)
  ;; TODO
  (getopt:getopt argv nil))

(defun display-help ()
  (format t "Usage: paktahn PACKAGE~%"))

(defun main (argv &aux (argc (length argv)))
  "Secondary entry point: process config and command line."
  (load-config)
  (cond
    ((some (lambda (x) (member x '("-h" "--help") :test #'equalp)) argv)
     (display-help))
    ((eql argc 1) 
     (search-and-install-packages (first argv)))
    ((and (>= argc 2) (equal (first argv) "-S"))
     (mapcar #'install-package (cdr argv)))
    (t
     (display-help))))

(defun core-main ()
  "Primary entry point for the binary."
  (setf *on-error* :quit)
  (handler-bind ((error #'default-error-handler))
    (init-alpm)
    (setf *local-db* (init-local-db))
    (setf *sync-dbs* (init-sync-dbs))
    (setf *print-pretty* nil)
    (enable-quit-on-sigint)
    (let ((argv (cdr (getargv))))
      (restart-case
          (main argv)
        (quit ()
          :report "Quit Paktahn"
          (quit))))))

(defun build-core (&key forkp)
  #-sbcl(error "don't know how to build a core image")
  #+sbcl(progn
          (flet ((dump ()
                       (sb-ext:save-lisp-and-die "paktahn"
                                                 :toplevel #'core-main
                                                 :executable t
                                                 :save-runtime-options nil)))
            (if forkp
              (let ((pid (sb-posix:fork)))
                (if (zerop pid)
                  (dump)
                  (progn
                    (format t "INFO: waiting for child to finish building the core...~%")
                    (sb-posix:waitpid pid 0)
                    (format t "INFO: ...done~%"))))
              (dump)))))

