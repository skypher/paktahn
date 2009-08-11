
#+sbcl(require :sb-posix)
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


(load "readline.lisp")
(load "util.lisp")
(load "alpm.lisp")
(load "aur.lisp")

(defvar *on-error* :debug)

(defparameter *ansi-colors* '((black . 0) (red . 1) (green . 2) (yellow . 3)
                              (blue . 4) (magenta . 5) (cyan . 6) (white . 7)))

(defparameter *db-colors* '((core . green) (extra . green) (unstable . red)))


(defun term-set (modes)
  (format t "~C[~Dm" #\Escape modes))

(defun term-set-fg-color (colorname &key (boldp t))
  (let ((colid (cdr (assoc colorname *ansi-colors*))))
    (assert colid)
    (term-set (format nil "~D;3~D" (if boldp 1 0) colid))))

(defun term-set-bg-color (colorname &key (boldp t))
  (let ((colid (cdr (assoc colorname *ansi-colors*))))
    (assert colid)
    (term-set (format nil "~D;4~D" (if boldp 1 0) colid))))

(defun term-reset-colors ()
  (term-set 0))

(defun term-invert ()
  (term-set 7))

(defmacro with-term-colors ((&key fg bg (boldp t) invertp) &body body)
  `(progn
     ,(when invertp
        `(term-invert))
     ,(when fg
        `(term-set-fg-color ,fg :boldp ,boldp))
     ,(when bg
        `(term-set-bg-color ,bg :boldp ,boldp))
     (unwind-protect (progn ,@body)
       (term-reset-colors))))


(defun print-package (id db-name name version description)
  ;; TODO: out of date indicator, votes
  (with-term-colors (:fg 'yellow :invertp t)
    (format t "~D" id))
  (format t " ")
  (with-term-colors (:fg (or (cdr (assoc db-name *db-colors*
                                         :test #'string-equal))
                             'magenta))
    (format t "~A/" db-name))
  (with-term-colors (:fg 'white)
    (format t "~A" name))
  (format t " ")
  (with-term-colors (:fg 'green)
    (format t "~A" version))
  (when (package-installed-p name)
    (format t " ")
    (with-term-colors (:fg 'yellow :invertp t)
      (format t "[installed]")))
  (format t "~%    ~A~%" description))

(defun get-package-results (query &key (quiet t) exact)
  (declare (string query))
  (let* ((i 0)
         packages ; (ID REPO NAME)
         (db-pkg-fn (lambda (db-spec pkg)
                      (let* ((name (alpm-pkg-get-name pkg))
                             (version (alpm-pkg-get-version pkg))
                             (desc (alpm-pkg-get-desc pkg)))
                        ;; TODO: search in desc, use regex
                        (when (or (and exact (equalp query name)) ; TODO we can return immediately on an exact result.
                                  (and (not exact)
                                       (or (search query name :test #'equalp)
                                           (search query desc :test #'equalp))))
                          (incf i)
                          (push-end (list i (car db-spec) name) packages)
                          (unless quiet
                            (print-package i (car db-spec) name version desc))))))
         (aur-pkg-fn (lambda (match)
                       (incf i)
                       (with-slots (id name version description) match
                         (push-end (list i "aur" name) packages)
                         (unless quiet
                           (print-package i "aur" name version description))))))
    (map-db-packages db-pkg-fn)
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

(defun install-package (pkg-name &key (db-name (first (find-package-by-name pkg-name))))
  (cond
    ((not db-name)
     (error "Couldn't find package ~S anywhere" pkg-name))
    ((equalp db-name "local")
     (error "BUG: trying to install a package from local"))
    ((equalp db-name "aur")
     (install-aur-package pkg-name))
    ((member db-name (mapcar #'car *sync-dbs*))
     (install-binary-package db-name pkg-name))))

(defun search-and-install-packages (query)
  (let* ((packages (get-package-results query :quiet nil))
         (total (length packages)))
    (if (null packages)
      (format t "INFO: Sorry, no packages matched ~S~%" query)
      (flet ((make-prompt ()
               (format nil "[1-~D] => " total)))
        (format t "=>  -----------------------------------------------------------~%~
                   =>  Enter numbers (e.g. '1,2-5,6') of packages to be installed.~%~
                   =>  -----------------------------------------------------------~%")
        (let* ((choices (loop for input = (getline (make-prompt))
                              for ranges = (expand-ranges (parse-ranges input 0 total))
                              until ranges
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
  (cond
    ((some (lambda (x) (member x '("-h" "--help") :test #'equalp)) argv)
     (display-help))
    ((eql argc 1) 
     (search-and-install-packages (first argv)))
    ((and (eql argc 2) (equal (first argv) "-S"))
     (install-package (second argv)))
    (t
     (display-help))))

(defun core-main ()
  (setf *on-error* :backtrace) ; TODO
  (handler-bind ((error (lambda (c)
                          (case *on-error*
                            (debug (invoke-debugger c))
                            (t (format t "Fatal error: ~A~%" c)
                               (quit))))))
    (init-alpm)
    (setf *local-db* (init-local-db))
    (setf *sync-dbs* (init-sync-dbs))
    (setf *print-pretty* nil)
    (enable-quit-on-sigint)
    (let ((argv (cdr (getargv))))
      (main argv))))

(defun build-core (&key forkp)
  #-sbcl(error "don't know how to build a core image")
  #+sbcl(flet ((dump ()
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
            (dump))))

