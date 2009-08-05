
(require :cl-json)
(require :drakma)
(require :cffi)
(require :alexandria)
(require :py-configparser)

(defmacro without-package-variance-warnings (&body body)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (handler-bind (#+sbcl(sb-int:package-at-variance #'muffle-warning))
       ,@body)))

(without-package-variance-warnings
    (defpackage pacman
      (:use :cl :cffi)
      (:import-from :alexandria :compose :curry :rcurry :ensure-list )))


(in-package :pacman)

(load "util.lisp")
(load "alpm.lisp")
(load "aur.lisp")

(defun print-package (id db-name name version description)
  ;; TODO: out of date indicator, votes
  (format t "~D ~A/~A ~A~%    ~A~%" id db-name name version description))

(defun get-package-results (query &key quiet)
  (declare (string query))
  (let* ((i 0)
         packages ; (ID REPO NAME)
         (db-pkg-fn (lambda (db-spec pkg)
                      (let* ((name (alpm-pkg-get-name pkg))
                             (version (alpm-pkg-get-version pkg))
                             (desc (alpm-pkg-get-desc pkg)))
                        ;; TODO: search in desc, use regex
                        (when (or (search query name :test #'equalp)
                                  (search query desc :test #'equalp))
                          (incf i)
                          (push (list i (car db-spec) name) packages)
                          (unless quiet
                            (print-package i (car db-spec) name version desc))))))
         (aur-pkg-fn (lambda (match)
                       (incf i)
                       (with-slots (id name version description) match
                         (push (list i "aur" name) packages)
                         (unless quiet
                           (print-package i "aur" name version description))))))
    (map-db-packages db-pkg-fn)
    (map-aur-packages aur-pkg-fn query)
    packages))


;;; user interface
(defun getline ()
  (finish-output *standard-output*)
  (or (read-line *standard-input* nil) (quit)))

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

(defun search-and-install-packages (query)
  (let* ((packages (get-package-results query))
         (total (length packages)))
    (flet ((show-prompt ()
             (format t "[1-~D] => " total)))
      (format t "=>  Enter numbers (e.g. '1,2-5,6') of packages to be installed.~%~
                 =>  -----------------------------------------------------------~%")
      (let* ((choices (loop for input = (progn (show-prompt)
                                               (getline))
                            for ranges = (expand-ranges (parse-ranges input 0 total))
                            until ranges
                            finally (return ranges)))
             (chosen-packages (remove-if-not (lambda (id)
                                               (member id choices))
                                             packages :key #'first))
             (chosen-packages (sort chosen-packages #'< :key #'first)))
        chosen-packages))))

(defun ask-for-editor ()
  (format t "You do not have your EDITOR environment variable set.~%
          Please tell me the name of your preferred editor.")
  (flet ((show-prompt () (format t " =>")))
    (loop for input = (progn (show-prompt) (getline))
          until input
          finally (return input))))

(defun launch-editor (filename)
  (tagbody again
    (let* ((editor (or (getenv "EDITOR")
                       (ask-for-editor)))
           (return-value (run-program (getenv "EDITOR") filename)))
      (unless (zerop return-value)
        (warn "Editor ~S exited with non-zero status ~D" editor return-value)
        (when (y-or-n-p "Choose another editor")
          (go again))))))

(defun install-binary-package (db-name pkg-name)
  "Use Pacman to install a package."
  (format t "Installing binary package ~S from repository ~S." pkg-name db-name)
  (let* ((fully-qualified-pkg-name (format nil "~A/~A" db-name pkg-name))
         (return-value (run-program "pacman"
                                   (list "-S" fully-qualified-pkg-name))))
    (unless (zerop return-value)
      (warn "Pacman exited with non-zero status ~D" return-value))))

(defun download-file (uri)
  (tagbody retry
    (let ((return-value (run-program "wget" (list "-c" uri))))
      (unless (zerop return-value)
        (if (y-or-n-p "Download via wget failed with status ~D. Retry?" return-value)
          (go retry)
          (return-from install-aur-package)))
      t)))

(defun unpack-file (name)
  (tagbody retry
    (let ((return-value (run-program "tar" (list "xfvz" name))))
      (unless (zerop return-value)
        (if (y-or-n-p "Unpacking failed with status ~D. Retry?" return-value)
          (go retry)
          (return-from unpack-file)))
      t)))

(defun install-package (db-name pkg-name)
  (cond
    ((equalp db-name "local")
     (error "BUG: trying to install a package from local?"))
    ((equalp db-name "aur")
     (install-aur-package pkg-name))
    ((member db-name (mapcar #'car *sync-dbs*))
     (install-binary-package db-name pkg-name))))

