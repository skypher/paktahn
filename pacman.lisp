
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

(defun quit ()
  #+sbcl(sb-ext:quit))

#+sbcl
(sb-sys:enable-interrupt sb-unix:sigint 
  (lambda (&rest args) (declare (ignore args))
    (sb-ext:quit)))

;;;; AUR interface

;;; setup simplified json->lisp translation
(defun simplified-camel-case-to-lisp (camel-string)
  "We don't want + and * all over the place."
  (declare (string camel-string))
  (let ((*print-pretty* nil))
    (with-output-to-string (result)
      (loop for c across camel-string
            with last-was-lowercase
            when (and last-was-lowercase
                      (upper-case-p c))
              do (princ "-" result)
            if (lower-case-p c)
              do (setf last-was-lowercase t)
            else
              do (setf last-was-lowercase nil)
            do (princ (char-upcase c) result)))))

(setf json:*json-identifier-name-to-lisp* #'simplified-camel-case-to-lisp)


;;; tell Drakma to handle JSON as strings
(pushnew '("application" . "json") drakma:*text-content-types*
         :test (lambda (x y)
                 (and (equalp (car x) (car y))
                      (equalp (cdr x) (cdr y)))))


(defun map-aur-packages (fn query)
  "Search AUR for a string"
  (let ((json:*json-symbols-package* nil))
    (json:with-decoder-simple-clos-semantics
      (let ((json (drakma:http-request "http://aur.archlinux.org/rpc.php"
                                       :parameters `(("type" . "search")
                                                     ("arg" . ,query)))))
        (check-type json string)
        (let* ((result (json:decode-json-from-string json))
               (matches (slot-value result 'results)))
          (dolist (match (coerce matches 'list))
            (funcall fn match)))))))



;;;; libalpm (Pacman) interface

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
           (return-value (sb-ext:run-program (getenv "EDITOR") filename
                                            :search t :wait t)))
      (unless (zerop return-value)
        (warn "Editor ~S exited with non-zero status ~D" editor return-value)
        (when (y-or-n-p "Choose another editor")
          (go again))))))

(defun install-binary-package (db-name pkg-name)
  "Use Pacman to install a package."
  (format t "Installing binary package ~S from repository ~S." pkg-name db-name)
  (let ((return-value (sb-ext:run-program "pacman" (list "-S" (format nil "~A/~A" db-name pkg-name))
                                          :search t :wait t)))
    (unless (zerop return-value)
      (warn "Pacman exited with non-zero status ~D" return-value))))

(defun install-aur-package (pkg-name)
  (format t "Installing package ~S from AUR." pkg-name)
  ;; download, extract
  ;; TODO

  ;; get dependencies, display, install

  ;; ask user whether he wishes to edit the PKGBUILD
  (when (y-or-n-p "Edit PKGBUILD")
    (launch-editor "PKGBUILD"))
  ;; TODO
  )

(defun install-package (db-name pkg-name)
  (cond
    ((equalp db-name "local")
     (error "BUG: trying to install a package from local?"))
    ((equalp db-name "aur")
     (install-aur-package pkg-name))
    ((member db-name (mapcar #'car *sync-dbs*))
     (install-binary-package db-name pkg-name))))

