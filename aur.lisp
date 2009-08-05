
(in-package :pacman)

(defparameter *pkgbuild-helper* "/home/sky/pacman/pkgbuild-helper.sh")

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

(defun get-pkgbuild-dependencies (pkgbuild-filename)
  (multiple-value-bind (return-value output-stream)
      (run-program *pkgbuild-helper* '(name) :capture-output-p t)
    (unless (zerop return-value)
      (error "Couldn't get dependencies from PKGBUILD :("))
    ;; TODO: now read output
    t))

(defun aur-tarball-uri (pkg-name)
  ;; TODO
  (format nil "/~A.tgz" pkg-name))

(defun aur-tarball-name (pkg-name)
  ;; TODO
  (format nil "/~A.tgz" pkg-name))

(defun install-aur-package (pkg-name)
  (format t "Installing package ~S from AUR." pkg-name)
  (let ((orig-dir (getcwd)))
    (unwind-protect
      (progn
        ;; download
        (download-file (aur-tarball-uri pkg-name))
        
        ;; unpack 
        (unpack-file (aur-tarball-name pkg-name))

        (chdir pkg-name) ; FIXME correct?

        ;; get dependencies, display, install
        (multiple-value-bind (deps make-deps) (get-pkgbuild-dependencies "PKGBUILD")
          (format t "~%deps: ~S~%makedeps: ~S~%" deps make-deps))

        ;; ask user whether he wishes to edit the PKGBUILD
        (when (y-or-n-p "Edit PKGBUILD")
          (launch-editor "PKGBUILD"))
        ;; TODO
        )
      (chdir orig-dir))))

