
(in-package :pak)

(load "pkgbuild.lisp")

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
  (let ((json:*json-symbols-package* #.*package*))
    (json:with-decoder-simple-clos-semantics
      (let ((json (drakma:http-request "http://aur.archlinux.org/rpc.php"
                                       :parameters `(("type" . "search")
                                                     ("arg" . ,query)))))
        (check-type json string)
        (let* ((response (json:decode-json-from-string json))
               (results (slot-value response 'results)))
          (if (equalp (slot-value response 'type) "search")
            (dolist (match (coerce results 'list))
              (funcall fn match))
            (format t "INFO: AUR: ~A~%" results)))))))

(defun install-dependencies (deps)
  (mapcar 'install-package deps))

(defun aur-tarball-uri (pkg-name)
  (format nil "http://aur.archlinux.org/packages/~(~A~)/~(~A~).tar.gz"
          pkg-name pkg-name))

(defun aur-tarball-name (pkg-name)
  (format nil "~(~A~).tar.gz" pkg-name))

(defun run-makepkg ()
  (let ((return-value (run-program "makepkg" nil)))
    (unless (zerop return-value)
      ;; TODO restarts?
      (error "Makepkg failed (status ~D)" return-value))
    t))

(defun install-aur-package (pkg-name)
  (format t "Installing package ~S from AUR.~%" pkg-name)
  (let ((orig-dir (getcwd)))
    (unwind-protect
      (progn
        ;; enter temporary directory
        (chdir (tempdir))

        ;; download
        (download-file (aur-tarball-uri pkg-name))
        
        ;; unpack 
        (unpack-file (aur-tarball-name pkg-name))

        (chdir pkg-name)

        ;; ask user whether he wishes to edit the PKGBUILD
        (when (ask-y/n "Edit PKGBUILD" t)
          (launch-editor "PKGBUILD"))

        (unless (ask-y/n (format nil "Continue building ~S" pkg-name) t)
          (return-from install-aur-package))

        ;; get dependencies, display, install
        (multiple-value-bind (deps make-deps) (get-pkgbuild-dependencies)
          ;(format t "~%deps: ~S~%makedeps: ~S~%" deps make-deps)
          (install-dependencies (append deps make-deps)))

        (run-makepkg)

        (run-pacman (list "-U" (get-pkgbuild-tarball-name))))
      ;; clean up
      (chdir orig-dir)
      (let ((pkg-pathname (make-pathname :directory `(:relative ,pkg-name))))
        (when (probe-file pkg-pathname)
          (osicat:delete-directory-and-files pkg-pathname))))
    t))

