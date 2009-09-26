
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
            (dolist (match (sort (coerce results 'list) #'string<
                                 :key (lambda (result)
                                        (slot-value result 'name))))
              (funcall fn match))
            (note "AUR message: ~A" results)))))))

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
  (info "Installing package ~S from AUR.~%" pkg-name)
  (when (rootp)
    (error "You're running Paktahn as root; makepkg will not work.~%~
            Try running as a normal user and Paktahn will invoke `sudo' as necessary."))
  (let ((orig-dir (current-directory)))
    (unwind-protect
      (progn
        ;; enter temporary directory
        (setf (current-directory) (tempdir))

        ;; download
        (download-file (aur-tarball-uri pkg-name))
        
        ;; unpack 
        (unpack-file (aur-tarball-name pkg-name))

        (setf (current-directory) pkg-name)

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
      (setf (current-directory) "..")
      (let ((pkgdir (merge-pathnames
                      (make-pathname :directory `(:relative ,pkg-name))
                      (current-directory)))
            (tarball (merge-pathnames
                       (make-pathname :name (aur-tarball-name pkg-name))
                       (current-directory))))
        (when (probe-file pkgdir)
          (delete-directory-and-files pkgdir))
        (when (probe-file tarball)
          (delete-file tarball)))
      (setf (current-directory) orig-dir))
    t))

