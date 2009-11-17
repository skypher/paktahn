
(in-package :pak)

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
      (let* (socket-error-p
             (json
               (handler-bind ((usocket:socket-error (lambda (e)
                                                      (setf socket-error-p t)
                                                      (error "Error connecting to AUR: ~A" e))))
                 (retrying
                   (restart-case
                       (drakma:http-request "http://aur.archlinux.org/rpc.php"
                                            :parameters `(("type" . "search")
                                                          ("arg" . ,query)))
                     (retry ()
                       :test (lambda (c) (declare (ignore c)) socket-error-p)
                       :report (lambda (s) (format s "Retry network connection."))
                       (setf socket-error-p nil)
                       (retry))
                     (ignore ()
                       :test (lambda (c) (declare (ignore c)) socket-error-p)
                       :report (lambda (s) (format s "Ignore this error and continue, skipping packages from AUR."))
                       (return-from map-aur-packages nil)))))))
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
  "Run makepkg in the current working directory"
  (retrying
    (restart-case
        (check-pkgbuild-arch)
      (add-arch ()
          :report (lambda (s)
                    (format s "Add ~S to the PKGBUILD's arch field" (get-carch)))
        (add-carch-to-pkgbuild)
        (retry))))
  (let ((return-value (run-program "makepkg" nil)))
    (unless (zerop return-value)
      ;; TODO restarts?
      (error "Makepkg failed (status ~D)" return-value))
    t))

(defun prompt-user-review (filename) ;; ask user whether they wish to edit a file
  (let ((str (concatenate 'string "Review/edit " filename)))
    (when (ask-y/n str t)
      (launch-editor filename))))

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

	;; update the checksum database in case another process has added new checksums
	(load-checksums)

	;; check to see if the PKGBUILD has been seen before
	(compare-checksums pkg-name)
	
        ;; store the modified checksums DB
	(save-checksums)

	;; if a customization exists for the pkg, apply it
	(when (customize-p pkg-name)
	  (apply-customizations))

        (unless (ask-y/n (format nil "Continue building ~S" pkg-name) t)
          (return-from install-aur-package))

        ;; get dependencies, display, install
        (multiple-value-bind (deps make-deps) (get-pkgbuild-dependencies)
          ;(format t "~%deps: ~S~%makedeps: ~S~%" deps make-deps)
          (install-dependencies (append deps make-deps)))

        (run-makepkg)
	(install-pkg-tarball))

      ;; clean up
      (clean-up-temp-files pkg-name orig-dir))
    t))

(defun install-pkg-tarball ()
  (let ((pkg-location (get-pkgdest))
	(pkg-tarball (get-pkgbuild-tarball-name))
	force)
    (if (string= pkg-location "")
	(setf pkg-location pkg-tarball)
	(setf pkg-location (concatenate 'string (ensure-trailing-slash pkg-location) pkg-tarball)))
    (retrying
     (restart-case
	 (let ((exit-code (if force
			      (run-pacman (list "-Uf" pkg-location))
			      (run-pacman (list "-U" pkg-location)))))
	   (unless (zerop exit-code)
	     (error "Failed to install package (error ~D)" exit-code)))
       (retry ()
	 :report "Retry installation"
	 (retry))
       (force-install ()
	 :report "Force installation (-Uf)"
	 (setf force t)
	 (retry))
       (save-package ()
	 :report (lambda (s) (format s "Save the package to ~A~A" (config-file "packages/") pkg-tarball))
	 (run-program "mv" (list pkg-tarball (format nil "~A~A" (config-file "packages/") pkg-tarball))))))))

(defun clean-up-temp-files (pkg-name &optional orig-dir)
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
  (when orig-dir
    (setf (current-directory) orig-dir)))