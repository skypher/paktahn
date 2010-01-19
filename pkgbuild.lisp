
(in-package :pak)

(defparameter *pkgbuild-helper* "/usr/lib/paktahn/pkgbuild-helper.sh"
  "Script to extract PKGBUILD values.")
(defparameter *makepkg-helper* "/usr/lib/paktahn/makepkg-helper.sh"
  "Script to extract makepkg.conf values.")

(defun parse-helper-output (stream)
  (loop for line = (read-line stream nil nil)
        with key
        while line
        if key
          collect (cons key line)
          and do (setf key nil)
        else
          do (setf key line)))

(defun get-pkgbuild-data (&optional (pkgbuild-filename "./PKGBUILD"))
  (multiple-value-bind (return-value output-stream)
      (run-program *pkgbuild-helper* (list pkgbuild-filename)
                   :capture-output-p t)
    (unless (zerop return-value)
      (error "Couldn't extract PKGBUILD data (error ~D)" return-value))
    (parse-helper-output output-stream)))

(defun get-makepkg-data ()
  (multiple-value-bind (return-value output-stream)
      (run-program *makepkg-helper* nil :capture-output-p t)
    (unless (zerop return-value)
      (error "Couldn't extract makepkg.conf data (error ~D)" return-value))
    (parse-helper-output output-stream)))

(defun get-makepkg-field (name)
  (let ((data (get-makepkg-data)))
    (flet ((field (name)
	     (cdr (assoc name data :test #'equalp))))
      (field name))))

(defun get-carch ()
  (get-makepkg-field "carch"))

(defun get-pkgdest ()
  (get-makepkg-field "pkgdest"))

(defun get-pkgbuild-arch (&optional (pkgbuild-filename "./PKGBUILD"))
  (let ((data (get-pkgbuild-data pkgbuild-filename)))
    (split-sequence #\Space (cdr (assoc "arch" data :test #'equalp)))))

(defun check-pkgbuild-arch (&optional (pkgbuild-filename "./PKGBUILD"))
  (let ((carch (get-carch))
        (archlist (get-pkgbuild-arch pkgbuild-filename)))
    (unless (or (member carch archlist :test #'equalp)
                (member "any" archlist :test #'equalp))
      (error "Your system (~A) isn't listed in the PKGBUILD's list of compatible ~
              architectures (~A).~%makepkg will refuse to build it." carch archlist))
    t))

(defun add-carch-to-pkgbuild (&optional (pkgbuild-filename (merge-pathnames"./PKGBUILD" (current-directory))))
  (with-open-file (f pkgbuild-filename :direction :output
                                       :if-exists :append
                                       :if-does-not-exist :error)
    (format f "arch=('~A')~%" (get-carch))))
                     

(defun get-pkgbuild-tarball-name (&optional (pkgbuild-filename "./PKGBUILD"))
  (let ((data (get-pkgbuild-data pkgbuild-filename)))
    (flet ((field (name)
             (cdr (assoc name data :test #'equalp))))
      (format nil "~A-~A-~A-~A.pkg.tar.gz" (field "pkgname") (field "pkgver")
              (field "pkgrel") (if (member "any" (get-pkgbuild-arch) :test #'equalp)
                                 "any"
                                 (get-carch))))))

(defun parse-dep (dep-spec)
  "Parse a versioned dependency specification into a list
(PKGNAME RELATION VERSION)."
  ;; TODO: intern the relation
  (let* ((matches (nth-value 1 (cl-ppcre:scan-to-strings
                                 "^([^<>=]+?)(?:(=|<|>|<=|>=)([^<>=]+))?$"
                                 dep-spec)))
         (result (remove nil (coerce matches 'list))))
    (or result
        (flet ((read-new-spec ()
                 (format t "Enter new dependency spec: ") (force-output)
                 (list (read-line))))
          (restart-case
              (error "Couldn't parse dependency spec ~S" dep-spec)
            (correct (new-spec)
              :report "Correct it by entering a new one"
              :interactive read-new-spec
              (return-from parse-dep (parse-dep new-spec)))))))) ; return-from needed?

(defun get-pkgbuild-dependencies (&optional (pkgbuild-filename "./PKGBUILD"))
  (let ((dep-data (remove-if-not (lambda (key)
                                   (member key '("depends" "makedepends")
                                           :test #'equalp))
                                 (get-pkgbuild-data pkgbuild-filename)
                                 :key #'car)))
    (flet ((field (name)
             (cdr (assoc name dep-data :test #'equalp))))
         ;; TODO: use another separator in case someone used spaces
         ;; in the dep specs themselves.
      (let ((deps (mapcar #'parse-dep
                          (split-sequence #\Space (field "depends")
                                          :remove-empty-subseqs t)))
            (makedeps (mapcar #'parse-dep
                              (split-sequence #\Space (field "makedepends")
                                              :remove-empty-subseqs t))))
        ;(format t "deps: ~A~%" deps)
        ;(format t "makedeps: ~A~%" makedeps)
        ;; TODO: for now we just ignore version information
        (setf deps (mapcar #'first deps)
              makedeps (mapcar #'first makedeps))
        (values deps makedeps)))))

(defun get-pkgbuild (pkg-name)
  (ensure-initial-cache)
  (let ((repo (car (find-package-by-name pkg-name))))
    (cond ((null repo)
	   (format nil "~a: Package not found in AUR or core/extra/community. May be in a custom repo." pkg-name))
	  ((string= "aur" repo) (get-pkgbuild-from-aur pkg-name))
	  (t (get-pkgbuild-from-svn pkg-name repo)))))

;; get-pkgbuild-from-aur currently duplicates install-aur-pkg but without:
;; unwind-protect, checksumming. okay for now.
;; TODO: investigate making a keyword argument :getpkgbuild for install-aur-pkg.
(defun get-pkgbuild-from-aur (pkg-name)
  (let ((aur-tarball (aur-tarball-name pkg-name)))
    (download-file (aur-tarball-uri pkg-name))
    (unpack-file aur-tarball)
    (delete-file aur-tarball)
    (pkgbuild-directory pkg-name)))

(defun get-pkgbuild-from-svn (pkg-name repo)
  (let ((arch (get-carch))
	(server "svn://svn.archlinux.org/")
	(operation "checkout"))
    (flet ((checkout-pkgbuild (directory)
	     (let ((return-value
		    (run-program "svn" (list operation
					     (concatenate 'string server directory pkg-name
							  "/repos/" repo "-" arch)
					     pkg-name))))
	       (if (zerop return-value)
		   (pkgbuild-directory pkg-name)
		   (format nil "Subversion exited with non-zero status ~d for package ~a."
			   return-value pkg-name)))))
      (if (string= repo "community")
	  (checkout-pkgbuild "community/")
	  (checkout-pkgbuild "packages/")))))

(defun pkgbuild-directory (pkg-name)
  (format nil "~a: The pkgbuild is in ~a"
	  pkg-name (concatenate 'string (namestring (current-directory)) pkg-name) "/"))

(defun install-pkg-tarball (&key (tarball (get-pkgbuild-tarball-name)) (location (get-pkgdest)))
  (let ((pkg-location (concatenate 'string (ensure-trailing-slash location) tarball))
	force)
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
	 :report (lambda (s) (format s "Save the package to ~A~A" (config-file "packages/") tarball))
	 (run-program "mv" (list tarball (format nil "~A~A" (config-file "packages/") tarball))))))))

(defun cleanup-temp-files (pkg-name &optional orig-dir)
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
