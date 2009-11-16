
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

(defun get-carch ()
  (let ((data (get-makepkg-data)))
    (flet ((field (name)
             (cdr (assoc name data :test #'equalp))))
      (field "carch"))))

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
  (remove nil (coerce (nth-value 1 (cl-ppcre:scan-to-strings
                                     "^([^<>=]+?)(?:(=|<|>|<=|>=)([^<>=]+))?$"
                                     dep-spec))
                      'list)))

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
  (maybe-refresh-cache)
  (let ((repo (car (find-package-by-name pkg-name)))) ; user can give bad input
    (if (string= "aur" repo)
	(get-pkgbuild-from-aur pkg-name)
	(get-pkgbuild-from-svn pkg-name repo))))

;;; should inform user of progress in get-pkgbuild-from-*

;; get-pkgbuild-from-aur currently duplicates install-aur-pkg but without:
;; unwind-protect, checksumming. okay for now.
;; TODO: investigate making a keyword argument :getpkgbuild for install-aur-pkg.
(defun get-pkgbuild-from-aur (pkg-name)
  (download-file (aur-tarball-uri pkg-name))
  (unpack-file (aur-tarball-name pkg-name))
  (delete-file (aur-tarball-name pkg-name)))

;; TODO: Handle the case where pkg-name directory exists where paktahn -G is invoked.
(defun get-pkgbuild-from-svn (pkg-name repo)
  (let ((arch (get-carch))
	(svnrepo "svn://svn.archlinux.org/")
	(svnop "checkout")
	(svndepth "--depth=empty"))
    (labels ((svn-file-shuffling (directory)
	       (run-program "svn" (list svnop svndepth (concatenate 'string svnrepo directory)))
	       (setf (current-directory) directory)
	       (run-program "svn" (list "update" pkg-name))
	       (run-program "mv" (list (concatenate 'string pkg-name "/repos/" repo "-" arch)
				       (concatenate 'string "../" pkg-name)))
	       (setf (current-directory) "..")
	       (delete-directory-and-files directory)))
      (if (string= repo "community")
	  (svn-file-shuffling "community")
	  (svn-file-shuffling "packages")))))