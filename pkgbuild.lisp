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

(defun get-pkgext ()
  (get-makepkg-field "pkgext"))

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

(defun add-carch-to-pkgbuild (&optional (pkgbuild-filename
                                         (merge-pathnames"./PKGBUILD" (current-directory))))
  (with-open-file (f pkgbuild-filename :direction :output
                                       :if-exists :append
                                       :if-does-not-exist :error)
    (format f "arch=('~A')~%" (get-carch))))

(defun get-pkgbuild-tarball-name (&optional (pkgbuild-filename "./PKGBUILD"))
  (let ((data (get-pkgbuild-data pkgbuild-filename)))
    (flet ((field (name)
             (cdr (assoc name data :test #'equalp))))
      (format nil "~A-~A-~A-~A~A" (field "pkgname")
              (field "pkgver") (field "pkgrel")
              (if (member "any" (get-pkgbuild-arch) :test #'equalp)
                  "any"
                  (get-carch)) (get-pkgext)))))

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
        (loop for pkg in (append deps makedeps) do
                (unless (find-package-by-name (first pkg))
                        (info "The dependency ~a could not be found. It will be ignored." (first pkg))))    
        (values (remove-if-not #'find-package-by-name
                        (mapcar #'first deps))
                (remove-if-not #'find-package-by-name
                        (mapcar #'first makedeps)))))))

(defun get-pkgbuild-provides (&optional (pkgbuild-filename "./PKGBUILD"))
  (let ((provides-data (remove-if-not (lambda (key)
                                   (member key '("provides" )
                                           :test #'equalp))
                                 (get-pkgbuild-data pkgbuild-filename)
                                 :key #'car)))
    (flet ((field (name)
             (cdr (assoc name provides-data :test #'equalp))))
         ;; TODO: use another separator in case someone used spaces
         ;; in the provides specs themselves.
      (let ((provides (mapcar #'parse-provides
                          (split-sequence #\Space (field "provides")
                                          :remove-empty-subseqs t))))
        ;; TODO: for now we just ignore version information
        (loop for pkg in (append provides) do
                (unless (find-package-by-name (first pkg))
                        (info "The provides ~a could not be found. It will be ignored." (first pkg))))    
        (values (remove-if-not #'find-package-by-name
                        (mapcar #'first provides)))))))

(defun get-pkgbuild (pkg-name)
  (ensure-initial-cache)
  (let ((repo (car (find-package-by-name pkg-name))))
    (cond ((null repo)
           (format nil "~a: Package not found in AUR or core/extra/community." pkg-name))
          ((string= "aur" repo) (get-pkgbuild-from-aur pkg-name))
          (t (get-pkgbuild-from-svn pkg-name repo)))))

(defun get-pkgbuild-from-aur (pkg-name)
  (with-tmp-dir (#P"/tmp/" (current-directory))
    (let ((aur-tarball (aur-tarball-name pkg-name)))
      (retrying
        (restart-case
            (when (probe-file (merge-pathnames aur-tarball))
              (error "A tarball named ~a already exists." aur-tarball))
          (overwrite ()
            :report (lambda (s) (format s "Overwrite the existing file."))
            (delete-file (merge-pathnames aur-tarball))
            (retry))
          (resume ()
            :report (lambda (s) (format s "Resume the download with the existing file.")))))
      (download-file (aur-tarball-uri pkg-name))
      (unpack-file aur-tarball end-dir)
      (delete-file aur-tarball)
      (pkgbuild-directory end-dir pkg-name))))

(defun get-pkgbuild-from-svn (pkg-name repo)
  (let ((server "svn://svn.archlinux.org/"))
    (flet ((checkout-pkgbuild (directory)
             (let ((return-value
                    (run-program "svn" `("co" ,(format nil "~a~a/~a/trunk/"
                                                       server directory pkg-name) ,pkg-name))))
               (if (zerop return-value)
                   (pkgbuild-directory (current-directory) pkg-name)
                   (format nil "Subversion exited with non-zero status ~d for package ~a."
                           return-value pkg-name)))))
      (if (string= repo "community")
          (checkout-pkgbuild "community")
          (checkout-pkgbuild "packages")))))

(defun pkgbuild-directory (pathspec pkg-name)
  (format nil "~a: The pkgbuild is in ~a~a/"
          pkg-name (namestring pathspec) pkg-name))

(defun install-pkg-tarball (&key (tarball (get-pkgbuild-tarball-name))
                            (location (get-pkgdest))
                            (as-dep nil))
  (let ((pkg-location (concatenate 'string (ensure-trailing-slash location) tarball))
        force)
    (retrying
     (restart-case
         (let ((exit-code
                 (run-pacman (append (list (if force "-Uf" "-U"))
                                     (when as-dep (list "--asdeps"))
                                     (list pkg-location)) :force t)))
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

(defun cleanup-temp-files (pkg-name)
  (let ((pkgdir (merge-pathnames
                 (make-pathname :directory `(:relative ,pkg-name))
                 (current-directory)))
        (tarball (merge-pathnames
                  (make-pathname :name (aur-tarball-name pkg-name))
                  (current-directory))))
    (when (probe-file pkgdir)
      (delete-directory-and-files pkgdir))
    (when (probe-file tarball)
      (delete-file tarball))))

(defun get-pkg-provides (pkg-name pkg-version)
  (let ((last-line nil)
       (desc-path (format nil "/var/lib/pacman/local/~a-~a/desc"
                           pkg-name pkg-version)))
    (with-open-file (in desc-path)
        (loop for line = (read-line in nil) while line do
              (if (string= last-line "%PROVIDES%")
                  (return line)
                  (setf last-line line))))))
