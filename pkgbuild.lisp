
(in-package :pak)

(defparameter *pkgbuild-helper* "/usr/lib/paktahn/pkgbuild-helper.sh")


(defun get-pkgbuild-data (&optional (pkgbuild-filename "./PKGBUILD"))
  (multiple-value-bind (return-value output-stream)
      (run-program *pkgbuild-helper* (list pkgbuild-filename)
                   :capture-output-p t)
    (unless (zerop return-value)
      (error "Couldn't extract PKGBUILD data (error ~D)" return-value))
    (loop for line = (read-line output-stream nil nil)
          with key
          while line
          if key
            collect (cons key line)
            and do (setf key nil)
          else
            do (setf key line))))

(defun get-pkgbuild-tarball-name (&optional (pkgbuild-filename "./PKGBUILD"))
  (let ((data (get-pkgbuild-data pkgbuild-filename)))
    (flet ((field (name)
             (cdr (assoc name data :test #'equalp))))
      (format nil "~A-~A-~A-~A.pkg.tar.gz" (field "pkgname") (field "pkgver")
              (field "pkgrel") (field "arch")))))

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

