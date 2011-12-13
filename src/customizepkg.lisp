(in-package :pak)

(defvar *customizepkg-bin* "/usr/bin/customizepkg")
(defvar *customizepkg-dir* "/etc/customizepkg.d/")
(defvar *custompkg-list* nil)

(defun check-for-customizepkg ()
  (when (probe-file *customizepkg-bin*)
    (check-for-custom-packages)))

(defun check-for-custom-packages ()
  (let ((files (directory (make-pathname
                           :name :wild :type :wild
                           :defaults *customizepkg-dir*))))
    (loop for file in files do
      (unless (pathname-type file)
        (push (pathname-name file) *custompkg-list*)))))

(defun customize-p (pkg-name)
  (member pkg-name *custompkg-list* :test #'equal))

;; Run customizepkg on the PKGBUILD
(defun apply-customizations (&optional path)
  (if path
      (run-program "customizepkg" (list "--modify" path))
      (run-program "customizepkg" '("--modify"))))
