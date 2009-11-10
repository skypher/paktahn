(in-package :pak)

(defvar *customizepkg-dir* "/etc/customizepkg.d/")
(defvar *custompkg-list* nil)

(defun check-for-customizepkg ()
  (when (probe-file *customizepkg-dir*)
    (setf *customizepkg-installed* t)
    (check-for-custom-pkgs)))

(defun check-for-custom-pkgs ()
  (let ((files (directory (make-pathname
			   :name :wild :type :wild
			   :defaults *customizepkg-dir*))))
    (loop for file in files do
      (unless (pathname-type file)
	(push (pathname-name file) *custompkg-list*)))))

(defun customization-p (pkg-name)
  (member pkg-name *custompkg-list* :test #'equal))

;; Run customizepkg on the PKGBUILD
(defun apply-customizations ()
  (run-program "customizepkg" '("--modify")))