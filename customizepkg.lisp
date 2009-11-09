(in-package :pak)

(defvar *customizepkg-dir* "/etc/customizepkg.d/")
(defvar *customizepkg-installed* nil)
(defvar *custompkg-list* (make-array 8 :element-type 'string
				       :adjustable t :fill-pointer 0))

(defun check-for-customizepkg ()
  (when (probe-file *customizepkg-dir*)
    (setf *customizepkg-installed* t)))

(defun check-for-custom-pkgs ()
  (when *customizepkg-installed*
    (let ((files (directory (make-pathname
			     :name :wild :type :wild
			     :defaults *customizepkg-dir*))))
      (loop for file in files do
	(vector-push-extend (pathname-name file) *custompkg-list*)))))