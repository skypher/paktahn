(in-package :pak)

(defvar *checksums* (make-hash-table :test #'equal)
  "Hash table containing checksums of PKGBUILDs.
The package names are the keys. The checksum byte arrays
are the values. Support will be added for non-PKGBUILD files later.")

(defun load-checksums ()
  (when (probe-file (config-file "checksums"))
    (setf *checksums* (cl-store:restore (config-file "checksums")))))

(defun compare-checksums (pkg-name)
  (multiple-value-bind (value present) (gethash pkg-name *checksums*)
    (cond ((not present) ; if not, add its md5sum to the checksum-db and ask the user to review it
	   (review-file "PKGBUILD")
	   (add-checksum pkg-name))
	  ((unless (equalp value pkgbuild-md5) ; otherwise, compare its md5sum to that on record and prompt the user if necessary
	     (when (ask-y/n "The PKGBUILD checksum doesn't match our records. Review the PKGBUILD?")
	       (launch-editor "PKGBUILD"))))))) ; Do we also (add-checksum pkg-name) here? Which md5 do we keep?

(defun add-checksum (pkg-name)
  (let ((pkgbuild-md5 (sb-md5:md5sum-file "PKGBUILD")))
    (setf (gethash pkg-name *checksums*) pkgbuild-md5)))

;; IMPLEMENT LOCKING HERE 