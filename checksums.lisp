(in-package :pak)

;; Right now, this data structure choice seems good but the choice of pkg-name for a key
;; could make extending the checksumming to files other than PKGBUILDs messy later.
(defvar *checksums* (make-hash-table :test #'equal)
  "Hash table containing checksums of PKGBUILDs.
The package names are the keys. The checksum byte arrays
are the values. Support will be added for non-PKGBUILD files later.")

(defun load-checksums ()
  (when (probe-file (config-file "checksums"))
    (setf *checksums* (cl-store:restore (config-file "checksums")))))

(defun compare-checksums (pkg-name)
  (let ((pkgbuild-md5 (sb-md5:md5sum-file "PKGBUILD"))
	(stored-md5 (lookup-checksum pkg-name)))
    (cond ((not stored-md5)  ; if new PKGBUILD, ask the user to review it and add it to the checksum-db
	   (prompt-user-review "PKGBUILD")
	   (add-checksum pkg-name pkgbuild-md5))
	  ((and (not (equalp stored-md5 pkgbuild-md5)) ; otherwise, compare its md5sum to that on record and prompt the user if necessary
		(ask-y/n "The PKGBUILD checksum doesn't match our records. Review the PKGBUILD?"))
	   (launch-editor "PKGBUILD"))))) ; Do we also need to (add-checksum pkg-name) here? Which md5 do we keep?

(defun add-checksum (pkg-name checksum)
  (setf (gethash pkg-name *checksums*) checksum))

(defun lookup-checksum (pkg-name)
  (gethash pkg-name *checksums*))

;; TODO: Add checksums-file locking here.