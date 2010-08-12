(in-package :pak)

(defvar *checksums* (make-hash-table :test #'equal)
  "Hash table containing checksums of PKGBUILDs.
The package names are the keys. The checksum byte arrays
are the values.")

(defun load-checksums ()
  (let ((checksum-file (config-file "checksums")))
    (when (probe-file checksum-file)
      (with-locked-input-file (file checksum-file)
	(setf *checksums* (cl-store:restore file))))))

(defun save-checksums ()
  (with-locked-output-file (file (config-file "checksums"))
    (cl-store:store *checksums* file)))

(defun compare-checksums (pkg-name)
  (let ((pkgbuild-md5 (md5sum-file "PKGBUILD"))
	(old-md5s (lookup-checksum pkg-name))
        (review-p t))
    (cond ((not old-md5s)
           (info "This is a new PKGBUILD."))
	  ((new-checksum-p pkgbuild-md5 old-md5s)
           (info "The PKGBUILD has changed since you saw it last."))
          (t
           (info "You have already reviewed this PKGBUILD")
           (setf review-p nil)))
    (when (and review-p (ask-y/n "Review it" t))
      (launch-editor "PKGBUILD"))
    (add-checksum pkg-name pkgbuild-md5)))

(defun add-checksum (pkg-name checksum)
  (pushnew checksum (gethash pkg-name *checksums*) :test #'equalp))

(defun lookup-checksum (pkg-name)
  (gethash pkg-name *checksums*))

(defun new-checksum-p (new old)
  (not (member new old :test #'equalp)))
