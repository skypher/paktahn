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

(defun new-pkgbuild-p (pkg-name)
  (let ((pkgbuild-md5 (md5sum-file "PKGBUILD"))
        (old-md5s (lookup-checksum pkg-name)))
    (add-checksum pkg-name pkgbuild-md5)
    (cond ((not old-md5s)
           (info "This is a new PKGBUILD.") t)
          ((new-checksum-p pkgbuild-md5 old-md5s)
           (info "The PKGBUILD has changed since you saw it last.") t)
          (t
           (info "You have already reviewed this PKGBUILD.") nil))))

(defun add-checksum (pkg-name checksum)
  (pushnew checksum (gethash pkg-name *checksums*) :test #'equalp))

(defun lookup-checksum (pkg-name)
  (gethash pkg-name *checksums*))

(defun new-checksum-p (new old)
  (not (member new old :test #'equalp)))
