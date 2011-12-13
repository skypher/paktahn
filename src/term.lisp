(in-package :pak)

(defparameter *ansi-colors* '((black . 0) (red . 1) (green . 2) (yellow . 3)
                              (blue . 4) (magenta . 5) (cyan . 6) (white . 7)))

(defparameter *color-scheme-darkbg* '(:info white
                                      :note (:fg white :boldp nil)
                                      :db ((core . green)
                                           (extra . green)
                                           (unstable . red))
                                      :pkg-result-id (:fg yellow :invertp t)
                                      :pkg-name white
                                      :pkg-version green
                                      :pkg-installed (:fg green :invertp t)
                                      :pkg-outofdate red
                                      :pkg-old (:fg red :invertp t)
                                      :pkg-description nil))

(defparameter *color-scheme-lightbg* '(:info black
                                       :note (:fg black :boldp nil)
                                       :db ((core . green)
                                            (extra . green)
                                            (unstable . red))
                                       :pkg-result-id (:fg blue :invertp t)
                                       :pkg-name black
                                       :pkg-version green
                                       :pkg-installed (:fg green :invertp t)
                                       :pkg-outofdate red
                                       :pkg-old (:fg red :invertp t)
                                       :pkg-description nil))

(defvar *color-scheme* *color-scheme-darkbg*)

(defun term-set (modes)
  (format t "~C[~Dm" #\Escape modes)
  (finish-output *standard-output*))

(defun term-set-fg-color (colorname &key (boldp t))
  (let ((colid (cdr (assoc colorname *ansi-colors*))))
    (assert colid)
    (term-set (format nil "~D;3~D" (if boldp 1 0) colid))))

(defun term-set-bg-color (colorname &key (boldp t))
  (let ((colid (cdr (assoc colorname *ansi-colors*))))
    (assert colid)
    (term-set (format nil "~D;4~D" (if boldp 1 0) colid))))

(defun term-reset-colors ()
  (term-set 0))

(defun term-invert ()
  (term-set 7))

(defun set-term-colors (&key fg bg (boldp t) invertp)
  (when invertp
    (term-invert))
  (when fg
    (term-set-fg-color fg :boldp boldp))
  (when bg
    (term-set-bg-color bg :boldp boldp)))

(defmacro with-term-colors ((&key fg bg (boldp t) invertp) &body body)
  `(progn
     (set-term-colors :fg ,fg :bg ,bg :boldp ,boldp :invertp ,invertp)
     (unwind-protect
         (progn ,@body)
       (term-reset-colors))))

(defmacro with-term-colors/id (id &body body)
  `(progn
     (let ((attrs ,(etypecase id
                     (atom `(getf *color-scheme* ,id))
                     (cons `(or (cdr (assoc ,(cadr id) (getf *color-scheme* :db)
                                            :test #'string-equal))
                                '(:fg magenta)))))) ; FIXME don't hardcode this
       (etypecase attrs
         (atom
           (set-term-colors :fg attrs :boldp t))
         (cons
           (apply #'set-term-colors attrs))))
     (unwind-protect
         (progn ,@body)
       (term-reset-colors))))
