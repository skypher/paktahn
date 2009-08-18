
(in-package :pak)


(defparameter *ansi-colors* '((black . 0) (red . 1) (green . 2) (yellow . 3)
                              (blue . 4) (magenta . 5) (cyan . 6) (white . 7)))

(defparameter *db-colors* '((core . green) (extra . green) (unstable . red)))


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

(defmacro with-term-colors ((&key fg bg (boldp t) invertp) &body body)
  `(progn
     ,(when invertp
        `(term-invert))
     ,(when fg
        `(term-set-fg-color ,fg :boldp ,boldp))
     ,(when bg
        `(term-set-bg-color ,bg :boldp ,boldp))
     (unwind-protect (progn ,@body)
       (term-reset-colors))))

