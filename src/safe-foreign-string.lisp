(in-package :cffi)

(export '(safe-string safe-foreign-string-to-lisp))

;; TODO: multiple fallback encodings

(define-foreign-type safe-string-type ()
  ()
  (:actual-type :pointer)
  (:simple-parser safe-string))

(define-parse-method safe-string-type ()
  (make-instance 'safe-string))

(defmethod translate-to-foreign (string (type safe-string-type))
  (foreign-string-alloc string))

(defmethod translate-from-foreign (pointer (type safe-string-type))
  (safe-foreign-string-to-lisp pointer))

(defmethod free-translated-object (pointer (type safe-string-type) param)
  (declare (ignore param))
  (foreign-string-free pointer))

(defun safe-foreign-string-to-lisp (cstring-sap)
  "Convert the foreign string CSTRING-SAP to a Lisp string
using the Babel default encoding (likely UTF8). Offers useful
restarts if this fails."
  (handler-case
      (foreign-string-to-lisp cstring-sap)
    (babel-encodings:character-decoding-error (c)
      ;; take a wild guess
      (let ((latin9 (handler-case
                        (foreign-string-to-lisp cstring-sap :encoding :latin9)
                      (babel-encodings:character-decoding-error () nil))))
        (restart-case
            (error "Problem decoding foreign string as UTF8: ~A" c)
          (use-latin9 ()
              :test (lambda (c) latin9)
              :report (lambda (s)
                        (format s "Use the ISO-8859-15 representation ~S instead"
                                (subseq latin9 0 (min (length latin9) 32))))
            (return-from safe-foreign-string-to-lisp latin9))
          (use-empty ()
              :report "Use an empty string instead. Be sure to know what you're doing!"
            (return-from safe-foreign-string-to-lisp "")))))))
