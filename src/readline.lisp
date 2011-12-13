(in-package :pak)

(define-foreign-library libreadline
  (:unix (:or "libreadline.so.6" "libreadline.so"))
  (t (:default "readline")))

(use-foreign-library libreadline)

(defcfun "readline" :string (prompt :string))

(define-foreign-library libhistory
  (:unix (:or "libhistory.so.6" "libhistory.so"))
  (t (:default "history")))

(use-foreign-library libhistory)

(defcfun "add_history" :void (line :string))
