(defmacro without-package-variance-warnings (&body body)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (handler-bind (#+sbcl(sb-int:package-at-variance #'muffle-warning))
       ,@body)))

(without-package-variance-warnings
    (defpackage paktahn
      (:nicknames :pak)
      (:use :cl :cffi)
      (:import-from :alexandria :compose :curry :rcurry :ensure-list)
      (:import-from :metatilities :push-end :aif :it)
      (:import-from :split-sequence :split-sequence)))

