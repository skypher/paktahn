(defpackage :paktahn-tests
  (:use :cl :paktahn :fiveam)
  (:export #:run!))

(in-package :paktahn-tests)

;; It will make sense to split this up into several suites as we get more tests.
;; i.e. (libalpm, pkgbuild, customizepkg, argv, etc)

(def-suite :pak)
(in-suite :pak)

(test t-is-not-null
  (is (not (null t))))
