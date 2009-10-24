(defsystem :paktahn
  :name "Paktahn"
  :description "A package management wrapper to replace Yaourt on Arch Linux"
  :version "0.8.2"
  :author "Leslie Polzer"
  :license "GPL"
  :depends-on (:md5 :trivial-backtrace :cl-store :cl-json
	       :drakma :cffi :alexandria :metatilities
	       :getopt :split-sequence :cl-ppcre :py-configparser)
  :serial t
  :components ((:file "pyconfig-fix")
	       (:file "readline")
	       (:file "term")
	       (:file "util")
	       (:file "alpm")
	       (:file "checksums")
	       (:file "pkgbuild")
	       (:file "aur")
	       (:file "cache")
	       (:file "main")))