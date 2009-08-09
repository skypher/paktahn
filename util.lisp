
(in-package :pak)

(defparameter *default-tempdir* "/var/tmp") ; TODO: separate subdir for Paktahn


(defun quit ()
  #+sbcl(sb-ext:quit))

#+sbcl
(defun enable-quit-on-sigint ()
  #+sbcl(sb-sys:enable-interrupt sb-unix:sigint 
          (lambda (&rest args) (declare (ignore args))
            (quit))))

(defun getargv ()
  #+sbcl sb-ext:*posix-argv*
  #-sbcl(error "no argv"))

(defun getcwd ()
  #+sbcl(sb-posix:getcwd)
  #-sbcl(error "no getcwd"))

(defun chdir (dir)
  #+sbcl(sb-posix:chdir dir)
  #-sbcl(error "no chdir"))

(defun mkdir (dir &optional (mode #o755))
  #+sbcl(sb-posix:mkdir dir mode)
  #-sbcl(error "no mkdir"))

(defun getenv (dir)
  #+sbcl(sb-posix:getenv dir)
  #-sbcl(error "no getenv"))

(defun tempdir ()
  (let ((dir (or (getenv "PAKTAHN_TMPDIR") (getenv "TMPDIR") *default-tempdir*)))
    (unless (probe-file dir)
      ;; TODO: offer restarts CREATE, SPECIFY, DEFAULT
      (warn "Temporary directory ~S doesn't exist, using default ~S"
            dir *default-tempdir*)
      (assert *default-tempdir*)
      (setf dir *default-tempdir*))
    dir))

(defun getuid ()
  #+sbcl(sb-unix:unix-getuid)
  #-sbcl(error "no getuid"))

(defun rootp ()
  (zerop (getuid)))

(defun run-program (program args &key capture-output-p)
  #+sbcl(let ((result (sb-ext:run-program program
                             (ensure-list args)
                             :wait t :search t
                             :input t
                             :output (if capture-output-p
                                       :stream
                                       t))))
          (values (sb-ext:process-exit-code result)
                  (sb-ext:process-output result)))
  #-sbcl(error "no run-program"))

(defun getline ()
  (finish-output *standard-output*)
  (or (read-line *standard-input* nil) (quit)))

(defun ask-for-editor ()
  (format t "You do not have your EDITOR environment variable set.~%
          Please tell me the name of your preferred editor.")
  (flet ((show-prompt () (format t " =>")))
    (loop for input = (progn (show-prompt) (getline))
          until input
          finally (return input))))

(defun launch-editor (filename)
  (format t "INFO: editing is not supported yet, but here's~
             the PKGBUILD for review:~%==========~%")
  (tagbody again
    ;; FIXME: run-program kludge again, can't do interactive I/O.
    (let* ((editor "cat" #+(or)(or (getenv "EDITOR")
                                   (ask-for-editor)))
           (return-value (run-program editor filename)))
      (unless (zerop return-value)
        (warn "Editor ~S exited with non-zero status ~D" editor return-value)
        (when (y-or-n-p "Choose another editor")
          (go again)))))
  (format t "~&==========~%"))

(defun download-file (uri)
  (tagbody retry
    (let ((return-value (run-program "wget" (list "-c" uri))))
      (unless (zerop return-value)
        (if (y-or-n-p "Download via wget failed with status ~D. Retry?" return-value)
          (go retry)
          (return-from download-file)))
      t)))

(defun unpack-file (name)
  (tagbody retry
    (let ((return-value (run-program "tar" (list "xfvz" name))))
      (unless (zerop return-value)
        (if (y-or-n-p "Unpacking failed with status ~D. Retry?" return-value)
          (go retry)
          (return-from unpack-file)))
      t)))

