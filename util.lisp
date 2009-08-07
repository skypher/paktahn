
(in-package :pak)

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

(defun getenv (dir)
  #+sbcl(sb-posix:getenv dir)
  #-sbcl(error "no chdir"))

(defun getuid ()
  #+sbcl(sb-unix:unix-getuid)
  #-sbcl(error "no chdir"))

(defun rootp ()
  (zerop (getuid)))

(defun run-program (program args &key capture-output-p)
  #+sbcl(let ((result (sb-ext:run-program program args
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
  (tagbody again
    (let* ((editor (or (getenv "EDITOR")
                       (ask-for-editor)))
           (return-value (run-program (getenv "EDITOR") filename)))
      (unless (zerop return-value)
        (warn "Editor ~S exited with non-zero status ~D" editor return-value)
        (when (y-or-n-p "Choose another editor")
          (go again))))))

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

