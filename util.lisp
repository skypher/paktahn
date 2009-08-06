
(in-package :pacman)

(defun quit ()
  #+sbcl(sb-ext:quit))

#+sbcl
(sb-sys:enable-interrupt sb-unix:sigint 
  (lambda (&rest args) (declare (ignore args))
    (sb-ext:quit)))

(defun getcwd ()
  #+sbcl(sb-posix:getcwd)
  #-sbcl(error "no getcwd"))

(defun chdir (dir)
  #+sbcl(sb-posix:chdir)
  #-sbcl(error "no chdir"))

(defun getenv (dir)
  #+sbcl(sb-posix:getenv)
  #-sbcl(error "no chdir"))

(defun run-program (program args &key capture-output-p)
  #+sbcl(let ((result (apply #'sb-ext:run-program program args
                             :wait t :search t
                             (when capture-output-p
                               '(:output :stream)))))
          (values (sb-ext:process-exit-code result)
                  (sb-ext:process-output result)))
  #-sbcl(error "no run-program"))

