
(in-package :pak)

(defparameter *default-tempdir* "/var/tmp/paktahn")

(defvar *on-error* :debug)

(defun show-restarts (restarts s)
  (cond ((null restarts)
         (format s
                 "~&(no restarts: If you didn't do this on purpose, ~
                  please report it as a bug.)~%"))
        (t
         (format s "~&Please choose how you want to proceed:~%")
         (let ((count 0)
               (names-used '(nil))
               (max-name-len 0))
           (dolist (restart restarts)
             (let ((name (restart-name restart)))
               (when name
                 (let ((len (length (princ-to-string name))))
                   (when (> len max-name-len)
                     (setf max-name-len len))))))
           (unless (zerop max-name-len)
             (incf max-name-len 3))
           (dolist (restart restarts)
             (let ((name (restart-name restart)))
               ;; FIXME: maybe it would be better to display later names
               ;; in parens instead of brakets, not just omit them fully.
               ;; Call BREAK, call BREAK in the debugger, and tell me
               ;; it's not confusing looking. --NS 20050310
               (cond ((member name names-used)
                      (format s "~& ~2D: ~V@T~A~%" count max-name-len restart))
                     (t
                      (format s "~& ~2D: [~VA] ~A~%"
                              count (- max-name-len 3) name restart)
                      (push name names-used))))
             (incf count))))))

(defun ask-for-restart (restarts)
  ;; TODO should be *query-io* but GETLINE needs to be modified
  (show-restarts restarts *standard-output*)
  ;(trace parse-integer read-char)
  (loop for x = (getline (format nil "~&[0-~D] ==> " (1- (length restarts))))
        for n = (ignore-errors (parse-integer x))
        until (and n (>= n 0) (< n (length restarts)))
        finally (return (nth n restarts))))

(defun default-error-handler (c)
  (term-reset-colors)
  (ecase *on-error*
    (:debug
     (invoke-debugger c))
    ((:backtrace :quit)
     (flet ((bail-out ()
              (format t "~&Fatal error: ~A~%" c)
              (ecase *on-error*
                (:backtrace
                 (trivial-backtrace:print-backtrace c)
                 (quit))
                (:quit
                 (quit)))))
       (let ((restarts (remove-if (lambda (name)
                                    (member name '(abort use-value store-value
                                                    muffle-warning)))
                                  (compute-restarts c) :key #'restart-name)))
         ;(format t "preprocessed restarts: ~S~%" restarts)
         (when restarts
           (let ((restart (ask-for-restart restarts)))
             (invoke-restart restart)))
         ;; out of options
         (bail-out))))))


(defun quit ()
  #+paktahn-deploy
    #+sbcl(sb-ext:quit))

#+sbcl
(defun enable-quit-on-sigint ()
  #+sbcl(sb-sys:enable-interrupt sb-unix:sigint 
          (lambda (&rest args) (declare (ignore args))
            (default-error-handler (make-condition 'simple-error
                                                   :format-control "Interrupt"))
            (quit))))

(defun getargv ()
  #+sbcl sb-ext:*posix-argv*
  #-sbcl(error "no argv"))

(defun getpid ()
  #+sbcl(sb-posix:getpid)
  #-sbcl(error "no getpid"))

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

(defun homedir ()
  (parse-namestring
    (concatenate 'string (getenv "HOME") "/")))

(defun home-relative (path)
  (merge-pathnames (parse-namestring path) (homedir)))

(defun config-file (path)
  (let ((result (merge-pathnames
                  (parse-namestring path)
                  (home-relative ".paktahn/"))))
    (ensure-directories-exist result)
    result))

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

(defun getline (prompt)
  (finish-output *standard-output*)
  (or (readline (the string prompt)) (quit)))

(defun ask-y/n (question-string &optional (default nil default-supplied-p))
  (assert (member default '(t nil)))
  (let ((y-ch (if (and default-supplied-p (eq default t)) #\Y #\y))
        (n-ch (if (and default-supplied-p (eq default nil)) #\N #\n)))
    (labels ((maybe-print-query (hint format-string &rest format-args)
               (fresh-line *query-io*)
               (when format-string
                 (apply #'format *query-io* format-string format-args)
                 (write-char #\Space *query-io*))
               (format *query-io* "~A " hint)
               (finish-output *query-io*))
             (print-query ()
               (maybe-print-query (format nil "(~C/~C)" y-ch n-ch)
                                  question-string))
             (query-read-char ()
               (clear-input *query-io*)
               (prog1 (read-char *query-io*)
                 (clear-input *query-io*)))
             (clarify-legal-query-input ()
               (format *query-io* "~&Please type \"y\" for yes or \"n\" for no.~:[~; Hit RETURN for the default.~]~%" default-supplied-p)))
      (loop (print-query)
            (let ((ch (query-read-char)))
              (cond 
                ((member ch '(#\y #\Y)) (return t))
                ((member ch '(#\n #\N)) (return nil))
                ((and default-supplied-p (char-equal ch #\Newline))
                 (return default))
                (t (clarify-legal-query-input))))))))

(defun ask-for-editor ()
  (format t "You do not have your EDITOR environment variable set.~%
          Please tell me the name of your preferred editor.")
  (loop for input = (getline " => ")
        until input
        finally (return input)))

(defun launch-editor (filename)
  #-run-program-fix
  (format t "INFO: editing is not supported because you're using Paktahn~%~
             with an unpatched SBCL, but here's the PKGBUILD for review:~%~
             ==========~%")
  (tagbody again
    ;; FIXME: run-program kludge again, can't do interactive I/O.
    (let* ((editor #-run-program-fix "cat"
                   #+run-program-fix (or (getenv "EDITOR")
                                         (ask-for-editor)))
           (return-value (run-program editor filename)))
      (unless (zerop return-value)
        (warn "Editor ~S exited with non-zero status ~D" editor return-value)
        (when (ask-y/n "Choose another editor?" t)
          (go again)))))
  (format t "~&==========~%"))

(defun download-file (uri)
  (tagbody retry
    (let ((return-value (run-program "wget" (list "-c" uri))))
      (unless (zerop return-value)
        (if (ask-y/n (format nil "Download via wget failed with status ~D. Retry?" return-value))
          (go retry)
          (error "Failed to download file ~S" uri)))
      t)))

(defun unpack-file (name)
  (tagbody retry
    (let ((return-value (run-program "tar" (list "xfvz" name))))
      (unless (zerop return-value)
        (if (ask-y/n (format nil "Unpacking failed with status ~D. Retry?" return-value))
          (go retry)
          (error "Failed to unpack file ~S" name)))
      t)))

(defparameter *info-fmt-prefix* "==> ")
(defparameter *info-fmt-suffix* "~%")

(defun info (fmt &rest args)
  (with-term-colors (:fg 'white)
    ;; TODO recursive format
    (apply #'format t (concatenate 'string *info-fmt-prefix* fmt *info-fmt-suffix*)
           args))
  (finish-output *standard-output*))

(defmacro with-progress-info ((fmt &rest args) &body body)
  ;; TODO: support nested progress infos
  `(progn
     (let ((*info-fmt-suffix* ""))
       (info (format nil "~A... " ,fmt) ,@args))
     ,@body
     (let ((*info-fmt-prefix* ""))
       (info "done."))))

(defun load-config ()
  (let ((conf (config-file "config.lisp")))
    (when (probe-file conf)
      (load conf))))

