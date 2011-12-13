(in-package :pak)

;;;; helper macros
(defmacro show-result (&body body)
  `(let ((result (progn ,@body)))
     (format t "result of form ~S: ~S~%" ',body result)
     result))

(defmacro retrying (&body body)
  "Execute BODY in a PROGN and return its value upon completion.
BODY may call RETRY at any time to restart its execution."
  (let ((tagbody-name (gensym))
        (block-name (gensym)))
    `(block ,block-name
       (tagbody ,tagbody-name
         (flet ((retry () (go ,tagbody-name)))
           (return-from ,block-name (progn ,@body)))))))

(defmacro with-tmp-dir ((start-dir end-dir) &body body)
  "Warning: This macro is unhygienic and captures START-DIR and END-DIR.
cd to START-DIR, execute BODY in an UNWIND-PROTECT and cd to END-DIR."
  `(let ((start-dir ,start-dir)
         (end-dir ,end-dir))
     (setf (current-directory) start-dir)
     (unwind-protect (progn ,@body)
       (setf (current-directory) end-dir))))

;;;; infos, notes, progress
(defparameter *info-fmt-prefix* "==> ")
(defparameter *info-fmt-suffix* "~%")

(defun %info (fmt &rest args)
  ;; TODO recursive format
  (apply #'format t (concatenate 'string *info-fmt-prefix* fmt *info-fmt-suffix*) args)
  (finish-output *standard-output*))

(defun info (fmt &rest args)
  (with-term-colors/id :info
    (apply #'%info fmt args)))

(defun note (fmt &rest args)
  (with-term-colors/id :note
    (apply #'%info fmt args)))

(defmacro with-progress-info ((fmt &rest args) &body body)
  ;; TODO: support nested progress infos
  `(progn
     (let ((*info-fmt-suffix* ""))
       (info (format nil "~A... " ,fmt) ,@args))
     ,@body
     (let ((*info-fmt-prefix* ""))
       (info "done."))))

;;;; error handling
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
  (format *standard-output* "~%")
  ;(trace parse-integer read-char)
  (loop for x = (getline (format nil "~&[0-~D] ==> " (1- (length restarts))))
        for n = (handler-case (parse-integer x) (parse-error () nil))
        until (and n (>= n 0) (< n (length restarts)))
        finally (return (nth n restarts))))

(defun default-error-handler (c &key before-invoke-restart-fn)
  (term-reset-colors)
  (let ((*info-fmt-prefix* "~&~%==> "))
    (info "~A" c))
  (ecase *on-error*
    (:debug
     (invoke-debugger c))
    ((:backtrace :quit)
     (flet ((bail-out ()
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
         (and restarts (> (length restarts) 1)
           (let ((restart (ask-for-restart restarts)))
             (when before-invoke-restart-fn
               (funcall before-invoke-restart-fn))
             (invoke-restart restart)))
         ;; out of options
         (bail-out))))))

;;;; quit and interruptions
(defun quit ()
  #+paktahn-deploy
  (progn
    #+sbcl(sb-ext:quit)
    #+ecl(ext:quit 0)
    #+ccl(ccl::quit)))

(defun enable-quit-on-sigint ()
  #+sbcl(labels ((install-handler (handler)
                   (sb-sys:enable-interrupt sb-unix:sigint handler))
                 (level2-handler (&rest args)
                   "No messing around, the user's serious this time."
                   (declare (ignore args))
                   (sb-sys:enable-interrupt sb-unix:sigint :default)
                   (quit))
                 (level1-handler (&rest args)
                   "Present restarts if applicable."
                   (declare (ignore args))
                   (install-handler #'level2-handler)
                   (sb-sys:with-interrupts
                     (default-error-handler
                         (make-condition 'simple-error
                                         :format-control "Interrupt")
                         :before-invoke-restart-fn (lambda ()
                                                     (install-handler
                                                      #'level1-handler))))
                   (quit)))
          (install-handler #'level1-handler))
  #+ecl(warn "SIGINT handling not implemented yet")
  #+ccl(warn "SIGINT handling not implemented yet")
  #-(or sbcl ecl ccl)(error "no SIGINT handling"))

;;;; posix and friends
(defun getargv ()
  #+sbcl sb-ext:*posix-argv*
  #+ecl(ext:command-args)
  #+ccl ccl:*command-line-argument-list*
  #-(or sbcl ecl ccl)(error "no argv"))

(defun getpid ()
  #+sbcl(sb-posix:getpid)
  #+ecl(ext:getpid)
  #+ccl(ccl::getpid)
  #-(or sbcl ecl ccl)(error "no getpid"))

(defun getcwd ()
  #+sbcl(sb-posix:getcwd)
  #+ecl(namestring (ext:getcwd))
  #+ccl(namestring (current-directory))
  #-(or sbcl ecl ccl)(error "no getcwd"))

(defun current-directory ()
  (let ((cwd (getcwd)))
    (if cwd
        (pathname (ensure-trailing-slash cwd))
        (error "Could not get current directory."))))

(defun (setf current-directory) (pathspec)
  (setf *default-pathname-defaults* (truename pathspec))
  #+sbcl(sb-posix:chdir pathspec)
  #+ecl(ext:chdir pathspec)
  #+ccl(ccl::%chdir (namestring pathspec))
  #-(or ecl sbcl ccl)(error "no chdir"))

(defun environment-variable (name)
  #+sbcl(sb-posix:getenv name)
  #+ecl(ext:getenv name)
  #+ccl(ccl:getenv name)
  #-(or sbcl ecl ccl)(error "no getenv"))

(defun mkdir (dir &optional (mode #o755))
  ;; TODO: ensure-directories-exist
  #+sbcl(sb-posix:mkdir dir mode)
  #+ecl(ext:mkdir (ensure-trailing-slash dir) mode)
  #+ccl(ccl::%mkdir dir mode)
  #-(or ecl sbcl ccl)(error "no mkdir"))

(defvar *paktahn-dir* nil)

(defun homedir ()
  (parse-namestring (ensure-trailing-slash (environment-variable "HOME"))))

(defun home-relative (path)
  (if *paktahn-dir*
      (merge-pathnames (parse-namestring path) *paktahn-dir*)
      (merge-pathnames (parse-namestring path) (homedir))))

(defun config-file (path)
  (let ((result (merge-pathnames
                  (parse-namestring path)
                  (home-relative ".paktahn/"))))
    (ensure-directories-exist result)
    result))

(defun tempdir ()
  (let ((dir (or (environment-variable "PAKTAHN_TMPDIR")
                 (environment-variable "TMPDIR")
                 (config-file "aur/"))))
    dir))

(defun getuid ()
  #+sbcl(sb-unix:unix-getuid)
  #+ecl(progn
         (ffi:clines "#include <unistd.h>")
         (ffi:c-inline () () :int "getuid()" :one-liner t))
  #+ccl(ccl::getuid)
  #-(or sbcl ecl ccl)(error "no getuid"))

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
  #+ccl(warn "not implemented yet.")
  #+ecl(ext:run-program (find-in-path program)
                       (ensure-list args)
                       :input t
                       :output (if capture-output-p
                                   :stream
                                   t))
  #-(or sbcl ecl ccl)(error "no run-program"))

;;;; interactive stuff
(defun getline (prompt)
  (finish-output *standard-output*)
  (or (readline (the string prompt)) (quit)))

(defun ask-y/n (question-string &optional (default nil default-supplied-p)
                (options '(#\y #\n) options-supplied-p)
                (clarify-string "Please type \"y\" for yes or \"n\" for no"))
  (cond
    ((null default) (nsubstitute #\N #\n options))
    ((characterp default) (nsubstitute (char-upcase default) default options))
    ((eql t default) (nsubstitute #\Y #\y options)))
  (labels ((maybe-print-query (hint format-string &rest format-args)
             (fresh-line *query-io*)
             (when format-string
               (let ((*info-fmt-suffix* ""))
                 (apply #'info format-string format-args)) ; FIXME: *query-io*
               (write-char #\Space *query-io*)
               (finish-output *query-io*))
             (let ((*info-fmt-prefix* "")
                   (*info-fmt-suffix* ""))
               (info "~A " hint)) ; FIXME: *query-io*
             (finish-output *query-io*))
           (print-query ()
             (maybe-print-query (format nil "(~{~C~^/~})" options)
                                question-string))
           (query-read-char ()
             (clear-input *query-io*)
             (prog1 (read-char *query-io*)
               (clear-input *query-io*)))
           (clarify-legal-query-input ()
             (format *query-io* "~&~a.~:[~; Hit RETURN for the default.~]~%"
                     clarify-string default-supplied-p)))
    (loop (print-query)
       (let ((ch (query-read-char)))
         (cond
           ((member ch '(#\y #\Y)) (return t))
           ((member ch '(#\n #\N)) (return nil))
           ((and default-supplied-p (char-equal ch #\Newline))
            (return default))
           ((and options-supplied-p (member ch options :test #'char-equal))
            (return (char-downcase ch)))
           (t (clarify-legal-query-input)))))))

(defun ask-for-editor ()
  (format t "You do not have your EDITOR environment variable set.~%~
          Please tell me the name of your preferred editor.")
  (loop for input = (getline " => ")
        until input
        finally (return input)))

(defun launch-editor (filename)
  (retrying
    (let* ((editor-spec (or (environment-variable "EDITOR")
                            (ask-for-editor)))
           (editor-spec (split-sequence #\Space editor-spec :remove-empty-subseqs t))
           (editor-bin (car editor-spec))
           (editor-args (cdr editor-spec))
           (return-value (run-program editor-bin (append editor-args (list filename)))))
      (unless (zerop return-value)
        (warn "Editor ~S exited with non-zero status ~D" editor-spec return-value)
        (when (ask-y/n "Choose another editor?" t)
          (retry))))))

(defun download-file (uri)
  (retrying
   (let ((return-value (run-program "wget" (list "-c" uri))))
     (unless (zerop return-value)
       (if (ask-y/n (format nil "Download via wget failed with status ~D. Retry?" return-value))
           (retry)
           (error "Failed to download file ~S" uri)))
     return-value)))

(defun unpack-file (name &optional destination)
  (retrying
    (let ((return-value (if destination
                            (let ((filepath (merge-pathnames
                                             (parse-namestring name)
                                             (current-directory))))
                              (with-tmp-dir (destination (current-directory))
                                (run-program "tar" (list "xfvz" (namestring filepath)))))
                            (run-program "tar" (list "xfvz" name)))))
      (unless (zerop return-value)
        (if (ask-y/n (format nil "Unpacking failed with status ~D. Retry?" return-value))
          (retry)
          (error "Failed to unpack file ~S" name)))
      return-value)))

;;;; configuration
(defun load-config ()
  (let ((conf (config-file "config.lisp")))
    (when (probe-file conf)
      (load conf))))

(defun delete-directory-and-files (dir)
  (let ((result (run-program "rm" (list "-rf" "--" (namestring dir)))))
    (unless (zerop result)
      (cerror "Continue anyway" "Could't remove directory ~S recursively" dir))
    t))

(defun parse-integer-between (s min max)
  (let ((i (handler-case (parse-integer s)
             (parse-error () nil))))
    (and i (>= i min) (<= i max) i)))

;;;; checksumming wrappers
(defun md5sum-file (path)
  (md5:md5sum-file path))

(defun stream->fd (stream)
  (check-type stream stream)
  #+sbcl(sb-sys:fd-stream-fd stream)
  #+ccl(ccl:stream-device stream nil)
;  #+ccl(ccl::ioblock-device (ccl::stream-ioblock stream t))
  #+ecl(ext:file-stream-fd stream)
  #-(or ecl sbcl ccl)(error "no stream->fd"))

(defun lockf (fd)
  (check-type fd integer)
  (assert (>= fd 0))
  #+sbcl(sb-posix:lockf fd sb-posix:f-lock 0)
  #+ccl(#_lockf fd #$F_LOCK 0)
  #+ecl(progn
         (ffi:clines "#include <unistd.h>")
         (ffi:c-inline (fd) (:int) :int "lockf(#0, F_LOCK, 0)" :one-liner t))
  #-(or sbcl ecl ccl)(error "no lockf"))

(defun ulockf (fd)
  (check-type fd integer)
  (assert (>= fd 0))
  #+sbcl(sb-posix:lockf fd sb-posix:f-ulock 0)
  #+ccl(#_lockf fd #$F_ULOCK 0)
  #+ecl(progn
         (ffi:clines "#include <unistd.h>")
         (ffi:c-inline (fd) (:int) :int "lockf(#0, F_ULOCK, 0)" :one-liner t))
  #-(or sbcl ecl ccl)(error "no lockf"))

(defmacro with-locked-open-file ((var filespec &rest open-args)
                                 &body body)
  "Binds var to filespec and passes filespec and open-args to with-open-file.
Once the file is locked with lockf(), the body is executed and the lock is
released. Note that :direction must be set to :io to satisfy lockf() in the
case of reading which necessitates :if-exists :overwrite for with-open-file."
  (let ((stream (gensym))
        (fd (gensym)))
    `(with-open-file (,stream ,filespec
                              ,@open-args)
       (let ((,fd (stream->fd ,stream))
             (,var ,filespec))
         (lockf ,fd)
         (unwind-protect (progn ,@body)
           (ulockf ,fd))))))

(defmacro with-locked-input-file ((var filespec) &body body)
  "Passes var, filespec and body unmodified to with-locked-open-file
along with the :direction :io and :if-exists :overwrite options
which are passed by with-locked-open-file to with-open-file."
  `(with-locked-open-file (,var ,filespec
                                :direction :io
                                :if-exists :overwrite)
     ,@body))

(defmacro with-locked-output-file ((var filespec) &body body)
  "Passes var, filespec and body unmodified to with-locked-open-file
along with the :direction :output, :if-exists :supersede and :if-does-not-exist :create
options which are passed by with-locked-open-file to with-open-file."
  `(with-locked-open-file (,var ,filespec
                                :direction :output
                                :if-exists :supersede
                                :if-does-not-exist :create)
     ,@body))

;; ECL compatibility
(defun find-in-path (program)
  (let ((path (reverse (split-sequence #\: (environment-variable "PATH")))))
    (loop for dir in path do
      (let ((abspath (concatenate 'string (ensure-trailing-slash dir) program)))
        (when (probe-file abspath)
          (return abspath))))))

(defun ensure-trailing-slash (path)
  (if (or (string= "" path)
          (char= (aref path (1- (length path))) #\/))
      path
      (concatenate 'string path "/")))

(defmacro with-interrupts (&body body)
  #+sbcl`(sb-sys:with-interrupts ,@body)
  #+ccl`(ccl:with-interrupts-enabled ,@body)
  #+ecl`(mp:with-interrupts ,@body)
  #+(or sbcl ecl ccl)(error "with-interrupts not implemented yet"))
