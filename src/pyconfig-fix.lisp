(in-package :py-configparser)

;;; kludge to read option names without values
(fmakunbound 'read-option-name)
(defun read-option-name (p s)
  (loop for c = (%read-char s)
        if (or (eql c #\:)
               (eql c #\=)
               (eql c #\Newline))
          do (let ((option-name (finalize-input p)))
               (when (= 0 (length option-name))
                 (error 'parsing-error
                              :text "No option name found.")) ;; No option name found
               (when (eql c #\Newline)
                 (unread-char #\Newline s))
               (return option-name))
        else if (is-whitespace c)
          do (unread-char (expect-one-of s '(#\: #\= #\Newline)
                                         :skip-whitespace t) s)
        else
          do (extend-input p c)))
