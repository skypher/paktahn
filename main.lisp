(in-package :pak)

(declaim (optimize (debug 3) (safety 3) (speed 1) (space 1)))

(defun package-installed-p (pkg-name &optional pkg-version) ; TODO groups
  (map-cached-packages (lambda (db-name pkg)
                         (declare (ignore db-name))
                         (unless (stringp pkg) ; ignore groups
                             (destructuring-bind (name version desc provides) pkg
                               (declare (ignore desc))
                               (when (and (equalp name pkg-name)
                                          (or (null pkg-version)
                                              (equalp pkg-version version)))
                                 (return-from package-installed-p (values version :installed)))
                               (when (member pkg-name provides
                                             :test #'equalp
                                             :key (compose #'first #'parse-dep))
                                 (return-from package-installed-p (values version :provided))))))
                       :db-list (list *local-db*))
  nil)

(defun print-group (id db-name name &key (stream *standard-output*))
  (let ((*standard-output* stream))
    ;; id
    (with-term-colors/id :pkg-result-id
      (format t "~D" id))
    (format t " ")
    ;; db
    (with-term-colors/id (:db db-name)
      (format t "~A/" db-name))
    ;; name
    (with-term-colors/id :pkg-name
      (format t "~A" name))
    (format t " ")
    (with-term-colors/id :pkg-version
      (format t "[group]~%")))) ; TODO: show group members

(defun print-package (id db-name name version description &key (stream *standard-output*) out-of-date-p)
  ;; TODO: votes
  (let ((*standard-output* stream))
    ;; id
    (with-term-colors/id :pkg-result-id
      (format t "~D" id))
    (format t " ")
    ;; db
    (with-term-colors/id (:db db-name)
      (format t "~A/" db-name))
    ;; name
    (with-term-colors/id :pkg-name
      (format t "~A" name))
    ;; version
    (format t " ")
    (with-term-colors/id :pkg-version
      (format t "~A" version))
    ;; installation status
    (let ((installed-version (package-installed-p name)))
      (when installed-version
        (format t " ")
        (cond
          ((version< installed-version version)
           (with-term-colors/id :pkg-old
             (format t "[~A installed]" installed-version)))
          ((version> installed-version version)
           (with-term-colors/id :pkg-result-id
             (format t "[~A installed]" installed-version)))
          (t
           (with-term-colors/id :pkg-installed
             (format t "[installed]"))))))
    ;; out of date? (aur-only)
    (when out-of-date-p
      (format t " ")
      (with-term-colors/id :pkg-outofdate
        (format t "(out of date)")))
    ;; description
    (with-term-colors/id :pkg-description
      (format t "~%    ~A~%" description))))

(defun get-package-results (query &key (quiet t) exact (stream *standard-output*) query-for-providers)
  (declare (string query))
  (let* ((*print-pretty* nil)
         (i 0)
         packages ; (ID REPO NAME)
         (db-pkg-and-grp-fn (lambda (db-name pkg)
                              (etypecase pkg
                                (string
                                  (let ((name pkg))
                                    (when (and (not query-for-providers) ; groups can't be providers
                                               (or (and exact (equalp query name)) ; TODO we can return immediately on an exact result.
                                                   (and (not exact)
                                                        (search query name :test #'equalp))))
                                      (incf i)
                                      (push-end (list i 'group name) packages)
                                      (unless quiet
                                        (print-group i db-name name :stream stream)))))
                                (cons
                                  (destructuring-bind (name version desc provides) pkg
                                    ;; TODO: search in desc, use regex
                                    (when (or (and query-for-providers (member query provides
                                                                               :test #'equalp
                                                                               :key (compose #'first #'parse-dep)))
                                              (and exact (equalp query name)) ; TODO we can return immediately on an exact result.
                                              (and (not exact)
                                                   (or (search query name :test #'equalp)
                                                       (search query desc :test #'equalp))))
                                      (incf i)
                                      (push-end (list i db-name name version) packages)
                                      (unless quiet
                                        (print-package i db-name name version desc :stream stream))))))))
         (aur-pkg-fn (lambda (match)
                       (with-slots (id name version description out-of-date) match
                         (when (or (and exact (equalp query name)) ; TODO we can return immediately on an exact result.
                                   (and (not exact)
                                        (or (search query name :test #'equalp)
                                            (search query description :test #'equalp))))
                           (incf i)
                           (push-end (list i "aur" name version) packages)
                           (unless quiet
                             (print-package i "aur" name version description
                                            :stream stream
                                            :out-of-date-p (equal out-of-date "1"))))))))
    (map-cached-packages db-pkg-and-grp-fn)
    (map-aur-packages aur-pkg-fn query)
    packages))

(defun package-remote-version (pkg-name)
  (let ((result (get-package-results pkg-name :exact t)))
    (if result
      (fourth (car result))
      (loop for repo-pkg-pair in (find-providing-packages pkg-name)
            thereis (package-installed-p (cdr repo-pkg-pair))))))

;;; user interface
#+(or) ; not used right now
(defun valid-range-input-p (s)
  (loop for c across s
        unless (or (digit-char-p c)
                   (member c '(#\, #\Space #\-)))
          return nil
        finally (return t)))

(defun parse-ranges (string start max)
  "Parse comma delimited numeric ranges, returning a list of min/max
pairs as cons cells."
  ;; This function isn't very good. Curiously, using parse-integer
  ;; actually made it harder, I suspect.
  (when (or (>= start (length string))
            (char= #\- (aref string start)))
    (return-from parse-ranges nil))  
  (labels ((clamp (x) (max 0 (min x max)))
           (range (x y) (cons (clamp (min x y)) (clamp (max x y)))))
    (multiple-value-bind (min idx)
        (parse-integer string :junk-allowed t :start start)      
      (cond
        ((null min) nil)
        ((= idx (length string)) (list (range min min)))
        ((or (char= #\, (aref string idx))
             (char= #\  (aref string idx)))
         (list* (range min min)
                (parse-ranges string (1+ idx) max)))
      ((char= #\- (aref string idx))
       (multiple-value-bind (parsed-max idx)
           (parse-integer string :junk-allowed t :start (1+ idx))
         (list* (range min (or parsed-max max))
                (parse-ranges string (1+ idx) max))))
      (t (format t "???~%") nil)))))

(defun expand-ranges (ranges)
  (loop for (min . max) in ranges
        nconcing (loop for i from min upto max collect i)))

(defun extract-ranges (vector rangespec-string)
  (map 'vector (lambda (i) (aref vector i))
       (expand-ranges (parse-ranges rangespec-string 0 (1- (length vector))))))

(defun find-package-by-name (pkg-name)
  ;; TODO multiple hit handling
  (cdr (first (get-package-results pkg-name :exact t))))

(defun install-package (pkg-name &key db-name force)
  "Install package PKG-NAME from AUR or sync databases. PKG-NAME
may also be a group name or the name of a provider package.
Returns T upon successful installation, NIL otherwise."
  (declare (special *root-package*))
  (ensure-initial-cache)
  (let ((db-name (or db-name (first (find-package-by-name pkg-name))))) ; FIXME: show all packages that provide PKG-NAME too (?)
    (labels ((do-install ()
               (cond
                 ((and (package-installed-p pkg-name) (not force))
                  (let ((local-version (package-installed-p pkg-name))
                        (remote-version (package-remote-version pkg-name)))
                    (flet ((force-install ()
                             (setf force t)
                             (do-install)))
                      (cond
                        ((version< local-version remote-version)
                         (when (ask-y/n (format nil "Package ~A is out of date. Upgrade it to version ~A"
                                                pkg-name remote-version)
                                        t)
                           (force-install)))
                        ((and (version= local-version remote-version)
                              (equalp *root-package* pkg-name))
                         (when (ask-y/n (format nil "Package ~A already installed. Reinstall it"
                                                pkg-name)
                                        nil)
                           (force-install)))
                        ((and (version> local-version remote-version))
                         (when (ask-y/n (format nil "Package ~A is more recent than remote version. ~
                                                Downgrade it to version ~A" pkg-name remote-version)
                                        nil)
                           (force-install)))
                        (t
                         (info "Package ~S is already installed." pkg-name)
                         t)))))
                 ((not db-name)
                  (unless (find-providing-packages pkg-name)
                    (restart-case
                        (error "Couldn't find package ~S anywhere" pkg-name)
                      (resync-db ()
                          :report "Resync pacman databases (-Sy) and try again"
                        (run-pacman '("-Sy"))
                        (do-install)))))
                 ((equalp db-name "aur")
                  (install-aur-package pkg-name))
                 ((not (equalp *root-package* pkg-name))
                  (install-binary-package db-name pkg-name :dep-of *root-package*))
                 ((or (eq db-name 'group)
                      (member db-name (mapcar #'car *sync-dbs*) :test #'equalp))
                  (install-binary-package db-name pkg-name :force force))
                 #+(or)
                 (t
                  (error "BUG: missing DO-INSTALL case")))))
      ;; either we're installing a root package and need to set up our
      ;; environment to reflect this, or we're installing a dep and
      ;; should check that the environment has been set up properly.
      (cond
        ;; if the package has a customizepkg definition, build it with
        ;; customizations applied whether it's a dependency or not
        #+(or) ; needs rework
        ((customize-p pkg-name)
         (unwind-protect
              (progn
                (get-pkgbuild pkg-name)
                (setf (current-directory) pkg-name)
                (apply-customizations)
                (run-makepkg)
                (install-pkg-tarball))
           (cleanup-temp-files pkg-name)))
        ;; installing a dep
        ((boundp '*root-package*)
         (assert *root-package*)
         (check-type *root-package* string)
         (retrying
           (restart-case
               (do-install)
             (retry ()
               :report (lambda (s)
                         (format s "Retry installation of package ~S" pkg-name))
               (retry)))))
        ;; installing an explicitly requested package
        (t
         (let ((*root-package* pkg-name))
           (declare (special *root-package*))
           (restart-case
               (do-install)
             (skip-package ()
               :report (lambda (s)
                         (format s "Skip installation of package ~S and continue" *root-package*))
               (values nil 'skipped))
             (checkout-and-skip ()
               :report (lambda (s)
                         (format s "Checkout the PKGBUILD into a subdirectory and skip installation"))
               (get-pkgbuild pkg-name)
               (values nil 'checkout-out-and-skipped))
             (checkout-and-quit ()
               :report (lambda (s)
                         (format s "Checkout the PKGBUILD into a subdirectory and quit"))
               (get-pkgbuild pkg-name)
               (quit)))))))))

(defun search-and-install-packages (query &key query-for-providers)
  (maybe-refresh-cache)
  (when query-for-providers
    (info "Please select a package providing ~S" query))
  (let* ((pkglist (make-string-output-stream))
         (bstream (make-broadcast-stream *standard-output* pkglist))
         (packages (get-package-results query :quiet nil
                                              :query-for-providers query-for-providers
                                              :stream bstream))
         (pkglist (get-output-stream-string pkglist))
         (total (length packages)))
    (if (null packages)
      (info "Sorry, no packages matched ~S~%" query)
      (flet ((print-list ()
               (format *standard-output* "~A" pkglist)) ; TODO: use write
             (make-prompt ()
               (with-term-colors/id :info ; FIXME use INFO
                 (format nil "[1-~D] => " total))))
        (if query-for-providers
          (format t "=>  --------------------------------------------------------------~%~
                     =>  Please choose a package that provides '~A' from this list.~%~
                     =>  Empty line prints the package list again. Hit Ctrl+C to abort.~%~
                     =>  --------------------------------------------------------------~%"
                     query)
          (format t "=>  --------------------------------------------------------------~%~
                     =>  Enter numbers (e.g. '1,2-5,6') of packages to be installed.~%~
                     =>  Empty line prints the package list again. Hit Ctrl+C to abort.~%~
                     =>  --------------------------------------------------------------~%"))
        (let* ((choices (loop for input = (getline (make-prompt))
                              for got-input-p = (and input (not (equal input "")))
                              for ranges = (when got-input-p
                                             (if query-for-providers
                                               (parse-integer-between input 0 total)
                                               (expand-ranges (parse-ranges input 0 total))))
                              until ranges
                              unless got-input-p
                                do (print-list)
                              finally (progn
                                        (add-history input)
                                        (return (ensure-list ranges)))))
               (chosen-packages (remove-if-not (lambda (id)
                                                 (member id choices))
                                               packages :key #'first))
               (chosen-packages (sort chosen-packages #'< :key #'first)))
          ;(format t "chosen: ~S~%" chosen-packages)
          (when query-for-providers
            ;; remove other providers since these usually conflict
            (map nil (compose #'remove-package #'third) (remove (car chosen-packages) packages
                                                                :test #'equalp :key #'first)))
          (mapc (lambda (pkgspec)
                  (funcall #'install-package (third pkgspec)
                           :db-name (second pkgspec)))
                                chosen-packages))))))

(defun remove-package (pkg-name)
  (maybe-refresh-cache)
  (labels ((do-remove ()
             ;; TODO: support removal of group, provides(?)
             (cond
               ((not (package-installed-p pkg-name))
                #+(or)(info "Package ~S is not installed, skipping removal." pkg-name)
                nil)
               (t
                (prog1
                    (run-pacman (list "-R" pkg-name))
                  (maybe-refresh-cache))))))
    (restart-case
        (do-remove)
      (skip-package ()
        :report (lambda (s)
                  (format s "Skip removal of package ~S and continue" pkg-name))
        (values nil 'skipped)))))

(defun display-help ()
  (format t "~
Usage:
  pak QUERY      # search for QUERY
  pak -S PACKAGE # install PACKAGE
  pak -R PACKAGE # remove PACKAGE
  pak -G PACKAGE # download pkgbuild into a new directory named PACKAGE~%"))

(defun main (argv &aux (argc (length argv)))
  "Secondary entry point: process config and command line."
  (load-config)
  (cond
    ((some (lambda (x) (member x '("-h" "--help") :test #'equalp)) argv)
     (display-help))
    ((eql argc 1) 
     (search-and-install-packages (first argv)))
    ((and (>= argc 2) (equal (first argv) "-S"))
     (mapcar #'install-package (cdr argv)))
    ((and (>= argc 2) (equal (first argv) "-R"))
     (mapcar #'remove-package (cdr argv)))
    ((and (>= argc 2) (equal (first argv) "-G"))
     (let ((return-values nil))
       (mapcar (lambda (pkg)
                 (push (get-pkgbuild pkg) return-values)) (cdr argv))
       (loop for result in (reverse return-values) do (info result))))
    (t
     (display-help))))

(defun core-main ()
  "Primary entry point for the binary."
  (setf *on-error* :quit)
  (handler-bind ((error #'default-error-handler))
    (init-alpm)
    (setf *local-db* (init-local-db))
    (setf *sync-dbs* (init-sync-dbs))
    (setf *print-pretty* nil)
    (enable-quit-on-sigint)
    (check-for-customizepkg)
    (let ((argv (cdr (getargv))))
      (restart-case
          (main argv)
        (quit ()
          :report "Quit Paktahn"
          (quit))))))

(defun build-core (&key forkp)
  #+sbcl(progn
          (flet ((dump ()
                   (sb-ext:save-lisp-and-die "paktahn"
                                             :toplevel #'core-main
                                             :executable t
                                             :save-runtime-options t)))
            (if forkp
              (let ((pid (sb-posix:fork)))
                (if (zerop pid)
                  (dump)
                  (progn
                    (format t "INFO: waiting for child to finish building the core...~%")
                    (sb-posix:waitpid pid 0)
                    (format t "INFO: ...done~%"))))
              (dump))))
  #+ccl(ccl:save-application "paktahn"
                             :toplevel-function #'core-main
                             :prepend-kernel t)
  #+ecl(asdf:make-build :paktahn :type :program :monolithic t
                        :prologue-code '(require :asdf)
                        :epilogue-code '(paktahn::core-main))
  #-(or sbcl ecl ccl)(error "don't know how to build a core image"))

