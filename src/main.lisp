(in-package :pak)

(declaim (optimize (debug 3) (safety 3) (speed 1) (space 1)))

(defvar *paktahn-version* "0.9.7.0")
(defvar *pacman-faithful-p* t)

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

(defun print-pkglist (pkglist &key (numbered t)
                       (stream *standard-output*))
  (dolist (pkg pkglist)
    (cond ((= (length pkg) 3) ; the pkg is a group
           (print-group (first pkg) (second pkg) (third pkg)
                        :numbered numbered :stream stream))
          ((> (length pkg) 4) ;; the pkg is a normal package
           (print-package (first pkg) (second pkg) (third pkg)
                          (fourth pkg) (fifth pkg)
                          :out-of-date-p (sixth pkg)
                          :numbered numbered :stream stream)))))

(defun print-group (id db-name name &key (numbered t)
                    (stream *standard-output*))
  (let ((*standard-output* stream))
    ;; id
    (when numbered
      (with-term-colors/id :pkg-result-id
        (format t "~D" id))
      (format t " "))
    ;; db
    (with-term-colors/id (:db db-name)
      (format t "~A/" db-name))
    ;; name
    (with-term-colors/id :pkg-name
      (format t "~A" name))
    (format t " ")
    (with-term-colors/id :pkg-version
      (format t "[group]~%")))) ; TODO: show group members

(defun print-package (id db-name name version description &key out-of-date-p
                      (stream *standard-output*) (numbered t))
  ;; TODO: votes
  (let ((*standard-output* stream))
    ;; id
    (when numbered
      (with-term-colors/id :pkg-result-id
        (format t "~D" id))
      (format t " "))
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

(defun get-package-results (query &key query-for-providers exact (search-aur t))
  (declare (string query))
  (let* ((*print-pretty* nil)
         (i 0)
         packages
         (db-pkg-and-grp-fn (lambda (db-name pkg)
                              (etypecase pkg
                                (string
                                 (let ((name pkg))
                                   (when (and (not query-for-providers) ; groups can't be providers
                                              (or (and exact (equalp query name)) ; TODO we can return immediately on an exact result.
                                                  (and (not exact)
                                                       (search query name :test #'equalp))))
                                     (incf i)
                                     (push-end (list i 'group name) packages))))
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
                                     (push-end (list i db-name name version desc) packages)))))))
         (aur-pkg-fn (lambda (match)
                       (with-slots (id name version description out-of-date) match
                         (when (or (and exact (equalp query name)) ; TODO we can return immediately on an exact result.
                                   (and (not exact)
                                        (or (search query name :test #'equalp)
                                            (search query description :test #'equalp))))
                           (incf i)
                           (push-end (list i "aur" name version description
                                           (equal out-of-date "1")) packages))))))
    (map-cached-packages db-pkg-and-grp-fn)
    (when search-aur
      (map-aur-packages aur-pkg-fn query))
    packages))

(defun package-remote-version (pkg-name &key (search-aur t))
  (let ((result (find-package-by-name pkg-name :search-aur search-aur)))
    (if result
        (third result)
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

(defun find-package-by-name (pkg-name &key (search-aur t))
  ;; TODO multiple hit handling
  (cdr (first (get-package-results pkg-name :exact t :search-aur search-aur))))

(defun versioned-package-p (pkg-name)
  (cl-ppcre:scan "-(git|svn|cvs|hg|bzr|darcs)$" pkg-name))

(defun install-package (pkg-name &key db-name force)
  "Install package PKG-NAME from AUR or sync databases. PKG-NAME
may also be a group name or the name of a provider package.
Returns T upon successful installation, NIL otherwise."
  (declare (special *root-package*))
  (ensure-initial-cache)
   ; FIXME: show all packages that provide PKG-NAME too (?)
  (let ((db-name (or db-name (first (find-package-by-name pkg-name)))))
    (labels ((do-install ()
               (cond
                 ((and (package-installed-p pkg-name)
                       (not force)
                       (not (versioned-package-p pkg-name)))
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
                        (sync-command)
                        (do-install)))))
                 ((and (equalp db-name "aur")
                       (not (equalp *root-package* pkg-name)))
                  (install-aur-package pkg-name :as-dep t))
                 ((and (equalp db-name "aur")
                       (equalp *root-package* pkg-name))
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

(defun search-packages (query)
  (maybe-refresh-cache)
  (let* ((packages (get-package-results (car query))))
    (unless (= (length query) 1)
      (dolist (name (cdr query))
        (let ((results (get-package-results name)))
          (loop for (id repo name version) in packages do
               (unless (member name results :key #'third :test #'equal)
                 (setf packages (remove name packages
                                        :key #'third :test #'equal)))))))
    ;; if we're being faithful to pacman -Ss, let us *really* be faithful.
    (if packages
        (print-pkglist packages :numbered nil)
        (or *pacman-faithful-p*
            (info "Sorry, no packages matched ~S~%" query)))))

(defun search-and-install-packages (query &key query-for-providers)
  (maybe-refresh-cache)
  (when query-for-providers
    (info "Please select a package providing ~S" query))
  (let* ((pkglist (make-string-output-stream))
         (bstream (make-broadcast-stream *standard-output* pkglist))
         (packages (get-package-results query :query-for-providers query-for-providers))
         (total (length packages)))
    (print-pkglist packages :stream bstream)
    (setf pkglist (get-output-stream-string pkglist))
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
            (map nil (compose #'remove-command #'third)
                 (remove (car chosen-packages) packages
                         :test #'equalp :key #'first)))
          (mapc (lambda (pkgspec)
                  (funcall #'install-package (third pkgspec)
                           :db-name (second pkgspec)))
                                chosen-packages))))))

(defun installed-aur-packages ()
  (ensure-initial-cache)
  (loop for pkg-spec in (reverse (gethash "local" *cache-contents*))
     when (and (not (stringp pkg-spec))
               (aur-package-p (first pkg-spec)))
     collect pkg-spec))

(defun upgrade-aur-packages ()
  (let ((ignore-pkgs (get-ignorepkg)))
    (loop for (pkg-name local-version . rest) in (installed-aur-packages) do
         (let ((remote-version (package-remote-version pkg-name)))
           (cond ((null remote-version)
                  (info "~a: Package no longer exists." pkg-name))
                 ((member pkg-name ignore-pkgs :test #'string=)
                  (info "~a: Package will be ignored." pkg-name))
                 ((version= local-version remote-version)
                  (info "~a: Up to date." pkg-name))
                 ((version< local-version remote-version)
                  (prompt-to-upgrade pkg-name local-version remote-version)))))))

(defun prompt-to-upgrade (name local-version remote-version)
  (when (ask-y/n (format nil "Upgrade ~a from ~a to ~a"
                         name local-version remote-version))
    (install-aur-package name)))

(defun display-help ()
  (format t "~
Usage:
  pak QUERY      # search for QUERY and prompt for installation
  pak -Ss QUERY  # search for QUERY
  pak -S PACKAGE # install PACKAGE
  pak -R PACKAGE # remove PACKAGE
  pak -Sy        # sync via pacman -Sy and update the paktahn cache
  pak -Su        # run pacman -Su
  pak -Syu       # run pacman -Syu
  pak -Su --aur  # upgrade AUR packages
  pak -Syu --aur # run pacman -Syu and upgrade AUR packages
  pak -G PACKAGE # download pkgbuild into a new directory named PACKAGE
  pak -Q* args   # run pacman -Q* args
  pak -D* args   # run pacman -D* args
  pak -V         # print paktahn version~%"))

(defun main (argv &aux (argc (length argv)))
  "Secondary entry point: process config and command line."
  (load-config)
  (cond
    ((some (lambda (x) (member x '("-h" "--help") :test #'equalp)) argv)
     (display-help))
    ((and (= argc 1) (equal (first argv) "-V"))
     (info "Paktahn Version ~A~%" *paktahn-version*)
     (run-pacman '("-V")))
    ((and (plusp argc) (equal (subseq (first argv) 0 2) "-R"))
     (remove-command argv))
    ((and (plusp argc) (member (subseq (first argv) 0 2)
                               '("-D" "-Q") :test #'equal))
     (run-pacman argv :sudo-p (string= (subseq (first argv) 0 2) "-D") :force t))
    ((and (= argc 1) (equal (first argv) "-Sy"))
     (sync-command))
    ((and (= argc 1) (equal (first argv) "-Su"))
     (run-pacman '("-Su"))
     (reset-cache))
    ((and (= argc 1) (equal (first argv) "-Syu"))
     (run-pacman '("-Syu"))
     (reset-cache))
    ((= argc 1)
     (search-and-install-packages (first argv)))
    ((and (>= argc 2) (equal (first argv) "-Ss"))
     (search-packages (cdr argv)))
    ((and (>= argc 2) (equal (first argv) "-S"))
     (mapcar #'install-package (cdr argv)))
    ((and (>= argc 2) (and (equal (first argv) "-Su")
                           (equal (second argv) "--aur")))
     (upgrade-aur-packages))
    ((and (>= argc 2) (and (equal (first argv) "-Syu")
                           (equal (second argv) "--aur")))
     (sync-command '("-Syu"))
     (upgrade-aur-packages))
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
    (init-dbs)
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
  #+sbcl
  (progn
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
  #+ccl
  (ccl:save-application "paktahn"
                        :toplevel-function #'core-main
                        :prepend-kernel t)
  #+ecl
  (asdf:make-build :paktahn :type :program :monolithic t
                   :prologue-code '(require :asdf)
                   :epilogue-code '(paktahn::core-main))
  #-(or sbcl ecl ccl)
  (error "don't know how to build a core image"))
