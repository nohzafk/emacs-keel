;;; keel-package.el --- Install declared packages -*- lexical-binding: t; -*-

;; The package layer.  `keel-package-installer' makes one `package!' entry
;; installed and activated: VC packages are cloned into a staging area and
;; linked into `user-lisp-directory' like `:local' ones, archive packages
;; go through `package-install'.  The revision of every installed package
;; is recorded in keel.lock.
;;
;; `keel-package-enable' plugs the installer into `keel-exec' and prefetches
;; all missing clones in parallel when a plan starts.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'package)
(require 'keel-plan)
(require 'keel-exec)
(require 'keel-config)

;; Binding this internal flag around `package-install' silences the
;; missing `lexical-binding' cookie warning for generated package files.
(defvar bytecomp--inhibit-lexical-cookie-warning)

(declare-function prepare-user-lisp "startup")
(defvar user-lisp-directory)

;;;; Options and state

(defgroup keel-package nil
  "Package installation for keel."
  :group 'keel)

(defcustom keel-package-clone-concurrency 8
  "Maximum number of git clones run in parallel."
  :type 'integer
  :group 'keel-package)

(defcustom keel-package-default-host "github"
  "Host assumed for `:repo' shorthand when no `:host' is given."
  :type 'string
  :group 'keel-package)

(defcustom keel-package-lock-file nil
  "Path of the lock file.  nil means keel.lock in `keel-config-directory'."
  :type '(choice (const :tag "Default" nil) file)
  :group 'keel-package)

(defconst keel-package-archives
  '(("gnu"    . "https://elpa.gnu.org/packages/")
    ("nongnu" . "https://elpa.nongnu.org/nongnu/")
    ("melpa"  . "https://melpa.org/packages/"))
  "Archives added to `package-archives' when absent.")

(defconst keel-package-lock-schema-version 1
  "Schema version written to and expected in the lock file.")

(defvar keel-package-ignore-lock nil
  "Non-nil skips lock revision resolution.  Bound during upgrades.")

(defvar keel-package-ignored-locks nil
  "Alist of lock revisions ignored this session.
Each element is (NAME :rev REV :reason REASON): an unpinned package whose
locked revision the remote no longer serves.")

(defvar keel-package-last-install-info nil
  "Alist of package name to (:rev REV :locked HOW) for this session.
HOW is `:pinned' (declared :ref or :tag), `:hit' (lock revision) or `:miss'
\(no lock entry; branch or default HEAD was used).")

(defvar keel-package-last-upgrade-report nil
  "Reports of the latest upgrade, most recent first.
Each is (:name NAME :status S :previous-rev R1 :current-rev R2).")

(defvar keel-package--clone-failures nil
  "Alist of name to reason for prefetch clones that failed.")

(defvar keel-package--archive-refreshed nil
  "Non-nil once the archive index was refreshed on behalf of an install.")

(define-error 'keel-package-stale-lock
  "Locked package revision is no longer available upstream")

;;;; Paths and package.el setup

(defun keel-package--ensure-directory (dir)
  "Create DIR if needed; return it as a directory name."
  (let ((path (file-name-as-directory (expand-file-name dir))))
    (make-directory path t)
    path))

(defun keel-package--state-root ()
  "Directory holding keel's clones and installed packages."
  (keel-package--ensure-directory
   (expand-file-name "keel/" user-emacs-directory)))

(defun keel-package--staging-root ()
  "Directory holding one staging clone per VC package."
  (keel-package--ensure-directory
   (expand-file-name "sources/" (keel-package--state-root))))

(defun keel-package--package-root ()
  "Directory that becomes `package-user-dir'."
  (keel-package--ensure-directory
   (expand-file-name "packages/" (keel-package--state-root))))

(defun keel-package--configure ()
  "Point package.el at keel's state and make sure the archives are set."
  (let ((root (keel-package--package-root)))
    (unless (equal (file-name-as-directory (expand-file-name package-user-dir))
                   root)
      (setq package-user-dir root)
      (setq package--initialized nil)))
  (keel-package--staging-root)
  (dolist (archive keel-package-archives)
    (unless (assoc (car archive) package-archives)
      (setq package-archives (append package-archives (list archive)))))
  :configured)

(defun keel-package--prepare ()
  "Configure package.el and load the installed package set."
  (keel-package--configure)
  (unless package--initialized
    (package-initialize))
  :ready)

;;;; Entries

(defun keel-package--entry (name-or-entry)
  "The declared entry for NAME-OR-ENTRY, a name or an entry plist."
  (if (and (consp name-or-entry) (plist-get name-or-entry :name))
      name-or-entry
    (let ((name (keel--name name-or-entry)))
      (or (keel-find-entry (keel-declared-packages) name)
          (error "Unknown package declaration: %s" name)))))

(defun keel-package--sym (entry)
  "ENTRY's name as a symbol."
  (keel--name (plist-get entry :name)))

(defun keel-package--str (entry)
  "ENTRY's name as a string."
  (symbol-name (keel-package--sym entry)))

(defun keel-package--local-repo-p (repo)
  "Non-nil when REPO is a filesystem path."
  (and (stringp repo)
       (or (string-prefix-p "/" repo)
           (string-prefix-p "~" repo)
           (string-prefix-p "./" repo)
           (string-prefix-p "../" repo))))

(defun keel-package--host-base (host)
  "URL prefix for HOST; unknown hosts mean GitHub."
  (pcase (and host (downcase (format "%s" host)))
    ("github"    "https://github.com/")
    ("gitlab"    "https://gitlab.com/")
    ("codeberg"  "https://codeberg.org/")
    ("sourcehut" "https://git.sr.ht/~")
    (_           "https://github.com/")))

(defun keel-package-build-url (entry)
  "Clone URL of ENTRY, or nil for an archive package."
  (let ((repo (plist-get entry :repo))
        (host (plist-get entry :host))
        (local (plist-get entry :local)))
    (cond
     (local (concat "file://" (expand-file-name local)))
     ((keel-package--local-repo-p repo)
      (concat "file://" (expand-file-name repo)))
     ((and (stringp repo) (string-match-p "://" repo)) repo)
     ((stringp repo)
      (concat (keel-package--host-base (or host keel-package-default-host))
              repo))
     (t nil))))

(defun keel-package-kind (entry)
  "Classify ENTRY: `local', `vc' or `archive'."
  (cond
   ((or (plist-get entry :local)
        (keel-package--local-repo-p (plist-get entry :repo)))
    'local)
   ((keel-package-build-url entry) 'vc)
   (t 'archive)))

(defun keel-package--clone-dir (entry)
  "Directory holding ENTRY's source checkout."
  (if (eq (keel-package-kind entry) 'local)
      (expand-file-name (or (plist-get entry :local) (plist-get entry :repo)))
    (expand-file-name (keel-package--str entry) (keel-package--staging-root))))

(defun keel-package--package-dir (entry)
  "Directory package.el installs ENTRY into."
  (expand-file-name (keel-package--str entry) package-user-dir))

(defun keel-package--user-lisp-dir (entry)
  "Where local ENTRY is deployed under `user-lisp-directory'."
  (unless user-lisp-directory
    (error "`user-lisp-directory' is not set"))
  (expand-file-name (keel-package--str entry) user-lisp-directory))

(defun keel-package--installed-p (entry)
  "Non-nil when ENTRY is already installed."
  (if (eq (keel-package-kind entry) 'archive)
      (package-installed-p (keel-package--sym entry))
    (file-exists-p (keel-package--user-lisp-dir entry))))

(defun keel-package--clone-present-p (entry)
  "Non-nil when ENTRY has a staging clone."
  (file-directory-p (keel-package--clone-dir entry)))

(defun keel-package--delete-path (path)
  "Delete PATH: a file, a directory, or a possibly dangling symlink."
  (cond
   ((file-symlink-p path) (delete-file path))
   ((file-directory-p path) (delete-directory path t))
   ((file-exists-p path) (delete-file path))))

;;;; Environment lists

(defun keel-package-parse-list (value)
  "Split comma-separated VALUE into package name symbols."
  (and value
       (mapcar #'intern
               (split-string value "," t "[ \t\n]+"))))

(defun keel-package--env-member-p (var name)
  "Non-nil when environment variable VAR lists NAME or says `all'."
  (let ((value (getenv var)))
    (and value
         (or (equal (string-trim value) "all")
             (memq name (keel-package-parse-list value)))
         t)))

(defun keel-package--upgrade-requested-p (name)
  "Non-nil when KEEL_UPGRADE_PACKAGES asks to upgrade NAME."
  (keel-package--env-member-p "KEEL_UPGRADE_PACKAGES" name))

(defun keel-package--rebuild-requested-p (name)
  "Non-nil when KEEL_REBUILD_PACKAGES asks to rebuild NAME."
  (keel-package--env-member-p "KEEL_REBUILD_PACKAGES" name))

;;;; Lock file

(defun keel-package--lock-path ()
  "Path of the lock file."
  (or keel-package-lock-file
      (expand-file-name "keel.lock" keel-config-directory)))

(defun keel-package-lock-read ()
  "Return the parsed lock plist, or nil when absent or unreadable.
A lock with another schema version is ignored, so a rewrite never destroys
fields this code does not know."
  (let ((file (keel-package--lock-path)))
    (when (file-readable-p file)
      (condition-case nil
          (with-temp-buffer
            (insert-file-contents file)
            (let* ((data (read (current-buffer)))
                   (version (and (listp data)
                                 (plist-get data :schema-version))))
              (cond
               ((null version) nil)
               ((not (equal version keel-package-lock-schema-version))
                (display-warning
                 'keel
                 (format "Ignoring lock %s: schema version %S, expected %S"
                         file version keel-package-lock-schema-version))
                nil)
               (t data))))
        (error nil)))))

(defun keel-package-lock-entries ()
  "Entries of the lock file."
  (plist-get (keel-package-lock-read) :entries))

(defun keel-package--lock-name= (entry name)
  "Non-nil when lock ENTRY is for package NAME, a symbol or string."
  (equal (format "%s" (plist-get entry :name)) (format "%s" name)))

(defun keel-package-lock-entry (name)
  "The lock entry for package NAME, or nil."
  (seq-find (lambda (e) (keel-package--lock-name= e name))
            (keel-package-lock-entries)))

(defun keel-package--lock-write (entries)
  "Write ENTRIES sorted by name, atomically; return the payload."
  (let* ((file (keel-package--lock-path))
         (directory (file-name-directory file))
         (sorted (sort (copy-sequence entries)
                       (lambda (a b)
                         (string< (format "%s" (plist-get a :name))
                                  (format "%s" (plist-get b :name))))))
         (payload (list :schema-version keel-package-lock-schema-version
                        :entries sorted)))
    (make-directory directory t)
    (let ((temp (make-temp-file (expand-file-name ".keel.lock." directory))))
      (with-temp-file temp
        (let ((print-length nil)
              (print-level nil))
          (insert ";; -*- mode: lisp-data -*-\n")
          (pp payload (current-buffer))))
      (rename-file temp file t))
    payload))

(defun keel-package-lock-put (entry)
  "Insert or replace the lock entry for ENTRY's :name."
  (let ((others (seq-remove
                 (lambda (e) (keel-package--lock-name= e (plist-get entry :name)))
                 (keel-package-lock-entries))))
    (keel-package--lock-write (cons entry others))
    entry))

(defun keel-package-lock-remove (name)
  "Remove the lock entry for package NAME when present."
  (let ((entries (keel-package-lock-entries)))
    (when (seq-find (lambda (e) (keel-package--lock-name= e name)) entries)
      (keel-package--lock-write
       (seq-remove (lambda (e) (keel-package--lock-name= e name)) entries)))))

(defun keel-package--lock-marker (lock-entry)
  "The revision marker of LOCK-ENTRY: :rev for VC, :version for archive."
  (or (plist-get lock-entry :rev) (plist-get lock-entry :version)))

(defun keel-package--declared-pin (entry)
  "The declared :ref or :tag of ENTRY, or nil."
  (or (plist-get entry :ref) (plist-get entry :tag)))

(defun keel-package-resolve-rev (entry locked)
  "Decide the revision to install for ENTRY given lock entry LOCKED.
Returns (REV . HOW).  A declared pin wins (`:pinned'); else the locked VC
revision (`:hit'); else (nil . `:miss'), meaning branch or default HEAD.
Local packages are never locked."
  (let ((pin (keel-package--declared-pin entry)))
    (cond
     (pin (cons pin :pinned))
     ((and (not (eq (keel-package-kind entry) 'local))
           (eq (plist-get locked :kind) :vc)
           (plist-get locked :rev))
      (cons (plist-get locked :rev) :hit))
     (t (cons nil :miss)))))

(defun keel-package--effective-lock (entry)
  "The lock entry that may drive ENTRY's resolution now, or nil."
  (let ((name (keel-package--str entry)))
    (unless (or keel-package-ignore-lock
                (assoc name keel-package-ignored-locks))
      (keel-package-lock-entry name))))

(defun keel-package--resolved-rev (entry)
  "Revision to check out for ENTRY, or nil."
  (car (keel-package-resolve-rev entry (keel-package--effective-lock entry))))

(defun keel-package--locked-how (entry)
  "How ENTRY's revision was decided: `:pinned', `:hit' or `:miss'."
  (cdr (keel-package-resolve-rev entry (keel-package--effective-lock entry))))

(defun keel-package--lock-checkout-p (entry rev)
  "Non-nil when REV for ENTRY came from the lock, not the declaration."
  (and rev
       (not (keel-package--declared-pin entry))
       (equal rev (plist-get (keel-package-lock-entry
                              (keel-package--str entry))
                             :rev))))

(defun keel-package--ignore-stale-lock (entry rev reason)
  "Ignore ENTRY's lock REV for the rest of the session; REASON says why."
  (let ((name (keel-package--str entry))
        (summary (car (split-string (string-trim (or reason ""))
                                    "[\r\n]+" t))))
    (unless (assoc name keel-package-ignored-locks)
      (push (list name :rev rev :reason summary) keel-package-ignored-locks)
      (display-warning
       'keel
       (format "Ignoring stale lock for %s at %s; using declaration target%s"
               name rev (if summary (format " (%s)" summary) ""))))
    :ignored))

(defun keel-package--note-install-info (entry info)
  "Remember INFO as the latest install info of ENTRY."
  (setf (alist-get (keel-package--sym entry) keel-package-last-install-info)
        info))

(defun keel-package--timestamp ()
  "Current UTC time for lock entries."
  (format-time-string "%FT%TZ" nil t))

(defun keel-package--clone-head-rev (entry)
  "The HEAD revision of ENTRY's staging clone, or nil."
  (let ((dir (keel-package--clone-dir entry)))
    (when (file-directory-p (expand-file-name ".git" dir))
      (let ((default-directory dir))
        (with-temp-buffer
          (when (zerop (call-process "git" nil t nil "rev-parse" "HEAD"))
            (string-trim (buffer-string))))))))

(defun keel-package--record-vc-lock (entry)
  "Lock installed VC ENTRY at its clone's HEAD; return the revision."
  (let ((rev (keel-package--clone-head-rev entry)))
    (when rev
      (keel-package-lock-put
       (list :name (keel-package--str entry)
             :kind :vc
             :url (keel-package-build-url entry)
             :branch (plist-get entry :branch)
             :rev rev
             :locked-at (keel-package--timestamp)))
      (keel-package--note-install-info
       entry (list :rev rev :locked (keel-package--locked-how entry))))
    rev))

(defun keel-package--record-archive-lock (entry)
  "Lock installed archive ENTRY at its version; return the version."
  (let* ((desc (cadr (assq (keel-package--sym entry) package-alist)))
         (version (and desc (package-desc-version desc))))
    (when version
      (keel-package-lock-put
       (list :name (keel-package--str entry)
             :kind :archive
             :archive (package-desc-archive desc)
             :version version
             :locked-at (keel-package--timestamp)))
      (keel-package--note-install-info
       entry (list :version version :locked :miss)))
    version))

(defun keel-package--backfill-lock (entry)
  "Lock already-installed ENTRY when it has no lock entry.
Local packages are never locked."
  (unless (or (eq (keel-package-kind entry) 'local)
              (keel-package-lock-entry (keel-package--str entry)))
    (if (eq (keel-package-kind entry) 'vc)
        (keel-package--record-vc-lock entry)
      (keel-package--record-archive-lock entry))))

;;;; Cloning

(defun keel-package--clone-command (entry)
  "The git command line that clones ENTRY."
  (let ((branch (plist-get entry :branch))
        (rev (keel-package--resolved-rev entry)))
    (append (list "git" "clone")
            (unless rev (list "--depth" "1" "--no-single-branch"))
            (when (plist-get entry :submodules) (list "--recurse-submodules"))
            (when branch (list "--branch" branch))
            (list (keel-package-build-url entry)
                  (keel-package--clone-dir entry)))))

(defun keel-package--clone-sync (entry)
  "Clone ENTRY synchronously into its staging directory."
  (let ((buffer (generate-new-buffer
                 (format " *keel-clone-%s*" (keel-package--str entry))))
        (command (keel-package--clone-command entry))
        status output)
    (keel-package--delete-path (keel-package--clone-dir entry))
    (unwind-protect
        (progn
          (setq status (apply #'call-process (car command) nil buffer nil
                              (cdr command)))
          (setq output (with-current-buffer buffer (buffer-string))))
      (kill-buffer buffer))
    (unless (zerop status)
      (error "git clone exited %d: %s" status (string-trim output)))
    :ok))

(defun keel-package--start-clone (entry on-done)
  "Start an async clone of ENTRY; call ON-DONE with :ok or (:error REASON)."
  (let ((buffer (generate-new-buffer
                 (format " *keel-clone-%s*" (keel-package--str entry))))
        (handled nil))
    (make-process
     :name (format "keel-clone-%s" (keel-package--str entry))
     :buffer buffer
     :command (keel-package--clone-command entry)
     :noquery t
     :sentinel
     (lambda (proc _event)
       (when (and (not handled) (memq (process-status proc) '(exit signal)))
         (setq handled t)
         (let ((code (process-exit-status proc))
               (output (if (buffer-live-p buffer)
                           (with-current-buffer buffer (buffer-string))
                         "")))
           (when (buffer-live-p buffer) (kill-buffer buffer))
           (funcall on-done
                    (if (zerop code)
                        :ok
                      (list :error (format "git clone exited %d: %s"
                                           code (string-trim output)))))))))))

(defun keel-package--pump-clones (entries on-each)
  "Clone ENTRIES in parallel; call ON-EACH with (ENTRY STATUS) as each ends.
Blocks until every clone has finished.  STATUS is :ok or (:error REASON)."
  (let ((pending (copy-sequence entries))
        (active 0)
        (limit (max 1 keel-package-clone-concurrency)))
    (cl-labels
        ((spawn-next ()
           (when (and pending (< active limit))
             (let ((entry (pop pending)))
               (cl-incf active)
               (keel-package--start-clone
                entry
                (lambda (status)
                  (cl-decf active)
                  (funcall on-each entry status)
                  (spawn-next)))))))
      (dotimes (_ limit) (spawn-next))
      (while (> active 0)
        (accept-process-output nil 0.1)))))

(defun keel-package--prefetch-entries (plan)
  "Entries of PLAN that need a clone before they can be installed."
  (let ((packages (keel-declared-packages)))
    (seq-filter
     (lambda (entry)
       (and entry
            (eq (keel-package-kind entry) 'vc)
            (not (keel-package--installed-p entry))
            (not (keel-package--clone-present-p entry))
            (not (keel-package--upgrade-requested-p
                  (keel-package--sym entry)))
            (not (keel-package--rebuild-requested-p
                  (keel-package--sym entry)))))
     (mapcar (lambda (name) (keel-find-entry packages name))
             (keel-plan-package-order plan)))))

(defun keel-package-prefetch (plan)
  "Clone the missing VC packages of PLAN in parallel.
Failures are remembered and surface when the installer reaches the package."
  (keel-package--prepare)
  (setq keel-package--clone-failures nil)
  (let ((entries (keel-package--prefetch-entries plan)))
    (when entries
      (message "[keel] cloning %d package%s..."
               (length entries) (if (cdr entries) "s" ""))
      (keel-package--pump-clones
       entries
       (lambda (entry status)
         (unless (eq status :ok)
           (push (cons (keel-package--sym entry) (cadr status))
                 keel-package--clone-failures)))))
    (length entries)))

;;;; Checkout and build

(defun keel-package--checkout-ref (entry)
  "Check out ENTRY's declared pin or locked revision in its clone.
Signals `keel-package-stale-lock' when a locked revision cannot be had."
  (let ((ref (keel-package--resolved-rev entry))
        (dir (keel-package--clone-dir entry)))
    (when ref
      (let* ((buffer (generate-new-buffer
                      (format " *keel-checkout-%s*" (keel-package--str entry))))
             (default-directory dir)
             (status (call-process "git" nil buffer nil "checkout" ref))
             (output (with-current-buffer buffer (buffer-string))))
        (kill-buffer buffer)
        (unless (zerop status)
          (if (keel-package--lock-checkout-p entry ref)
              (signal 'keel-package-stale-lock (list ref output))
            (error "git checkout %s failed: %s" ref output)))))))

(defun keel-package--recover-stale-lock (entry condition)
  "Recover ENTRY after CONDITION, a stale-lock signal: reclone from HEAD."
  (keel-package--ignore-stale-lock entry (cadr condition) (caddr condition))
  (keel-package--clone-sync entry))

(defun keel-package--run-in-clone (entry command label)
  "Run shell COMMAND in ENTRY's clone; LABEL names the step in errors."
  (let* ((default-directory
          (file-name-as-directory (keel-package--clone-dir entry)))
         (buffer (generate-new-buffer
                  (format " *keel-%s-%s*" label (keel-package--str entry))))
         (status (call-process shell-file-name nil buffer nil
                               shell-command-switch command))
         (output (with-current-buffer buffer (buffer-string))))
    (kill-buffer buffer)
    (unless (zerop status)
      (error "%s for %s failed (exit %d): %s"
             label (keel-package--str entry) status (string-trim output)))))

(defun keel-package--prepare-checkout (entry)
  "Initialise submodules and run the :build commands of ENTRY.
Both run in the clone before it is linked into `user-lisp-directory', so
build artifacts are present when the package is activated."
  (when (plist-get entry :submodules)
    (keel-package--run-in-clone
     entry "git submodule update --init --recursive" "submodules"))
  (let ((build (plist-get entry :build)))
    (dolist (command (if (listp build) build (list build)))
      (when (stringp command)
        (keel-package--run-in-clone entry command "build")))))

;;;; Activation

(defun keel-package--activate (entry)
  "Activate ENTRY and put its directories on `load-path'."
  (let ((sym (keel-package--sym entry))
        (dir (keel-package--package-dir entry)))
    (when (package-installed-p sym)
      (ignore-errors (package-activate sym)))
    (when (file-directory-p dir)
      (add-to-list 'load-path (file-name-as-directory dir)))
    (when-let* ((lisp-dir (plist-get entry :lisp-dir))
                (full (expand-file-name lisp-dir dir))
                ((file-directory-p full)))
      (add-to-list 'load-path (file-name-as-directory full)))))

(defun keel-package--note-present (entry)
  "Apply runtime metadata for already-present ENTRY."
  (if (eq (keel-package-kind entry) 'archive)
      (keel-package--activate entry)
    (let ((dir (keel-package--user-lisp-dir entry)))
      (when (file-directory-p dir)
        (add-to-list 'load-path (file-name-as-directory dir))))))

;;;; Local and VC packages

(defun keel-package--install-user-lisp (entry)
  "Deploy ENTRY, a local or VC package, under `user-lisp-directory'.
The link points at the entry's :lisp-dir, or the whole checkout.  A copy is
made where symlinks fail."
  (unless user-lisp-directory
    (error "`user-lisp-directory' is not set"))
  (make-directory user-lisp-directory t)
  (let* ((target (keel-package--user-lisp-dir entry))
         (lisp-dir (plist-get entry :lisp-dir))
         (root (keel-package--clone-dir entry))
         (source (if lisp-dir (expand-file-name lisp-dir root) root)))
    (unless (file-directory-p source)
      (error "Source missing: %s" source))
    (keel-package--delete-path target)
    (condition-case nil
        (make-symbolic-link source target t)
      (error
       (keel-package--delete-path target)
       (copy-directory source target t t)))
    ;; Full prepare: byte-compile and scrape autoloads for the new link.
    ;; JUST-ACTIVATE would skip both and leave the package without autoloads.
    (when (fboundp 'prepare-user-lisp)
      (condition-case err
          (prepare-user-lisp)
        (error (message "[keel] prepare-user-lisp after linking %s: %s"
                        (keel-package--sym entry) (error-message-string err)))))
    (keel-package--note-present entry)))

(defun keel-package--purge-user-lisp (entry)
  "Remove the deployment of ENTRY, never its source."
  (keel-package--delete-path (keel-package--user-lisp-dir entry)))

;;;; VC and archive installs

(defun keel-package--adopt (entry)
  "Adopt ENTRY's staging clone by linking it into `user-lisp-directory'.
The revision is checked out, submodules and :build run, then the clone
\(or its :lisp-dir) is linked and the clone's HEAD is locked."
  (condition-case err
      (keel-package--checkout-ref entry)
    (keel-package-stale-lock
     (keel-package--recover-stale-lock entry err)))
  (keel-package--prepare-checkout entry)
  (keel-package--install-user-lisp entry)
  (keel-package--record-vc-lock entry))

(defun keel-package--vc-install (entry)
  "Install VC ENTRY, cloning first unless a clone exists."
  (let ((failure (assq (keel-package--sym entry) keel-package--clone-failures)))
    (when failure
      (error "%s" (cdr failure))))
  (unless (keel-package--clone-present-p entry)
    (keel-package--clone-sync entry))
  (keel-package--adopt entry))

(defun keel-package--refresh-archives ()
  "Refresh the archive index and note that it happened."
  (setq keel-package--archive-refreshed t)
  (package-refresh-contents))

(defun keel-package--archive-install (entry)
  "Install archive ENTRY with `package-install'.
The archive index is refreshed when empty and once more after a failure,
since a stale index can name a version the server no longer has."
  (let ((sym (keel-package--sym entry)))
    (unless (package-installed-p sym)
      (unless package-archive-contents
        (keel-package--refresh-archives))
      (let ((bytecomp--inhibit-lexical-cookie-warning t))
        (condition-case err
            (package-install sym)
          (error
           (if keel-package--archive-refreshed
               (error "Cannot install %s from archive: %s"
                      sym (error-message-string err))
             (keel-package--refresh-archives)
             (condition-case err2
                 (package-install sym)
               (error
                (error "Cannot install %s from archive: %s"
                       sym (error-message-string err2))))))))
      (keel-package--record-archive-lock entry))
    (keel-package--note-present entry)))

(defun keel-package--install (entry)
  "Install ENTRY by kind; signal on failure."
  (cond
   ((keel-package--installed-p entry)
    (keel-package--note-present entry)
    (keel-package--backfill-lock entry))
   ((eq (keel-package-kind entry) 'local)
    (keel-package--install-user-lisp entry))
   ((eq (keel-package-kind entry) 'vc)
    (keel-package--vc-install entry))
   (t (keel-package--archive-install entry)))
  (keel-package--sym entry))

;;;; Purge and rebuild

(defun keel-package--purge-eln-cache (name)
  "Delete native-compiled files of package NAME from user eln caches."
  (when (and (fboundp 'native-comp-available-p)
             (native-comp-available-p)
             (boundp 'native-comp-eln-load-path))
    (let ((pattern (concat (regexp-quote name)
                           "-[0-9a-f]+-[0-9a-f]+\\.eln\\'")))
      (dolist (dir (symbol-value 'native-comp-eln-load-path))
        (unless (string-match-p "/native-lisp/" dir)
          (when (file-directory-p dir)
            (dolist (subdir (directory-files dir t "^[0-9]"))
              (when (file-directory-p subdir)
                (dolist (file (directory-files subdir t pattern))
                  (ignore-errors (delete-file file)))))))))))

(defun keel-package--unload-features (name)
  "Unload the loaded features that package NAME provided."
  (let ((prefixes (cons (file-name-as-directory
                         (expand-file-name name package-user-dir))
                        (and user-lisp-directory
                             (list (file-name-as-directory
                                    (expand-file-name name
                                                      user-lisp-directory)))))))
    (when (seq-some #'file-directory-p prefixes)
      (dolist (item load-history)
        (let ((file (car item)))
          (when (and file (seq-some (lambda (p) (string-prefix-p p file))
                                    prefixes))
            (let ((feat (cl-loop for x in (cdr item)
                                 when (and (consp x) (eq (car x) 'provide))
                                 return (cdr x))))
              (when (and feat (featurep feat))
                (ignore-errors (unload-feature feat t))))))))))

(defun keel-package--purge (entry)
  "Drop every cached trace of ENTRY: features, package state, files, eln."
  (let* ((sym (keel-package--sym entry))
         (name (symbol-name sym))
         (desc (cadr (assq sym package-alist))))
    (keel-package--unload-features name)
    (when (and desc (package-installed-p sym))
      (ignore-errors (package-delete desc t t)))
    ;; A local package's clone directory is the user's own checkout; only
    ;; its deployment is removed.
    (ignore-errors (keel-package--purge-user-lisp entry))
    (unless (eq (keel-package-kind entry) 'local)
      (dolist (dir (list (keel-package--clone-dir entry)
                         (keel-package--package-dir entry)))
        (ignore-errors (keel-package--delete-path dir))))
    (keel-package--purge-eln-cache name)
    (setq package-alist (assq-delete-all sym package-alist))
    (setq package-activated-list (delq sym package-activated-list))))

(defun keel-package--rebuild (entry)
  "Purge ENTRY, then install it afresh; signal on failure."
  (keel-package--purge entry)
  (message "[keel] rebuilding %s..." (keel-package--str entry))
  (keel-package--install entry))

;;;; Upgrade

(defun keel-package--archive-candidate-p (entry)
  "Non-nil when ENTRY is an unpinned archive package."
  (and (eq (keel-package-kind entry) 'archive)
       (not (keel-package--declared-pin entry))))

(defun keel-package--refresh-error ()
  "Refresh the archive index; nil on success, else the error text."
  (condition-case err
      (progn (package-refresh-contents) nil)
    (error (error-message-string err))))

(defun keel-package--lock-revision (entry)
  "The revision marker now locked for ENTRY."
  (keel-package--lock-marker (keel-package-lock-entry (keel-package--str entry))))

(defun keel-package--upgrade-entry (entry &optional refresh-error)
  "Upgrade ENTRY ignoring the lock; return a report plist.
A declared pin only installs ENTRY if missing.  REFRESH-ERROR, from an
earlier index refresh, fails unpinned archive entries without rebuilding."
  (let* ((name (keel-package--sym entry))
         (previous (keel-package--lock-revision entry))
         (report
          (cond
           ((keel-package--declared-pin entry)
            (message "[keel] %s is pinned by declaration, skipped." name)
            (keel-package--install entry)
            (list :name name :status :pinned
                  :previous-rev previous :current-rev previous))
           ((and refresh-error (keel-package--archive-candidate-p entry))
            (list :name name :status :failed
                  :previous-rev previous :reason refresh-error))
           (t
            (condition-case err
                (let ((keel-package-ignore-lock t))
                  (keel-package--rebuild entry)
                  (let ((current (keel-package--lock-revision entry)))
                    (message "[keel] upgraded %s: %s -> %s"
                             name (or previous "?") (or current "?"))
                    (list :name name :status :ok
                          :previous-rev previous :current-rev current)))
              (error (list :name name :status :failed
                           :previous-rev previous
                           :reason (error-message-string err))))))))
    (push report keel-package-last-upgrade-report)
    report))

;;;; Orphans

(defun keel-package--requires-closure (names)
  "NAMES plus the transitive Package-Requires of installed packages.
Return a hash table keyed by symbol."
  (let ((keep (make-hash-table :test #'eq))
        (worklist (copy-sequence names)))
    (while worklist
      (let ((name (pop worklist)))
        (unless (gethash name keep)
          (puthash name t keep)
          (when-let* ((desc (cadr (assq name package-alist))))
            (dolist (req (package-desc-reqs desc))
              (push (car req) worklist))))))
    keep))

(defun keel-package-orphans (declared-names)
  "Installed package names (symbols) not kept by DECLARED-NAMES.
The kept set is DECLARED-NAMES plus the transitive Package-Requires closure
of installed packages.  Packages in `package-user-dir' and stale staging
clones are both scanned."
  (let ((keep (keel-package--requires-closure declared-names))
        (root (file-name-as-directory (expand-file-name package-user-dir)))
        orphans)
    (dolist (item package-alist)
      (let* ((name (car item))
             (desc (cadr item))
             (dir (and desc (package-desc-dir desc))))
        (when (and dir
                   (string-prefix-p root
                                    (file-name-as-directory
                                     (expand-file-name dir)))
                   (not (gethash name keep)))
          (push name orphans))))
    (let ((staging (expand-file-name "keel/sources/" user-emacs-directory)))
      (when (file-directory-p staging)
        (dolist (dir (directory-files staging nil "\\`[^.]"))
          (let ((name (intern dir)))
            (unless (or (gethash name keep) (memq name orphans))
              (push name orphans))))))
    (sort orphans (lambda (a b) (string< (symbol-name a) (symbol-name b))))))

(defun keel-package--declared-names ()
  "Names of the declared packages."
  (mapcar #'keel-entry-name (keel-declared-packages)))

;;;; Entry points

(defun keel-package-installer (entry)
  "Make package ENTRY installed and activated; signal on failure.
KEEL_UPGRADE_PACKAGES and KEEL_REBUILD_PACKAGES name packages (or `all') to
upgrade or rebuild instead of just installing."
  (keel-package--prepare)
  (let ((name (keel-package--sym entry)))
    (cond
     ((keel-package--upgrade-requested-p name)
      (let ((report (keel-package--upgrade-entry
                     entry
                     (and (keel-package--archive-candidate-p entry)
                          (keel-package--refresh-error)))))
        (when (eq (plist-get report :status) :failed)
          (error "Upgrade of %s failed: %s" name (plist-get report :reason)))))
     ((keel-package--rebuild-requested-p name)
      (keel-package--rebuild entry))
     (t (keel-package--install entry)))
    name))

(defun keel-package--on-event (event)
  "Prefetch clones when EVENT, a `keel-exec' event plist, is `start'."
  (when (eq (plist-get event :event) 'start)
    (keel-package-prefetch (plist-get event :plan))))

;;;###autoload
(defun keel-package-enable ()
  "Make `keel-exec' install packages with this library.
Safe to call more than once."
  (setq keel-exec-package-installer #'keel-package-installer)
  (add-hook 'keel-exec-event-functions #'keel-package--on-event)
  t)

(defun keel-package--read-name (prompt)
  "Read a declared package name with PROMPT."
  (intern (completing-read
           prompt (mapcar #'symbol-name (keel-package--declared-names)) nil t)))

;;;###autoload
(defun keel-rebuild-package (name)
  "Rebuild declared package NAME from scratch and reload its feature."
  (interactive (list (keel-package--read-name "Rebuild package: ")))
  (let ((entry (keel-package--entry name)))
    (keel-package--prepare)
    (keel-package--rebuild entry)
    (require (keel-package--sym entry) nil t)
    (message "[keel] %s rebuilt and reloaded." (keel-package--sym entry))
    (keel-package--sym entry)))

;;;###autoload
(defun keel-upgrade-package (name)
  "Upgrade declared package NAME to its declaration target, ignoring the lock."
  (interactive (list (keel-package--read-name "Upgrade package: ")))
  (let* ((entry (keel-package--entry name))
         (report (progn
                   (keel-package--prepare)
                   (keel-package--upgrade-entry
                    entry
                    (and (keel-package--archive-candidate-p entry)
                         (keel-package--refresh-error))))))
    (when (eq (plist-get report :status) :failed)
      (error "Package %s upgrade failed: %s" name (plist-get report :reason)))
    report))

;;;###autoload
(defun keel-upgrade-all-packages ()
  "Upgrade every declared package to its declaration target.
The archive index is refreshed once; a failed refresh fails only archive
packages.  One package failing never stops the rest."
  (interactive)
  (keel-package--prepare)
  (setq keel-package-last-upgrade-report nil)
  (let* ((entries (keel-declared-packages))
         (refresh-error (and (seq-some #'keel-package--archive-candidate-p
                                       entries)
                             (keel-package--refresh-error)))
         (reports (mapcar (lambda (e)
                            (keel-package--upgrade-entry e refresh-error))
                          entries)))
    (cl-flet ((count (status)
                (cl-count status reports
                          :key (lambda (r) (plist-get r :status)))))
      (message "[keel] upgrade finished: %d ok, %d pinned, %d failed."
               (count :ok) (count :pinned) (count :failed)))
    reports))

;;;###autoload
(defun keel-prune-packages ()
  "Remove installed packages that are no longer declared.
Declared packages and their Package-Requires closure are kept.  Nothing is
deleted unless the user confirms."
  (interactive)
  (keel-package--prepare)
  (let ((orphans (keel-package-orphans (keel-package--declared-names))))
    (if (null orphans)
        (message "[keel] no orphaned packages.")
      (when (yes-or-no-p
             (format "Prune %d orphaned package%s (%s)? "
                     (length orphans) (if (cdr orphans) "s" "")
                     (mapconcat #'symbol-name orphans ", ")))
        (let (failures)
          (dolist (name orphans)
            (condition-case err
                (progn
                  (keel-package--purge
                   (or (keel-find-entry (keel-declared-packages) name)
                       (list :name name)))
                  (keel-package-lock-remove name))
              (error (push (cons name (error-message-string err)) failures))))
          (if failures
              (message "[keel] pruned %d of %d; failed: %s"
                       (- (length orphans) (length failures))
                       (length orphans)
                       (mapconcat (lambda (f) (format "%s (%s)" (car f) (cdr f)))
                                  (nreverse failures) ", "))
            (message "[keel] pruned %d package%s."
                     (length orphans) (if (cdr orphans) "s" ""))))))
    orphans))

(provide 'keel-package)
;;; keel-package.el ends here
