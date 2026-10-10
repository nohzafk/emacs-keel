;;; keel-package-test.el --- Tests for keel-package -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'keel-plan)
(require 'keel-exec)
(require 'keel-config)
(require 'keel-package)

(defmacro keel-package-test-with (&rest body)
  "Run BODY with every package.el and keel path in a fresh temp directory."
  `(let* ((root (make-temp-file "keel-package-test" t))
          (user-emacs-directory (file-name-as-directory root))
          (keel-config-directory (expand-file-name "cfg" root))
          (keel-package-lock-file nil)
          (package-user-dir (expand-file-name "keel/packages/" root))
          (package-alist nil)
          (package-activated-list nil)
          (package-archive-contents nil)
          (package-archives nil)
          (package--initialized nil)
          (package-vc-selected-packages nil)
          (user-lisp-directory (expand-file-name "user-lisp/" root))
          (keel-package-ignore-lock nil)
          (keel-package-ignored-locks nil)
          (keel-package-last-install-info nil)
          (keel-package--clone-failures nil)
          (keel-package--archive-refreshed nil)
          (keel-packages nil)
          (keel-units nil))
     (unwind-protect (progn ,@body)
       (delete-directory root t))))

(defun keel-package-test-fake-package (name reqs dir)
  "Add package NAME requiring REQS (symbols) installed in DIR to the alist."
  (push (list name (package-desc-create
                    :name name :version '(1 0)
                    :reqs (mapcar (lambda (r) (list r '(1 0))) reqs)
                    :dir dir))
        package-alist))

;;;; URLs and classification

(ert-deftest keel-package-build-url-shorthand-per-host ()
  (should (equal (keel-package-build-url '(:name magit :repo "magit/magit"))
                 "https://github.com/magit/magit"))
  (should (equal (keel-package-build-url
                  '(:name a :repo "u/a" :host "github"))
                 "https://github.com/u/a"))
  (should (equal (keel-package-build-url
                  '(:name a :repo "u/a" :host "gitlab"))
                 "https://gitlab.com/u/a"))
  (should (equal (keel-package-build-url
                  '(:name a :repo "u/a" :host "codeberg"))
                 "https://codeberg.org/u/a"))
  (should (equal (keel-package-build-url
                  '(:name a :repo "u/a" :host "sourcehut"))
                 "https://git.sr.ht/~u/a"))
  (let ((keel-package-default-host "codeberg"))
    (should (equal (keel-package-build-url '(:name a :repo "u/a"))
                   "https://codeberg.org/u/a"))))

(ert-deftest keel-package-build-url-full-url-and-local ()
  (should (equal (keel-package-build-url
                  '(:name a :repo "https://example.org/x.git" :host "gitlab"))
                 "https://example.org/x.git"))
  (should (equal (keel-package-build-url '(:name a :local "/tmp/a"))
                 "file:///tmp/a"))
  (should (equal (keel-package-build-url '(:name a :repo "/tmp/a"))
                 "file:///tmp/a"))
  (should-not (keel-package-build-url '(:name consult))))

(ert-deftest keel-package-kind-classifies-entries ()
  (should (eq (keel-package-kind '(:name consult)) 'archive))
  (should (eq (keel-package-kind '(:name a :repo "u/a")) 'vc))
  (should (eq (keel-package-kind '(:name a :repo "https://x.org/a.git")) 'vc))
  (should (eq (keel-package-kind '(:name a :local "/tmp/a")) 'local))
  (should (eq (keel-package-kind '(:name a :repo "~/src/a")) 'local))
  (should (eq (keel-package-kind '(:name a :repo "../a")) 'local)))

;;;; Lock file

(ert-deftest keel-package-lock-roundtrip-sorted-and-atomic ()
  (keel-package-test-with
   (should-not (keel-package-lock-read))
   (keel-package-lock-put '(:name "zeta" :kind :vc :rev "z1"))
   (keel-package-lock-put '(:name "alpha" :kind :vc :rev "a1"))
   (keel-package-lock-put '(:name "mid" :kind :archive :version (1 2)))
   (should (equal (mapcar (lambda (e) (plist-get e :name))
                          (keel-package-lock-entries))
                  '("alpha" "mid" "zeta")))
   (should (equal (plist-get (keel-package-lock-read) :schema-version) 1))
   (should (equal (plist-get (keel-package-lock-entry 'alpha) :rev) "a1"))
   (should (equal (plist-get (keel-package-lock-entry "mid") :version) '(1 2)))
   ;; Replace, not duplicate.
   (keel-package-lock-put '(:name "alpha" :kind :vc :rev "a2"))
   (should (= (length (keel-package-lock-entries)) 3))
   (should (equal (plist-get (keel-package-lock-entry 'alpha) :rev) "a2"))
   (keel-package-lock-remove 'mid)
   (should-not (keel-package-lock-entry "mid"))
   ;; The temp file used for the atomic rename is gone.
   (should (equal (directory-files keel-config-directory nil "\\`[^.]")
                  '("keel.lock")))
   (should (equal (directory-files keel-config-directory nil "\\`\\.keel")
                  nil))))

(ert-deftest keel-package-lock-file-override-and-schema-mismatch ()
  (keel-package-test-with
   (let ((keel-package-lock-file (expand-file-name "other.lock" user-emacs-directory)))
     (keel-package-lock-put '(:name "a" :kind :vc :rev "r"))
     (should (file-exists-p keel-package-lock-file))
     (should (keel-package-lock-entry 'a))
     (with-temp-file keel-package-lock-file
       (insert "(:schema-version 99 :entries ((:name \"a\")))"))
     (cl-letf (((symbol-function 'display-warning) #'ignore))
       (should-not (keel-package-lock-read))
       (should-not (keel-package-lock-entry 'a))))))

(ert-deftest keel-package-resolve-rev-precedence ()
  (let ((locked '(:name "a" :kind :vc :rev "lockrev")))
    ;; Declared pin beats the lock.
    (should (equal (keel-package-resolve-rev '(:name a :ref "pin") locked)
                   '("pin" . :pinned)))
    (should (equal (keel-package-resolve-rev '(:name a :tag "v1") locked)
                   '("v1" . :pinned)))
    (should (equal (keel-package-resolve-rev '(:name a :ref "r" :tag "t") nil)
                   '("r" . :pinned)))
    ;; The lock beats nothing.
    (should (equal (keel-package-resolve-rev '(:name a :repo "u/a") locked)
                   '("lockrev" . :hit)))
    ;; Nothing at all, or a lock that is not a VC lock.
    (should (equal (keel-package-resolve-rev '(:name a :repo "u/a") nil)
                   '(nil . :miss)))
    (should (equal (keel-package-resolve-rev
                    '(:name a :repo "u/a") '(:kind :archive :version (1)))
                   '(nil . :miss)))
    ;; Local packages never use the lock.
    (should (equal (keel-package-resolve-rev '(:name a :local "/x") locked)
                   '(nil . :miss)))))

(ert-deftest keel-package-lock-ignore-flag-drives-resolution ()
  (keel-package-test-with
   (keel-package-lock-put '(:name "a" :kind :vc :rev "lockrev"))
   (let ((entry '(:name a :repo "u/a")))
     (should (equal (keel-package--resolved-rev entry) "lockrev"))
     (let ((keel-package-ignore-lock t))
       (should-not (keel-package--resolved-rev entry)))
     (let ((keel-package-ignored-locks '(("a" :rev "lockrev"))))
       (should-not (keel-package--resolved-rev entry)))
     (should (equal (keel-package--resolved-rev '(:name a :ref "p")) "p")))))

(ert-deftest keel-package-clone-command-shape ()
  (keel-package-test-with
   (let ((cmd (keel-package--clone-command
               '(:name a :repo "u/a" :branch "main" :submodules t))))
     (should (equal (seq-take cmd 2) '("git" "clone")))
     (should (member "--depth" cmd))
     (should (member "--recurse-submodules" cmd))
     (should (equal (cadr (member "--branch" cmd)) "main"))
     (should (equal (nth (- (length cmd) 2) cmd) "https://github.com/u/a"))
     (should (string-suffix-p "/keel/sources/a" (car (last cmd)))))
   ;; A pinned revision needs full history.
   (should-not (member "--depth"
                       (keel-package--clone-command
                        '(:name a :repo "u/a" :ref "abc"))))))

;;;; Environment lists

(ert-deftest keel-package-env-list-parsing ()
  (should (equal (keel-package-parse-list "magit, transient ,,vertico")
                 '(magit transient vertico)))
  (should-not (keel-package-parse-list nil))
  (should-not (keel-package-parse-list ""))
  (let ((process-environment
         (list "KEEL_REBUILD_PACKAGES=magit, consult"
               "KEEL_UPGRADE_PACKAGES=all")))
    (should (keel-package--rebuild-requested-p 'magit))
    (should (keel-package--rebuild-requested-p 'consult))
    (should-not (keel-package--rebuild-requested-p 'vertico))
    (should (keel-package--upgrade-requested-p 'anything)))
  (let ((process-environment '("KEEL_UPGRADE_PACKAGES=magit")))
    (should (keel-package--upgrade-requested-p 'magit))
    (should-not (keel-package--upgrade-requested-p 'consult))
    (should-not (keel-package--rebuild-requested-p 'magit))))

;;;; Orphans

(ert-deftest keel-package-orphans-keep-declared-and-requires-closure ()
  (keel-package-test-with
   (let ((dir (lambda (n) (expand-file-name (symbol-name n) package-user-dir))))
     (keel-package-test-fake-package 'declared '(dep-a) (funcall dir 'declared))
     (keel-package-test-fake-package 'dep-a '(dep-b) (funcall dir 'dep-a))
     (keel-package-test-fake-package 'dep-b nil (funcall dir 'dep-b))
     (keel-package-test-fake-package 'stray-z nil (funcall dir 'stray-z))
     (keel-package-test-fake-package 'stray-a '(dep-b) (funcall dir 'stray-a))
     ;; Outside package-user-dir: never an orphan.
     (keel-package-test-fake-package 'builtin nil "/usr/share/elsewhere/builtin")
     (should (equal (keel-package-orphans '(declared)) '(stray-a stray-z)))
     (should (equal (keel-package-orphans '(declared stray-a stray-z)) nil)))))

(ert-deftest keel-package-orphans-include-stale-staging-clones ()
  (keel-package-test-with
   (let ((staging (expand-file-name "keel/sources/" user-emacs-directory)))
     (make-directory (expand-file-name "old-clone" staging) t)
     (make-directory (expand-file-name "kept-clone" staging) t)
     (should (equal (keel-package-orphans '(kept-clone)) '(old-clone))))))

(ert-deftest keel-package-prune-needs-confirmation ()
  (keel-package-test-with
   (let ((clone (expand-file-name "keel/sources/old-clone" user-emacs-directory))
         (asked nil))
     (make-directory clone t)
     (cl-letf (((symbol-function 'yes-or-no-p)
                (lambda (_prompt) (setq asked t) nil))
               ((symbol-function 'package-initialize) #'ignore))
       (should (equal (keel-prune-packages) '(old-clone))))
     (should asked)
     (should (file-directory-p clone))
     (cl-letf (((symbol-function 'yes-or-no-p) (lambda (_prompt) t))
               ((symbol-function 'package-initialize) #'ignore))
       (keel-prune-packages))
     (should-not (file-directory-p clone)))))

;;;; Installer

(ert-deftest keel-package-installer-archive-package-not-found-signals ()
  (keel-package-test-with
   (let ((refreshes 0))
     (cl-letf (((symbol-function 'package-refresh-contents)
                (lambda (&rest _) (cl-incf refreshes))))
       (let ((err (should-error
                   (keel-package-installer
                    '(:name keel-package-test-no-such-package :deps nil)))))
         (should (stringp (error-message-string err)))
         (should (string-match-p "keel-package-test-no-such-package"
                                 (error-message-string err))))
       (should (<= 1 refreshes 2))))))

(ert-deftest keel-package-installer-configures-package-el ()
  (keel-package-test-with
   (cl-letf (((symbol-function 'package-refresh-contents) #'ignore))
     (ignore-errors (keel-package-installer '(:name keel-package-test-none))))
   (should (equal package-user-dir
                  (file-name-as-directory
                   (expand-file-name "keel/packages" user-emacs-directory))))
   (dolist (name '("gnu" "nongnu" "melpa"))
     (should (assoc name package-archives)))))

;; Clones a file:// repository, so no network is involved.
(ert-deftest keel-package-installer-vc-package-links-and-locks ()
  (keel-package-test-with
   (let* ((repo (expand-file-name "origin/" user-emacs-directory))
          (entry (list :name 'vcpkg :repo (concat "file://" repo)))
          (git (lambda (&rest args)
                 (let ((default-directory repo))
                   (apply #'call-process "git" nil nil nil
                          "-c" "user.email=t@example.org" "-c" "user.name=t"
                          args)))))
     (make-directory repo t)
     (with-temp-file (expand-file-name "vcpkg.el" repo)
       (insert "(provide 'vcpkg)"))
     (funcall git "init" "-q")
     (funcall git "add" ".")
     (funcall git "commit" "-q" "-m" "init")
     (cl-letf (((symbol-function 'prepare-user-lisp) #'ignore))
       (should (eq (keel-package-installer entry) 'vcpkg))
       (let ((target (expand-file-name "vcpkg" user-lisp-directory))
             (lock (keel-package-lock-entry 'vcpkg)))
         (should (file-symlink-p target))
         (should (file-exists-p (expand-file-name "vcpkg.el" target)))
         (should (eq (plist-get lock :kind) :vc))
         (should (equal (plist-get lock :rev)
                        (keel-package--clone-head-rev entry)))
         (should (member (file-name-as-directory target) load-path))
         ;; Installing again changes nothing.
         (should (eq (keel-package-installer entry) 'vcpkg))
         (should (equal (keel-package-lock-entry 'vcpkg) lock)))))))

(ert-deftest keel-package-installer-local-links-into-user-lisp ()
  (keel-package-test-with
   (let* ((src (expand-file-name "src/mine/lisp/" user-emacs-directory))
          (entry (list :name 'mine
                       :local (expand-file-name "src/mine" user-emacs-directory)
                       :lisp-dir "lisp")))
     (make-directory src t)
     (with-temp-file (expand-file-name "mine.el" src) (insert "(provide 'mine)"))
     (cl-letf (((symbol-function 'prepare-user-lisp) #'ignore))
       (should (eq (keel-package-installer entry) 'mine)))
     (let ((target (expand-file-name "mine" user-lisp-directory)))
       (should (file-symlink-p target))
       (should (file-exists-p (expand-file-name "mine.el" target))))
     ;; Local packages never touch the lock.
     (should-not (keel-package-lock-entries))
     (should-not (file-exists-p (expand-file-name "keel.lock"
                                                  keel-config-directory))))))

;;;; Enable

(ert-deftest keel-package-enable-is-idempotent ()
  (let ((keel-exec-package-installer #'ignore)
        (keel-exec-event-functions nil))
    (keel-package-enable)
    (keel-package-enable)
    (should (eq keel-exec-package-installer #'keel-package-installer))
    (should (= (length keel-exec-event-functions) 1))
    (should (memq #'keel-package--on-event keel-exec-event-functions))))

(ert-deftest keel-package-start-event-prefetches-ok-vc-packages ()
  (keel-package-test-with
   (let (cloned)
     (eval '(progn (package! consult)
                   (package! alpha :repo "u/alpha")
                   (package! beta :repo "u/beta" :deps (alpha))
                   (package! loc :local "/tmp/loc")
                   (package! bad :repo "u/bad" :deps (nope)))
           t)
     (let ((plan (keel-plan-from-declarations)))
       (cl-letf (((symbol-function 'package-initialize) #'ignore)
                 ((symbol-function 'keel-package--pump-clones)
                  (lambda (entries on-each)
                    (dolist (e entries)
                      (push (keel-entry-name e) cloned)
                      (funcall on-each e (list :error "boom"))))))
         (keel-package--on-event (list :event 'package))
         (should-not cloned)
         (keel-package--on-event (list :event 'start :plan plan))
         (should (equal (nreverse cloned) '(alpha beta)))
         (should (equal (sort (mapcar #'car keel-package--clone-failures)
                             #'string<)
                        '(alpha beta))))
       ;; A failed prefetch surfaces when the installer reaches the package.
       (let ((err (should-error (keel-package--vc-install
                                 (keel-find-entry (keel-declared-packages)
                                                  'alpha)))))
         (should (string-match-p "boom" (error-message-string err))))))))

(provide 'keel-package-test)
;;; keel-package-test.el ends here
