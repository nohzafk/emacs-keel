;;; keel-early.el --- Settings Emacs reads before init.el -*- lexical-binding: t; -*-

;; Load this from early-init.el:
;;
;;   (add-to-list 'load-path "~/path/to/keel/lisp")
;;   (require 'keel-early)
;;
;; Emacs activates packages and prepares `user-lisp-directory' between
;; early-init.el and init.el, so what those two steps need cannot wait
;; for `keel-boot'.  This file holds only that; it must stay cheap and
;; must not load the rest of keel.

;;; Code:

(defvar package-user-dir)
(defvar user-lisp-ignored-directories)

;; keel owns package.el's state.  Set here so `package-activate-all' and
;; the startup byte-compile of user-lisp/ see ELPA packages (magit,
;; transient) on `load-path'.  `keel-package--configure' leaves this
;; value alone when it matches.
(setq package-user-dir
      (expand-file-name "keel/packages/" user-emacs-directory))

;; keel links whole git checkouts into user-lisp/.  Their test suites
;; cannot byte-compile at startup (ert fixtures, packages not loaded yet)
;; and are never needed there.  Bound by now: Emacs evaluates delayed
;; customs before early-init.el.
(setq user-lisp-ignored-directories
      (append '("test" "tests" "t") user-lisp-ignored-directories))

;; `prepare-user-lisp' byte-compiles every .el below user-lisp/, which in
;; a linked checkout includes .dir-locals.el: data, not code, so it fails
;; on every start and there is no variable that excludes a file by name.
;; Answer as Emacs does for a file marked `no-byte-compile'.
(defconst keel-early-data-files '(".dir-locals.el" ".dir-locals-2.el")
  "Files under user-lisp/ that are Lisp data, never byte-compiled.")

(define-advice byte-recompile-file (:around (fn file &rest args) keel-early)
  "Skip the Lisp data files in `keel-early-data-files'."
  (if (member (file-name-nondirectory file) keel-early-data-files)
      'no-byte-compile
    (apply fn file args)))

(provide 'keel-early)
;;; keel-early.el ends here
