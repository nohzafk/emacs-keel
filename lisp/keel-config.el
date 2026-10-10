;;; keel-config.el --- Find, tangle, load, boot, reload -*- lexical-binding: t; -*-

;; The config lives in `keel-config-directory' as config.org (tangled with
;; a provenance marker per block) or config.el.  Loading goes form by form
;; so every declaration captures the file, heading and line it came from.
;; `keel-boot' is the startup entry point; `keel-reload' re-reads the
;; config, retracts what changed, and re-applies only that.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'keel-plan)
(require 'keel-exec)
(require 'keel-static)
(require 'keel-effects)

(defvar org-babel-default-header-args)
(defvar org-babel-tangle-comment-format-beg)
(defvar org-babel-tangle-comment-format-end)
(defvar org-babel-tangle-use-relative-file-links)
(declare-function org-babel-merge-params "ob-core")
(declare-function org-babel-tangle-file "ob-tangle")

;;;; Paths

(defvar keel-config-directory
  (expand-file-name "keel" (or (getenv "XDG_CONFIG_HOME") "~/.config"))
  "Directory holding config.org or config.el, keel.lock and the tangled file.")

(defvar keel-config-org-file nil
  "Literate config: one path or an ordered list of paths.
nil means config.org in `keel-config-directory'.")

(defvar keel-config-file nil
  "Plain config, used when no Org file exists.
nil means config.el in `keel-config-directory'.")

(defvar keel-env-file nil
  "File holding a Lisp list of \"KEY=VALUE\" strings to inject at boot.
nil means $EMACS_KEEL_ENV_FILE, else env in `keel-config-directory'.")

(defun keel-config--org-files ()
  "Org sources that exist, in order."
  (seq-filter #'file-exists-p
              (mapcar #'expand-file-name
                      (let ((v (or keel-config-org-file
                                   (expand-file-name "config.org" keel-config-directory))))
                        (if (listp v) v (list v))))))

(defun keel-config--plain-file ()
  "The plain config path."
  (expand-file-name (or keel-config-file
                        (expand-file-name "config.el" keel-config-directory))))

(defun keel-config--tangled-file ()
  "The shadow file Org sources tangle into."
  (expand-file-name ".config.tangled.el" keel-config-directory))

(defun keel-config--env-file ()
  "The env file path."
  (expand-file-name (or keel-env-file
                        (getenv "EMACS_KEEL_ENV_FILE")
                        (expand-file-name "env" keel-config-directory))))

;;;; Tangle with provenance

(defconst keel-config--marker-beg
  "keel-block-begin file:%file line:%start-line name:%source-name")
(defconst keel-config--marker-end "keel-block-end")
(defconst keel-config--marker-regexp
  "^;;+ keel-block-begin file:\\(.*\\) line:\\([0-9]+\\) name:\\(.*\\)$")

(defun keel-config--tangle-one (source target)
  "Tangle Org SOURCE to TARGET with a provenance marker before each block."
  (require 'ob-tangle)
  (let ((org-babel-default-header-args
         (org-babel-merge-params org-babel-default-header-args
                                 '((:comments . "link"))))
        (org-babel-tangle-comment-format-beg keel-config--marker-beg)
        (org-babel-tangle-comment-format-end keel-config--marker-end)
        (org-babel-tangle-use-relative-file-links nil))
    (org-babel-tangle-file source target "\\`\\(elisp\\|emacs-lisp\\)\\'"))
  target)

(defun keel-config-tangle (sources)
  "Tangle SOURCES in order into the shadow file; return its path."
  (let ((target (keel-config--tangled-file))
        (staging (make-temp-file "keel-tangle" t)))
    (unwind-protect
        (let ((parts (seq-map-indexed
                      (lambda (source i)
                        (keel-config--tangle-one
                         source (expand-file-name (format "%03d.el" i) staging)))
                      sources)))
          (make-directory (file-name-directory target) t)
          (with-temp-file target
            (insert ";; -*- lexical-binding: t; -*-\n")
            (dolist (part parts)
              (when (file-exists-p part)
                (insert-file-contents part)
                (goto-char (point-max))
                (unless (bolp) (insert "\n"))))))
      (delete-directory staging t))
    target))

(defun keel-config--markers ()
  "Provenance markers in the current buffer: (TANGLED-LINE FILE ORG-LINE HEADING)."
  (save-excursion
    (goto-char (point-min))
    (let (markers)
      (while (re-search-forward keel-config--marker-regexp nil t)
        (let ((name (match-string-no-properties 3)))
          (push (list (line-number-at-pos (match-beginning 0))
                      (match-string-no-properties 1)
                      (string-to-number (match-string-no-properties 2))
                      (if (string-match "\\`\\(.*\\):[0-9]+\\'" name)
                          (match-string 1 name)
                        name))
                markers)))
      (nreverse markers))))

(defun keel-config--source-at (markers line file)
  "Provenance plist for tangled LINE given MARKERS, falling back to FILE."
  (let (best)
    (dolist (m markers)
      (when (< (car m) line) (setq best m)))
    (if best
        (pcase-let ((`(,mline ,mfile ,oline ,heading) best))
          (list :file mfile :heading heading
                :line (+ oline (- line mline 1)) :tangled-line line))
      (list :file file :line line))))

;;;; Loading

(defun keel-config-load-file (file)
  "Load FILE form by form, giving each declaration its provenance."
  (with-temp-buffer
    (insert-file-contents file)
    (set-syntax-table emacs-lisp-mode-syntax-table)
    (let* ((markers (keel-config--markers))
           (lexical (save-excursion
                      (goto-char (point-min))
                      (and (re-search-forward "-\\*-.*lexical-binding: *t.*-\\*-"
                                              (line-end-position) t)
                           t)))
           (load-file-name file)
           (default-directory (file-name-directory file)))
      (goto-char (point-min))
      (while (progn (forward-comment (buffer-size)) (not (eobp)))
        (let* ((line (line-number-at-pos (point)))
               (keel--current-source (keel-config--source-at markers line file))
               (form (read (current-buffer))))
          (eval form lexical)))))
  file)

(defun keel-config-load-target ()
  "The file to load: the tangled Org sources, else the plain config.
Signals `keel-config-missing' when neither exists."
  (let ((org (keel-config--org-files))
        (plain (keel-config--plain-file)))
    (cond (org (when (file-exists-p plain)
                 (message "[keel] %s ignored: config.org is present" plain))
               (keel-config-tangle org))
          ((file-exists-p plain) plain)
          (t (signal 'keel-config-missing (list keel-config-directory))))))

(define-error 'keel-config-missing "No config.org or config.el")

(defun keel-config-load-declarations ()
  "Reset the registries and load the config into them.  Return the file loaded."
  (keel-reset-declarations)
  (keel-static-enable)
  (keel-config-load-file (keel-config-load-target)))

;;;; Environment

(defun keel-config-load-env (&optional file)
  "Inject the \"KEY=VALUE\" entries of FILE into the process environment.
Rebuilds `exec-path' from PATH.  Returns the number of entries, or nil
when FILE does not exist."
  (let ((file (or file (keel-config--env-file))))
    (when (file-exists-p file)
      (let ((entries (with-temp-buffer
                       (insert-file-contents file)
                       (read (current-buffer)))))
        (dolist (entry entries)
          (when (string-match "\\`\\([^=]+\\)=\\(.*\\)\\'" entry)
            (setenv (match-string 1 entry) (match-string 2 entry))))
        (let ((path (getenv "PATH")))
          (when path
            (setq exec-path (append (parse-colon-path path) (list exec-directory)))))
        (length entries)))))

;;;; Boot and reload

(defvar keel-last-run nil
  "The `keel-run' of the last boot or reload.")

(defvar keel-last-lint nil
  "Lint findings from the last boot or reload.")

(defvar keel-show-report-on-startup nil
  "Show the report after boot even when nothing failed.")

(defvar keel-after-run-hook nil
  "Run after a boot or reload, with `keel-last-run' set.")

(defun keel-config--announce (run verb)
  "Message a one-line summary of RUN after VERB."
  (let* ((results (keel-run-results run))
         (count (lambda (status)
                  (seq-count (lambda (r) (eq (keel-report-status r) status)) results))))
    (message "[keel] %s: %d ok, %d failed, %d skipped, %d invalid in %.2fs"
             verb (funcall count 'ok) (funcall count 'failed)
             (funcall count 'skipped) (funcall count 'invalid)
             (keel-run-elapsed run))))

(defun keel-config--run (verb)
  "Plan and execute the current declarations; record and announce as VERB."
  (let* ((packages (keel-declared-packages))
         (units (keel-declared-units))
         (plan (keel-plan-compute packages units))
         (run (keel-exec-run plan packages units)))
    (setq keel-last-run run
          keel-last-lint (keel-static-lint units))
    (keel-config--announce run verb)
    (run-hooks 'keel-after-run-hook)
    run))

;;;###autoload
(defun keel-boot ()
  "Load the config, plan it, run it.  Call from init.el."
  (keel-config-load-env)
  (keel-effects-enable)
  (keel-effects-reset)
  (keel-config-load-declarations)
  (let ((keel-exec-unit-filter #'ignore))
    (keel-config--run "boot")))

(defun keel-config--unit-signature (unit)
  "What makes UNIT the same declaration across loads."
  (list (plist-get unit :after) (plist-get unit :requires)
        (plist-get unit :env) (plist-get unit :executable)
        (plist-get unit :defer) (plist-get unit :body)))

(defun keel-config-diff-units (old new)
  "Compare unit lists OLD and NEW.
Returns a plist :unchanged :changed :added :removed of names."
  (let (unchanged changed added removed)
    (dolist (u new)
      (let* ((name (keel-entry-name u))
             (prev (keel-find-entry old name)))
        (cond ((null prev) (push name added))
              ((equal (keel-config--unit-signature prev)
                      (keel-config--unit-signature u))
               (push name unchanged))
              (t (push name changed)))))
    (dolist (u old)
      (unless (keel-find-entry new (keel-entry-name u))
        (push (keel-entry-name u) removed)))
    (list :unchanged (nreverse unchanged) :changed (nreverse changed)
          :added (nreverse added) :removed (nreverse removed))))

;;;###autoload
(defun keel-reload ()
  "Re-read the config and apply what changed.
Changed and removed units have their captured effects retracted first;
unchanged units that ran ok last time are left alone."
  (interactive)
  (let ((old-units (keel-declared-units))
        (old-run keel-last-run))
    (keel-config-load-declarations)
    (let* ((diff (keel-config-diff-units old-units (keel-declared-units)))
           (leave (seq-filter
                   (lambda (name)
                     (and old-run (keel-report-ok-p (keel-run-result old-run name))))
                   (plist-get diff :unchanged))))
      (dolist (name (append (plist-get diff :changed) (plist-get diff :removed)))
        (dolist (record (keel-effects-retract-unit name))
          (message "[keel] retracted %s for %s" (keel-effects-describe record) name)))
      (let ((keel-exec-unit-filter
             (lambda (unit) (and (memq (keel-entry-name unit) leave) 'unchanged))))
        (prog1 (keel-config--run "reload")
          (message "[keel] reload: %d changed, %d added, %d removed, %d unchanged"
                   (length (plist-get diff :changed)) (length (plist-get diff :added))
                   (length (plist-get diff :removed)) (length leave)))))))

(provide 'keel-config)
;;; keel-config.el ends here
