;;; keel-report.el --- See what happened, and why -*- lexical-binding: t; -*-

;; The report buffer, `keel-explain', `keel-list-units' and
;; `keel-rerun-unit'.  All of it reads `keel-last-run', `keel-last-lint'
;; and the effect registry; none of it is needed for boot.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'button)
(require 'tabulated-list)
(require 'keel-plan)
(require 'keel-exec)
(require 'keel-static)
(require 'keel-effects)
(require 'keel-config)

(defvar keel-report-buffer-name "*keel*")

;;;; Source navigation

(defun keel-report-source-string (source)
  "SOURCE plist as \"file:line (heading)\"."
  (if (null source)
      "(no source)"
    (concat (file-name-nondirectory (or (plist-get source :file) "?"))
            (when (plist-get source :line) (format ":%d" (plist-get source :line)))
            (when (plist-get source :heading) (format " (%s)" (plist-get source :heading))))))

(defun keel-report-visit-source (source)
  "Visit the file and line SOURCE names."
  (let ((file (plist-get source :file))
        (line (plist-get source :line)))
    (unless (and file (file-exists-p file))
      (user-error "keel: no source file for this declaration"))
    (pop-to-buffer (find-file-noselect file))
    (when line
      (goto-char (point-min))
      (forward-line (1- line)))))

(define-button-type 'keel-source
  'action (lambda (button) (keel-report-visit-source (button-get button 'source)))
  'follow-link t
  'help-echo "Jump to the declaration")

(define-button-type 'keel-unit
  'action (lambda (button) (keel-explain (button-get button 'unit)))
  'follow-link t
  'help-echo "Explain this unit")

(defun keel-report--insert-source (source)
  "Insert a button for SOURCE."
  (if source
      (insert-text-button (keel-report-source-string source)
                          :type 'keel-source 'source source)
    (insert "(no source)")))

;;;; Describing a result

(defun keel-report-reason-string (result)
  "Human sentence for why RESULT is not ok, or \"\"."
  (let ((d (keel-report-details result)))
    (pcase (keel-report-reason result)
      ('nil "")
      ('unchanged "unchanged since last run")
      ('duplicate-name "declared more than once")
      ('missing-reference (format "refers to undeclared %s" (plist-get d :missing)))
      ('cycle (format "dependency cycle with %s" (plist-get d :members)))
      ('blocked (format "blocked by %s" (plist-get d :blockers)))
      ('requires-blocked (format ":requires package not ok: %s" (plist-get d :blockers)))
      ('requires-missing (format ":requires feature not found: %s" (plist-get d :missing)))
      ('env-missing (format "env unset: %s" (plist-get d :missing)))
      ('executable-missing (format "executable not on PATH: %s" (plist-get d :missing)))
      ('error (format "error: %s" (plist-get d :message)))
      (r (format "%s" r)))))

(defun keel-report--elapsed (result)
  "Elapsed time of RESULT as text."
  (let ((s (and (keel-result-p result) (keel-result-elapsed result))))
    (if s (format "%5.0fms" (* 1000 s)) "      ")))

(defun keel-report--status-face (status)
  (pcase status
    ('ok 'success)
    ('failed 'error)
    ('invalid 'error)
    ('skipped 'warning)
    (_ 'default)))

;;;; The report buffer

(defun keel-report--insert-result (result)
  "Insert one line for RESULT."
  (insert (propertize (format "  %-8s" (keel-report-status result))
                      'face (keel-report--status-face (keel-report-status result)))
          (keel-report--elapsed result) " ")
  (if (eq (keel-report-kind result) 'unit)
      (insert-text-button (symbol-name (keel-report-name result))
                          :type 'keel-unit 'unit (keel-report-name result))
    (insert (symbol-name (keel-report-name result))))
  (let ((why (keel-report-reason-string result)))
    (unless (string-empty-p why) (insert "  " why)))
  (insert "\n"))

(defun keel-report--insert-finding (finding)
  "Insert one lint FINDING."
  (insert (propertize (format "  %-8s" (plist-get finding :level))
                      'face (if (eq (plist-get finding :level) 'warning) 'warning 'shadow))
          (format "%s: %s  " (plist-get finding :unit) (plist-get finding :message)))
  (keel-report--insert-source (plist-get finding :source))
  (insert "\n"))

(defun keel-report-render (run lint)
  "Fill the current buffer with the report for RUN and LINT."
  (let* ((inhibit-read-only t)
         (results (keel-run-results run))
         (problems (seq-remove #'keel-report-ok-p results)))
    (erase-buffer)
    (insert (propertize "keel report" 'face 'bold)
            (format "  %s, %.2fs, %d declarations\n\n"
                    (format-time-string "%H:%M:%S" (keel-run-started run))
                    (keel-run-elapsed run) (length results)))
    (when keel-load-errors
      (insert (propertize "Load errors\n" 'face 'bold))
      (dolist (e keel-load-errors)
        (insert (propertize "  error   " 'face 'error) (plist-get e :message) "  ")
        (keel-report--insert-source (plist-get e :source))
        (insert "\n"))
      (insert "\n"))
    (when problems
      (insert (propertize "Problems\n" 'face 'bold))
      (dolist (r problems)
        (keel-report--insert-result r)
        (insert "           at ")
        (keel-report--insert-source (keel-report-source r))
        (insert "\n"))
      (insert "\n"))
    (dolist (kind '(package unit))
      (insert (propertize (if (eq kind 'package) "Packages\n" "Units\n") 'face 'bold))
      (dolist (r results)
        (when (eq (keel-report-kind r) kind)
          (keel-report--insert-result r)))
      (insert "\n"))
    (when lint
      (insert (propertize "Lint\n" 'face 'bold))
      (mapc #'keel-report--insert-finding lint)
      (insert "\n"))
    (insert "RET on a unit explains it; g refreshes; l lists units; q quits.\n")
    (goto-char (point-min))))

(defvar keel-report-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "g") #'keel-report)
    (define-key map (kbd "l") #'keel-list-units)
    (define-key map (kbd "r") #'keel-reload)
    map))

(define-derived-mode keel-report-mode special-mode "keel-report"
  "Major mode for the keel report buffer.")

;;;###autoload
(defun keel-report ()
  "Show the report of the last boot or reload."
  (interactive)
  (unless keel-last-run (user-error "keel has not run yet"))
  (with-current-buffer (get-buffer-create keel-report-buffer-name)
    (keel-report-mode)
    (keel-report-render keel-last-run keel-last-lint)
    (pop-to-buffer (current-buffer))))

(defun keel-report-show-if-needed ()
  "Show the report when something is not ok, or when asked to always."
  (when (and keel-last-run
             (or keel-show-report-on-startup keel-load-errors
                 (keel-run-problems keel-last-run)))
    (keel-report)))

(add-hook 'keel-after-run-hook #'keel-report-show-if-needed)

;;;; Explain

(defun keel-report--unit-names ()
  (mapcar #'keel-entry-name (keel-declared-units)))

(defun keel-report-read-unit ()
  "Read a declared unit name with completion."
  (intern (completing-read "Unit: " (keel-report--unit-names) nil t
                           (let ((at (get-text-property (point) 'unit)))
                             (and at (symbol-name at))))))

;;;###autoload
(defun keel-explain (name)
  "Explain unit NAME: declaration, plan verdict, effects, lint, source."
  (interactive (list (keel-report-read-unit)))
  (let* ((unit (keel-find-entry (keel-declared-units) name))
         (result (and keel-last-run (keel-run-result keel-last-run name)))
         (effects (keel-effects-of name))
         (lint (seq-filter (lambda (f) (eq (plist-get f :unit) name)) keel-last-lint))
         (dependents (seq-filter (lambda (u) (memq name (plist-get u :after)))
                                 (keel-declared-units))))
    (unless unit (user-error "keel: no unit named %s" name))
    (with-current-buffer (get-buffer-create (format "*keel: %s*" name))
      (let ((inhibit-read-only t))
        (keel-report-mode)
        (erase-buffer)
        (insert (propertize (format "config-unit! %s\n" name) 'face 'bold))
        (insert "  source:     ") (keel-report--insert-source (plist-get unit :source)) (insert "\n")
        (dolist (key '(:after :requires :env :executable :defer))
          (when (plist-get unit key)
            (insert (format "  %-12s%s\n" key (plist-get unit key)))))
        (when dependents
          (insert (format "  %-12s%s\n" "needed by:" (mapcar #'keel-entry-name dependents))))
        (insert "\n")
        (if result
            (progn
              (insert (propertize "Last run\n" 'face 'bold))
              (keel-report--insert-result result))
          (insert "Not in the last run.\n"))
        (insert "\n")
        (insert (propertize (format "Effects (%d)\n" (length effects)) 'face 'bold))
        (dolist (e effects) (insert "  " (keel-effects-describe e) "\n"))
        (insert "\n")
        (when lint
          (insert (propertize "Lint\n" 'face 'bold))
          (mapc #'keel-report--insert-finding lint)
          (insert "\n"))
        (insert (propertize "Body\n" 'face 'bold))
        (dolist (form (plist-get unit :body))
          (insert "  " (pp-to-string form)))
        (goto-char (point-min)))
      (pop-to-buffer (current-buffer)))))

;;;; Rerun

;;;###autoload
(defun keel-rerun-unit (name)
  "Retract unit NAME's effects and run its body again."
  (interactive (list (keel-report-read-unit)))
  (let ((unit (keel-find-entry (keel-declared-units) name)))
    (unless unit (user-error "keel: no unit named %s" name))
    (keel-effects-retract-unit name)
    (let* ((report (keel-report-make :name name :kind 'unit :status 'ok
                                     :source (plist-get unit :source)))
           (result (keel-exec-unit unit report
                                   (and keel-last-run (keel-run-results keel-last-run)))))
      (when keel-last-run
        (setf (keel-run-results keel-last-run)
              (mapcar (lambda (r) (if (eq (keel-report-name r) name) result r))
                      (keel-run-results keel-last-run))))
      (message "[keel] %s: %s %s" name (keel-report-status result)
               (keel-report-reason-string result))
      result)))

;;;; List

(defun keel-list-units--entries ()
  (mapcar
   (lambda (unit)
     (let* ((name (keel-entry-name unit))
            (r (and keel-last-run (keel-run-result keel-last-run name))))
       (list name
             (vector (symbol-name name)
                     (if r (propertize (symbol-name (keel-report-status r))
                                       'face (keel-report--status-face (keel-report-status r)))
                       "")
                     (if r (string-trim (keel-report--elapsed r)) "")
                     (number-to-string (length (keel-effects-of name)))
                     (if r (keel-report-reason-string r) "")
                     (keel-report-source-string (plist-get unit :source))))))
   (keel-declared-units)))

(defvar keel-list-units-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") (lambda () (interactive) (keel-explain (tabulated-list-get-id))))
    (define-key map (kbd "j") (lambda () (interactive)
                                (keel-report-visit-source
                                 (plist-get (keel-find-entry (keel-declared-units)
                                                             (tabulated-list-get-id))
                                            :source))))
    (define-key map (kbd "r") (lambda () (interactive)
                                (keel-rerun-unit (tabulated-list-get-id))
                                (revert-buffer)))
    map))

(define-derived-mode keel-list-units-mode tabulated-list-mode "keel-units"
  "List every declared unit with its last result."
  (setq tabulated-list-format
        [("Unit" 24 t) ("Status" 8 t) ("Time" 7 t) ("Fx" 3 t) ("Why" 40 nil) ("Source" 30 nil)])
  (setq tabulated-list-entries #'keel-list-units--entries)
  (tabulated-list-init-header))

;;;###autoload
(defun keel-list-units ()
  "List declared units: RET explains, j jumps to source, r reruns."
  (interactive)
  (with-current-buffer (get-buffer-create "*keel units*")
    (keel-list-units-mode)
    (tabulated-list-print)
    (pop-to-buffer (current-buffer))))

(provide 'keel-report)
;;; keel-report.el ends here
