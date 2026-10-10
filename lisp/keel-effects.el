;;; keel-effects.el --- Capture what a unit body does, and undo it -*- lexical-binding: t; -*-

;; Elisp effects are not reified, so no registry can be complete.  This
;; one captures what can be observed from inside the process while a body
;; runs:
;;
;; - `add-hook', `advice-add' and `define-key' (which `keymap-set',
;;   `global-set-key' and friends call) are advised for the dynamic extent
;;   of the body.  Loops and helper functions are covered because capture
;;   is dynamic, not syntactic.
;; - The globals the static pass saw assigned (`setq', `setopt',
;;   `customize-set-variable', ...) get variable watchers, so their first
;;   old value is kept and can be restored.
;;
;; Every record carries a retract thunk.  Retraction runs in reverse
;; application order.  A body that signals has its effects retracted before
;; the error propagates (`keel-effects-transactional').
;;
;; Not captured, by construction: `add-to-list', mode toggles, `defun'
;; redefinitions, timers, faces, and any variable the static pass could not
;; see.  Those re-evaluate on reload as they always did.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'keel-plan)
(require 'keel-exec)

(defvar keel-effects-registry nil
  "Alist unit name -> effect records, newest first.")

(defvar keel-effects-transactional t
  "When non-nil, a unit body that signals has its effects retracted first.")

(defvar keel-effects--unit nil
  "Name of the unit whose body is running, or nil.")

(defvar keel-effects--current nil
  "Records captured for the running unit, newest first.")

(defvar keel-effects--watched nil
  "Symbols already recorded for the running unit's variable watchers.")

(defconst keel-effects--unbound (make-symbol "unbound")
  "Marker for a variable that had no default value.")

;;;; Records

(defun keel-effects-make (kind target retract &rest extra)
  "An effect record of KIND on TARGET undone by thunk RETRACT, plus EXTRA."
  (append (list :kind kind :target target :retract retract :unit keel-effects--unit)
          extra))

(defun keel-effects--record (record)
  "Add RECORD to the running unit's capture."
  (push record keel-effects--current)
  record)

(defun keel-effects-of (unit)
  "Effect records of UNIT, oldest first."
  (reverse (alist-get unit keel-effects-registry)))

(defun keel-effects-describe (record)
  "One line describing RECORD."
  (pcase (plist-get record :kind)
    ('hook (format "hook %s -> %S" (plist-get record :target)
                   (plist-get record :function)))
    ('advice (format "advice %s %s %S" (plist-get record :target)
                     (plist-get record :how) (plist-get record :function)))
    ('key (format "key %s %s -> %S" (plist-get record :map-name)
                  (key-description (plist-get record :key))
                  (plist-get record :binding)))
    ('variable (format "variable %s" (plist-get record :target)))
    (kind (format "%s %s" kind (plist-get record :target)))))

;;;; Capture hooks

(defun keel-effects--around-add-hook (orig hook function &optional depth local)
  "Record the hook that ORIG installs."
  (prog1 (funcall orig hook function depth local)
    (let ((buffer (and local (current-buffer))))
      (keel-effects--record
       (keel-effects-make
        'hook hook
        (lambda ()
          (if buffer
              (when (buffer-live-p buffer)
                (with-current-buffer buffer (remove-hook hook function t)))
            (remove-hook hook function)))
        :function function :local local)))))

(defun keel-effects--around-advice-add (orig symbol how function &optional props)
  "Record the advice that ORIG installs."
  (prog1 (funcall orig symbol how function props)
    (keel-effects--record
     (keel-effects-make
      'advice symbol
      (lambda () (advice-remove symbol function))
      :how how :function function))))

(defvar keel-effects--known-maps nil
  "Keymap variables seen in unit bodies; used to name keymaps in records.")

(defun keel-effects--map-name (keymap)
  "A symbol naming KEYMAP when one is known, else `anonymous'."
  (cond ((eq keymap (current-global-map)) 'global-map)
        (t (or (seq-find (lambda (sym) (and (boundp sym) (eq (symbol-value sym) keymap)))
                         keel-effects--known-maps)
               'anonymous))))

(defun keel-effects--around-define-key (orig keymap key def &optional remove)
  "Record the binding ORIG makes, with the binding it replaces."
  (let ((old (lookup-key keymap key))
        (map-name (keel-effects--map-name keymap)))
    (prog1 (funcall orig keymap key def remove)
      (keel-effects--record
       (keel-effects-make
        'key (cons map-name key)
        (lambda ()
          ;; Only undo a binding that is still ours.
          (when (eq (lookup-key keymap key) def)
            (define-key keymap key (and (not (numberp old)) old))))
        :map-name map-name
        :keymap keymap :key key :binding def :previous old)))))

(defun keel-effects--watch-variable (symbol _newval operation where)
  "Record the first global write to SYMBOL during the running unit."
  (when (and keel-effects--unit
             (null where)
             (memq operation '(set makunbound))
             (not (memq symbol keel-effects--watched)))
    (push symbol keel-effects--watched)
    (let ((old (if (default-boundp symbol)
                   (default-value symbol)
                 keel-effects--unbound)))
      (keel-effects--record
       (keel-effects-make
        'variable symbol
        (lambda ()
          (if (eq old keel-effects--unbound)
              (makunbound symbol)
            (set-default symbol old)))
        :previous old)))))

;;;; Running a unit under capture

(defun keel-effects-retract-records (records)
  "Retract RECORDS, newest first.  Return the records whose retract failed."
  (let (failed)
    (dolist (record records (nreverse failed))
      (condition-case err
          (funcall (plist-get record :retract))
        (error (push (plist-put (copy-sequence record) :error err) failed))))))

(defun keel-effects-run-unit (unit thunk)
  "Run THUNK as UNIT's body, capturing its effects into the registry."
  (let* ((name (keel-entry-name unit))
         (targets (plist-get (plist-get unit :static) :setq))
         (keel-effects--unit name)
         (keel-effects--current nil)
         (keel-effects--watched nil)
         (keel-effects--known-maps
          (append (plist-get (plist-get unit :static) :maps) keel-effects--known-maps))
         (ok nil))
    (dolist (sym targets) (add-variable-watcher sym #'keel-effects--watch-variable))
    (advice-add 'add-hook :around #'keel-effects--around-add-hook)
    (advice-add 'define-key :around #'keel-effects--around-define-key)
    ;; Last in, first out: this one would otherwise record the others.
    (advice-add 'advice-add :around #'keel-effects--around-advice-add)
    (unwind-protect
        (prog1 (funcall thunk)
          (setq ok t))
      (advice-remove 'advice-add #'keel-effects--around-advice-add)
      (advice-remove 'define-key #'keel-effects--around-define-key)
      (advice-remove 'add-hook #'keel-effects--around-add-hook)
      (dolist (sym targets) (remove-variable-watcher sym #'keel-effects--watch-variable))
      (if (or ok (not keel-effects-transactional))
          (setf (alist-get name keel-effects-registry)
                (append keel-effects--current (alist-get name keel-effects-registry)))
        (keel-effects-retract-records keel-effects--current)))))

(defun keel-effects-retract-unit (name)
  "Retract every recorded effect of unit NAME; return the records retracted."
  (let ((records (alist-get name keel-effects-registry)))
    (setf (alist-get name keel-effects-registry nil 'remove) nil)
    (keel-effects-retract-records records)
    records))

(defun keel-effects-reset ()
  "Forget the registry without retracting anything."
  (setq keel-effects-registry nil))

;;;###autoload
(defun keel-effects-enable ()
  "Make the executor run unit bodies under effect capture."
  (setq keel-exec-unit-runner #'keel-effects-run-unit))

(provide 'keel-effects)
;;; keel-effects.el ends here
