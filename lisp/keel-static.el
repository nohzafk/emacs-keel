;;; keel-static.el --- What a unit body says before it runs -*- lexical-binding: t; -*-

;; Two sources of knowledge an external planner never had:
;;
;; 1. The body as a tree, at `config-unit!' expansion time.  A walk
;;    collects the globals it assigns, the hooks, advice and keys it
;;    installs, and the mode-ish symbols it touches.  keel-effects uses the
;;    assignment targets to install variable watchers; the lint uses the
;;    rest to compare the body against `:requires'.
;;
;; 2. The byte-compiler.  Compiling the body yields its warnings (free
;;    variables, unknown functions, wrong arity) with no lint of our own.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'bytecomp)
(require 'keel-plan)

;;;; Tree walk

(defun keel-static--quoted (form)
  "The symbol FORM quotes, if FORM is (quote SYM) or (function SYM)."
  (and (consp form)
       (memq (car form) '(quote function))
       (symbolp (cadr form))
       (cadr form)))

(defun keel-static--walk (form acc)
  "Walk FORM, accumulating into ACC, an alist of lists.  Return ACC."
  (cond
   ((symbolp form)
    (when (and form (not (keywordp form)))
      (push form (alist-get :symbols acc)))
    acc)
   ((not (consp form)) acc)
   (t
    (let ((head (car form)))
      (pcase head
        ((or 'quote 'function)
         (when (symbolp (cadr form))
           (push (cadr form) (alist-get :quoted acc)))
         acc)
        ;; Names the body binds itself are not external keymaps.
        ((or 'let 'let* 'dlet 'if-let 'if-let* 'when-let 'when-let* 'while-let)
         (dolist (b (cadr form))
           (let ((var (if (consp b) (car b) b)))
             (when (symbolp var) (push var (alist-get :bound acc)))))
         (keel-static--walk-list (cdr form) acc))
        ((or 'lambda 'defun 'defmacro 'cl-defun 'defsubst)
         (let ((args (if (eq head 'lambda) (cadr form) (caddr form))))
           (dolist (a (if (listp args) args nil))
             (when (and (symbolp a) (not (memq a '(&optional &rest &key &aux))))
               (push a (alist-get :bound acc)))))
         (keel-static--walk-list (cdr form) acc))
        ((or 'defvar 'defvar-local 'defvar-keymap 'defconst 'defcustom)
         (when (symbolp (cadr form))
           (push (cadr form) (alist-get :bound acc)))
         (keel-static--walk-list (cdr form) acc))
        ((or 'setq 'setq-default 'setopt)
         (cl-loop for (var _val) on (cdr form) by #'cddr
                  when (symbolp var) do (push var (alist-get :setq acc)))
         (keel-static--walk-list (cdr form) acc))
        ((or 'customize-set-variable 'set-default 'set)
         (let ((var (keel-static--quoted (cadr form))))
           (when var (push var (alist-get :setq acc))))
         (keel-static--walk-list (cdr form) acc))
        ('add-hook
         (let ((hook (keel-static--quoted (cadr form))))
           (when hook (push hook (alist-get :hooks acc))))
         (keel-static--walk-list (cdr form) acc))
        ('advice-add
         (let ((fn (keel-static--quoted (cadr form))))
           (when fn (push fn (alist-get :advice acc))))
         (keel-static--walk-list (cdr form) acc))
        ((or 'keymap-set 'define-key)
         (let ((map (cadr form)))
           (push (cons (if (symbolp map) map 'anonymous) (caddr form))
                 (alist-get :keys acc)))
         (keel-static--walk-list (cdr form) acc))
        ((or 'global-set-key 'keymap-global-set)
         (push (cons 'global-map (cadr form)) (alist-get :keys acc))
         (keel-static--walk-list (cdr form) acc))
        (_ (keel-static--walk-list form acc)))))))

(defun keel-static--walk-list (forms acc)
  "Walk each of FORMS (a possibly dotted list) into ACC."
  (while (consp forms)
    (setq acc (keel-static--walk (car forms) acc))
    (setq forms (cdr forms)))
  (if forms (keel-static--walk forms acc) acc))

(defun keel-static-analyze (_name body)
  "Analyze BODY, a list of forms, at expansion time.
Returns a plist: :setq :hooks :advice :keys :modes :maps :symbols."
  (let* ((acc (keel-static--walk-list body nil))
         (bound (alist-get :bound acc))
         (all (delete-dups (append (alist-get :symbols acc)
                                   (alist-get :quoted acc))))
         (names (mapcar #'symbol-name all))
         (keys (reverse (alist-get :keys acc))))
    (list :setq (delete-dups (nreverse (alist-get :setq acc)))
          :hooks (delete-dups (nreverse (alist-get :hooks acc)))
          :advice (delete-dups (nreverse (alist-get :advice acc)))
          :keys keys
          :modes (seq-filter (lambda (s) (string-suffix-p "-mode" (symbol-name s))) all)
          :maps (seq-filter (lambda (s)
                              ;; Only maps the body binds keys in, and
                              ;; does not bind as a variable itself.
                              (and (not (eq s 'global-map))
                                   (not (memq s bound))
                                   (seq-some (lambda (k) (eq (car k) s)) keys)))
                            all)
          :symbols names)))

;;;; Findings

(defun keel-static-finding (unit level kind message)
  "A lint finding for UNIT: plist with :unit :level :kind :message :source."
  (list :unit (keel-entry-name unit) :level level :kind kind
        :message message :source (plist-get unit :source)))

(defun keel-static--provided-by-p (feature symbol)
  "Heuristic: FEATURE's name is a prefix of SYMBOL's name."
  (string-prefix-p (symbol-name feature) (symbol-name symbol)))

(defun keel-static-lint-requires (unit)
  "Findings comparing UNIT's `:requires' with what its body touches.

A `foo-mode-map' used as a variable must be bound when the body runs, so
a feature that plausibly defines it belongs in `:requires'.  A required
feature whose name prefixes nothing in the body is probably unused."
  (let* ((static (plist-get unit :static))
         (requires (plist-get unit :requires))
         (symbols (mapcar #'intern (plist-get static :symbols)))
         findings)
    (dolist (map (plist-get static :maps))
      ;; A map that is already bound (preloaded, or from a feature loaded
      ;; earlier) needs no feature.
      (unless (or (boundp map)
                  (seq-some (lambda (f) (keel-static--provided-by-p f map)) requires))
        (push (keel-static-finding
               unit 'warning 'map-without-requires
               (format "%s is used as a keymap but no :requires feature provides it"
                       map))
              findings)))
    (dolist (feature requires)
      (unless (seq-some (lambda (s) (keel-static--provided-by-p feature s)) symbols)
        (push (keel-static-finding
               unit 'info 'unused-requires
               (format ":requires %s but the body never names anything of it"
                       feature))
              findings)))
    (nreverse findings)))

(defun keel-static--warning-kind (message)
  "Classify a byte-compiler warning MESSAGE."
  (cond ((string-match-p "is not known to be defined" message) 'unresolved-function)
        ((string-match-p "free variable" message) 'free-variable)
        ((string-match-p "called with" message) 'wrong-arity)
        ((string-match-p "obsolete" message) 'obsolete)
        (t 'compiler)))

(defun keel-static-lint-compile (unit)
  "Findings from byte-compiling UNIT's body.
Unknown functions are `info': the body may rely on autoloads.  The rest
are `warning'."
  (let ((body (plist-get unit :body))
        findings)
    (let ((byte-compile-log-warning-function
           (lambda (message &optional _pos _fill _level)
             (let ((kind (keel-static--warning-kind message)))
               (push (keel-static-finding
                      unit (if (memq kind '(unresolved-function obsolete)) 'info 'warning)
                      kind (string-trim message))
                     findings))))
          (byte-compile-warnings t)
          (byte-compile-unresolved-functions nil)
          (byte-compile-verbose nil))
      (condition-case err
          (progn
            (byte-compile `(lambda () ,@body))
            ;; `byte-compile' defers this to the end of a file; there is none.
            (byte-compile-warn-about-unresolved-functions))
        (error (push (keel-static-finding
                      unit 'warning 'compile-error (error-message-string err))
                     findings))))
    (nreverse findings)))

(defun keel-static-lint-unit (unit &optional compile)
  "All findings for UNIT.  With COMPILE, include byte-compiler warnings."
  (append (keel-static-lint-requires unit)
          (and compile (keel-static-lint-compile unit))))

(defun keel-static-lint (units &optional compile)
  "Findings for every unit in UNITS."
  (mapcan (lambda (u) (keel-static-lint-unit u compile)) units))

;;;###autoload
(defun keel-static-enable ()
  "Make `config-unit!' analyze bodies as they are declared."
  (setq keel-unit-body-analyzer #'keel-static-analyze))

(provide 'keel-static)
;;; keel-static.el ends here
