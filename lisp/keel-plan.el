;;; keel-plan.el --- Declarations, graph, preflight, plan -*- lexical-binding: t; -*-

;; This file is the pure half of keel.  It turns declarations into a plan
;; and performs no side effects beyond pushing onto the declaration
;; registries.  Everything that probes the environment goes through
;; `keel-plan-probes' so batch checks and tests can substitute their own.

;;; Code:

(require 'cl-lib)
(require 'seq)

;;;; Declarations

(defvar keel-packages nil
  "Declared packages, newest first.  Each is a plist with at least :name.")

(defvar keel-units nil
  "Declared config units, newest first.  Each is a plist with at least :name.")

(defvar keel--current-source nil
  "Provenance plist (:file :heading :line) bound by the loader around each form.")

(defvar keel-unit-body-analyzer nil
  "Function called as (NAME BODY-FORMS) at `config-unit!' expansion time.
Its result is stored under :static in the unit.  nil disables analysis.")

(defconst keel--package-keys
  '(:repo :host :branch :tag :ref :deps :local :lisp-dir :submodules :build)
  "Keys `package!' accepts.")

(defconst keel--unit-keys
  '(:after :requires :env :executable :defer :config)
  "Keys `config-unit!' accepts.")

(defun keel-reset-declarations ()
  "Forget every declaration."
  (setq keel-packages nil
        keel-units nil))

(defun keel--name (value)
  "Return VALUE as a symbol."
  (cond ((symbolp value) value)
        ((stringp value) (intern value))
        (t (error "keel: not a name: %S" value))))

(defun keel--names (value)
  "Return VALUE, a name or list of names, as a list of symbols."
  (cond ((null value) nil)
        ((listp value) (mapcar #'keel--name value))
        (t (list (keel--name value)))))

(defun keel--strings (value)
  "Return VALUE, a name or list of names, as a list of strings."
  (mapcar #'symbol-name (keel--names value)))

(defun keel--capture-source ()
  "Provenance for the form being expanded now."
  (or keel--current-source
      (and load-file-name (list :file load-file-name))))

(defun keel--check-keys (macro name args allowed)
  "Signal if plist ARGS of MACRO NAME has a key outside ALLOWED."
  (cl-loop for (key _val) on args by #'cddr
           unless (memq key allowed)
           do (error "%s %s: unknown key %S" macro name key)))

(defun keel-declare-package (entry)
  "Register package plist ENTRY."
  (push entry keel-packages)
  entry)

(defun keel-declare-unit (entry)
  "Register config-unit plist ENTRY."
  (push entry keel-units)
  entry)

;;;###autoload
(defmacro package! (name &rest args)
  "Declare package NAME.

ARGS is a plist.  `:deps' lists packages that must be installed first;
`:repo', `:host', `:branch', `:tag', `:ref', `:local', `:lisp-dir',
`:submodules' and `:build' describe where the package comes from and are
evaluated.  The declaration records where it was written."
  (declare (indent defun))
  (let ((name (keel--name name)))
    (keel--check-keys 'package! name args keel--package-keys)
    `(keel-declare-package
      (list :name ',name
            :deps ',(keel--names (plist-get args :deps))
            :source ',(keel--capture-source)
            ,@(cl-loop for (key val) on args by #'cddr
                       unless (eq key :deps)
                       append (list key val))))))

;;;###autoload
(defmacro config-unit! (name &rest args)
  "Declare config unit NAME.

ARGS is a plist ending in `:config' followed by the body.  Keys:
`:after'      units that must complete before this one runs
`:requires'   features to load before the body
`:env'        environment variables that must be set, else the unit is skipped
`:executable' binaries that must be on PATH, else the unit is skipped
`:defer'      a feature; the body runs when it loads instead of at startup

The body is kept both as data (for analysis) and as a closure (for
execution)."
  (declare (indent defun))
  (let* ((name (keel--name name))
         (idx (cl-position :config args)))
    (unless idx
      (error "config-unit! %s: missing :config" name))
    (let ((opts (seq-subseq args 0 idx))
          (body (seq-subseq args (1+ idx))))
      (keel--check-keys 'config-unit! name opts keel--unit-keys)
      `(keel-declare-unit
        (list :name ',name
              :after ',(keel--names (plist-get opts :after))
              :requires ',(keel--names (plist-get opts :requires))
              :env ',(keel--strings (plist-get opts :env))
              :executable ',(keel--strings (plist-get opts :executable))
              :defer ',(and (plist-get opts :defer)
                            (keel--name (plist-get opts :defer)))
              :body ',body
              :thunk (lambda () ,@body)
              :static ',(and keel-unit-body-analyzer
                             (funcall keel-unit-body-analyzer name body))
              :source ',(keel--capture-source))))))

(defun keel-declared-packages ()
  "Declared packages in declaration order."
  (reverse keel-packages))

(defun keel-declared-units ()
  "Declared units in declaration order."
  (reverse keel-units))

(defun keel-entry-name (entry)
  "Name of declaration ENTRY."
  (plist-get entry :name))

(defun keel-find-entry (entries name)
  "The entry named NAME in ENTRIES, or nil."
  (seq-find (lambda (e) (eq (keel-entry-name e) name)) entries))

;;;; Reports

(cl-defstruct (keel-report (:constructor keel-report-make))
  "The planner's verdict on one declaration.
STATUS is `ok', `invalid' or `skipped'.  REASON is a symbol; DETAILS a plist."
  name kind status reason details source)

(defun keel-report-ok-p (report)
  "Non-nil when REPORT is ok."
  (and report (eq (keel-report-status report) 'ok)))

(defun keel-find-report (reports name &optional kind)
  "The report named NAME in REPORTS, or nil.
With KIND, only a report of that kind: a package and a unit may share a
name."
  (seq-find (lambda (r)
              (and (eq (keel-report-name r) name)
                   (or (null kind) (eq (keel-report-kind r) kind))))
            reports))

;;;; Graph resolution

(defun keel--duplicate-names (entries)
  "Names declared more than once in ENTRIES."
  (let (seen dups)
    (dolist (e entries (nreverse dups))
      (let ((n (keel-entry-name e)))
        (if (memq n seen)
            (cl-pushnew n dups)
          (push n seen))))))

(defun keel--reaches-p (from to edges)
  "Non-nil when TO is reachable from FROM in EDGES, an alist name -> deps."
  (let ((stack (copy-sequence (alist-get from edges)))
        seen found)
    (while (and stack (not found))
      (let ((n (pop stack)))
        (cond ((eq n to) (setq found t))
              ((memq n seen))
              (t (push n seen)
                 (setq stack (append (alist-get n edges) stack))))))
    found))

(defun keel--cycle-members (entries dep-key)
  "Names in ENTRIES that lie on a cycle through DEP-KEY edges."
  (let ((edges (mapcar (lambda (e)
                         (cons (keel-entry-name e) (plist-get e dep-key)))
                       entries)))
    (seq-filter (lambda (n) (keel--reaches-p n n edges))
                (mapcar #'car edges))))

(defun keel--scc-of (name members edges)
  "The members of MEMBERS that are on a cycle with NAME in EDGES."
  (seq-filter (lambda (m)
                (or (eq m name)
                    (and (keel--reaches-p name m edges)
                         (keel--reaches-p m name edges))))
              members))

(defun keel--resolve (entries dep-key kind check)
  "Validate ENTRIES, a list of plists linked by DEP-KEY, and order them.

KIND tags the reports.  CHECK is called with an entry that passed
validation and whose dependencies are all ok; it returns nil, or
\(REASON . DETAILS) to skip the entry.

Returns (REPORTS . ORDER): REPORTS in declaration order, ORDER the names
of ok entries in execution order."
  (let* ((dups (keel--duplicate-names entries))
         (known (mapcar #'keel-entry-name entries))
         decided candidates)
    ;; Structural validation: duplicates and dangling references.
    (dolist (e entries)
      (let* ((n (keel-entry-name e))
             (missing (seq-remove (lambda (d) (memq d known))
                                  (plist-get e dep-key))))
        (cond
         ((memq n dups)
          (push (keel-report-make
                 :name n :kind kind :status 'invalid :reason 'duplicate-name
                 :source (plist-get e :source))
                decided))
         (missing
          (push (keel-report-make
                 :name n :kind kind :status 'invalid :reason 'missing-reference
                 :details (list :missing missing)
                 :source (plist-get e :source))
                decided))
         (t (push e candidates)))))
    (setq candidates (nreverse candidates))
    ;; Cycles among the candidates.
    (let* ((cyclic (keel--cycle-members candidates dep-key))
           (edges (mapcar (lambda (e)
                            (cons (keel-entry-name e) (plist-get e dep-key)))
                          candidates)))
      (dolist (e candidates)
        (let ((n (keel-entry-name e)))
          (when (memq n cyclic)
            (push (keel-report-make
                   :name n :kind kind :status 'invalid :reason 'cycle
                   :details (list :members (keel--scc-of n cyclic edges))
                   :source (plist-get e :source))
                  decided))))
      (setq candidates (seq-remove (lambda (e) (memq (keel-entry-name e) cyclic))
                                   candidates)))
    ;; Topological walk in declaration order.  Every remaining entry is
    ;; acyclic, so a ready one always exists.
    (let (order)
      (while candidates
        (let* ((ready (seq-find
                       (lambda (e)
                         (seq-every-p (lambda (d) (keel-find-report decided d))
                                      (plist-get e dep-key)))
                       candidates))
               (n (keel-entry-name ready))
               (blockers (seq-remove
                          (lambda (d) (keel-report-ok-p (keel-find-report decided d)))
                          (plist-get ready dep-key)))
               (skip (and (not blockers) (funcall check ready))))
          (setq candidates (delq ready candidates))
          (push (cond
                 (blockers
                  (keel-report-make
                   :name n :kind kind :status 'skipped :reason 'blocked
                   :details (list :blockers blockers)
                   :source (plist-get ready :source)))
                 (skip
                  (keel-report-make
                   :name n :kind kind :status 'skipped :reason (car skip)
                   :details (cdr skip)
                   :source (plist-get ready :source)))
                 (t
                  (push n order)
                  (keel-report-make
                   :name n :kind kind :status 'ok
                   :source (plist-get ready :source))))
                decided)))
      (cons (mapcar (lambda (e) (keel-find-report decided (keel-entry-name e)))
                    entries)
            (nreverse order)))))

;;;; Preflight probes

(defun keel--default-feature-available-p (feature)
  "Non-nil when FEATURE is loaded or can be found on `load-path'."
  (or (featurep feature)
      (locate-library (symbol-name feature))))

(defvar keel-plan-probes
  (list :getenv #'getenv
        :executable-find #'executable-find
        :feature-available-p #'keel--default-feature-available-p)
  "Functions the planner uses to look at the environment.
Rebind to plan against a different environment, as `keel check' and the
tests do.")

(defun keel--probe (key &rest args)
  "Apply probe KEY from `keel-plan-probes' to ARGS."
  (apply (plist-get keel-plan-probes key) args))

(defun keel--unit-check (package-reports)
  "Return the preflight CHECK for units given PACKAGE-REPORTS."
  (lambda (unit)
    (let (blocked missing)
      (dolist (feature (plist-get unit :requires))
        (let ((pkg (keel-find-report package-reports feature)))
          (cond ((and pkg (not (keel-report-ok-p pkg))) (push feature blocked))
                (pkg)
                ((not (keel--probe :feature-available-p feature))
                 (push feature missing)))))
      (let ((env (seq-remove (lambda (v)
                               (let ((val (keel--probe :getenv v)))
                                 (and val (not (string-empty-p val)))))
                             (plist-get unit :env)))
            (exe (seq-remove (lambda (b) (keel--probe :executable-find b))
                             (plist-get unit :executable))))
        (cond (blocked (cons 'requires-blocked (list :blockers (nreverse blocked))))
              (missing (cons 'requires-missing (list :missing (nreverse missing))))
              (env (cons 'env-missing (list :missing env)))
              (exe (cons 'executable-missing (list :missing exe))))))))

;;;; Plan

(cl-defstruct (keel-plan (:constructor keel-plan-make))
  "A validated, ordered plan.
PACKAGES and UNITS are reports in declaration order.  PACKAGE-ORDER and
UNIT-ORDER name the ok entries in execution order."
  packages units package-order unit-order)

(defun keel-plan-compute (packages units)
  "Plan PACKAGES and UNITS, both lists of declaration plists.
Pure apart from `keel-plan-probes'."
  (let* ((pkg (keel--resolve packages :deps 'package (lambda (_) nil)))
         (unit (keel--resolve units :after 'unit (keel--unit-check (car pkg)))))
    (keel-plan-make :packages (car pkg) :units (car unit)
                    :package-order (cdr pkg) :unit-order (cdr unit))))

(defun keel-plan-from-declarations ()
  "Plan the current declarations."
  (keel-plan-compute (keel-declared-packages) (keel-declared-units)))

(defun keel-plan-reports (plan)
  "All reports of PLAN, packages first."
  (append (keel-plan-packages plan) (keel-plan-units plan)))

(defun keel-plan-problems (plan)
  "Reports of PLAN that are not ok."
  (seq-remove #'keel-report-ok-p (keel-plan-reports plan)))

(provide 'keel-plan)
;;; keel-plan.el ends here
