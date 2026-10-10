;;; keel-exec.el --- Walk a plan and run it -*- lexical-binding: t; -*-

;; The executor is deliberately thin.  It walks the plan's order, asks the
;; installer to install packages and the runner to run unit bodies, times
;; each step, and records a result per declaration.  A failure marks its
;; dependents skipped; nothing else is affected.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'keel-plan)

;;;; Results

(cl-defstruct (keel-result (:include keel-report)
                           (:constructor keel-result-make))
  "The outcome of one declaration after execution.
STATUS adds `failed' to the planner's set.  ELAPSED is seconds, ERROR the
condition that failed it."
  elapsed error)

(cl-defstruct (keel-run (:constructor keel-run-make))
  "One execution of a plan: RESULTS per declaration, in declaration order."
  plan results started elapsed)

(defun keel-run-result (run name)
  "The result named NAME in RUN."
  (keel-find-report (keel-run-results run) name))

(defun keel-run-problems (run)
  "Results of RUN that are not ok."
  (seq-remove #'keel-report-ok-p (keel-run-results run)))

(defun keel-result-from-report (report)
  "Copy planner REPORT into a result."
  (keel-result-make :name (keel-report-name report)
                    :kind (keel-report-kind report)
                    :status (keel-report-status report)
                    :reason (keel-report-reason report)
                    :details (keel-report-details report)
                    :source (keel-report-source report)))

;;;; Pluggable steps

(defvar keel-exec-package-installer #'keel-exec-default-package-installer
  "Function called with a package entry to make it installed and activated.
It returns normally on success and signals on failure.  The default knows
nothing about fetching; keel-package replaces it.")

(defvar keel-exec-unit-runner #'keel-exec-default-unit-runner
  "Function called with (UNIT THUNK) to run a unit body.
keel-effects replaces it to capture effects around THUNK.")

(defvar keel-exec-unit-filter #'ignore
  "Function called with a unit before it runs.
nil runs the unit; a symbol records it ok with that reason, unrun.  Reload
uses it to leave unchanged units alone.")

(defvar keel-exec-event-functions nil
  "Abnormal hook run with one plist per execution event.
Events carry :event (`start' `package' `unit' `finish') and a :result when
one exists.")

(defun keel-exec--event (&rest plist)
  "Run `keel-exec-event-functions' with PLIST."
  (run-hook-with-args 'keel-exec-event-functions plist))

(defun keel-exec-default-package-installer (entry)
  "Signal unless package ENTRY is already available."
  (let ((name (keel-entry-name entry)))
    (unless (or (featurep name)
                (locate-library (symbol-name name))
                (and (fboundp 'package-installed-p)
                     (package-installed-p name)))
      (signal 'keel-package-not-installed (list name)))))

(define-error 'keel-package-not-installed "Package is not installed")

(defun keel-exec-default-unit-runner (_unit thunk)
  "Run THUNK."
  (funcall thunk))

;;;; Execution

(defun keel-exec--timed (thunk)
  "Call THUNK; return (VALUE . SECONDS)."
  (let ((t0 (current-time)))
    (cons (funcall thunk) (float-time (time-since t0)))))

(defun keel-exec--run-step (report step)
  "Run STEP under REPORT; return a result.
STEP is a thunk; an error makes the result `failed'."
  (let (elapsed err)
    (condition-case e
        (setq elapsed (cdr (keel-exec--timed step)))
      (error (setq err e)))
    (keel-result-make :name (keel-report-name report)
                      :kind (keel-report-kind report)
                      :status (if err 'failed 'ok)
                      :reason (and err 'error)
                      :details (and err (list :message (error-message-string err)))
                      :source (keel-report-source report)
                      :elapsed elapsed
                      :error err)))

(defun keel-exec--blocked-result (report blockers)
  "A skipped result for REPORT blocked by BLOCKERS."
  (keel-result-make :name (keel-report-name report)
                    :kind (keel-report-kind report)
                    :status 'skipped
                    :reason 'blocked
                    :details (list :blockers blockers)
                    :source (keel-report-source report)))

(defun keel-exec--not-ok (results names)
  "Those of NAMES whose result in RESULTS is not ok."
  (seq-remove (lambda (n) (keel-report-ok-p (keel-find-report results n))) names))

(defun keel-exec--run-body (unit report)
  "Load UNIT's `:requires' and run its body under REPORT; return a result."
  (keel-exec--run-step
   report
   (lambda ()
     (dolist (feature (plist-get unit :requires))
       (require feature))
     (funcall keel-exec-unit-runner unit (plist-get unit :thunk)))))

(defun keel-exec-unit (unit report results)
  "Execute UNIT under planner REPORT given RESULTS so far; return its result.
Loads `:requires' features first.  A failed dependency blocks the unit.
A unit with `:defer' is registered to run when its feature loads and its
`:after' units have run; its result is ok with reason `deferred' until
then."
  (let ((blockers (keel-exec--not-ok results
                                     (append (plist-get unit :after)
                                             (seq-filter
                                              (lambda (f) (keel-find-report results f))
                                              (plist-get unit :requires))))))
    (cond
     (blockers (keel-exec--blocked-result report blockers))
     ((funcall keel-exec-unit-filter unit)
      (keel-result-make :name (keel-report-name report)
                        :kind (keel-report-kind report)
                        :status 'ok
                        :reason (funcall keel-exec-unit-filter unit)
                        :source (keel-report-source report)))
     ((plist-get unit :defer)
      (keel-exec--defer unit report))
     (t (keel-exec--run-body unit report)))))

;;;; Deferred units

(defvar keel-exec-deferred nil
  "Units waiting to run: plists (:name :unit :report :feature :loaded).")

(defvar keel-exec-deferred-result-functions nil
  "Abnormal hook run with the result of a deferred unit once it has run.")

(defun keel-exec--defer (unit report)
  "Register UNIT to run when its `:defer' feature loads; return the result."
  (let ((name (keel-entry-name unit))
        (feature (plist-get unit :defer)))
    (keel-exec-deferred-forget name)
    (push (list :name name :unit unit :report report :feature feature :loaded nil)
          keel-exec-deferred)
    (with-eval-after-load feature
      (keel-exec--deferred-feature-loaded name))
    (keel-result-make :name name
                      :kind (keel-report-kind report)
                      :status 'ok
                      :reason 'deferred
                      :details (list :feature feature)
                      :source (keel-report-source report))))

(defun keel-exec-deferred-forget (name)
  "Drop the pending deferred unit NAME, if any."
  (setq keel-exec-deferred
        (seq-remove (lambda (p) (eq (plist-get p :name) name)) keel-exec-deferred)))

(defun keel-exec--deferred-feature-loaded (name)
  "Note that deferred unit NAME's feature has loaded, and run what is ready."
  (let ((pending (seq-find (lambda (p) (eq (plist-get p :name) name)) keel-exec-deferred)))
    (when pending
      (plist-put pending :loaded t)
      (keel-exec--pump-deferred))))

(defun keel-exec--deferred-ready-p (pending)
  "Non-nil when PENDING's feature has loaded and no `:after' unit is still pending."
  (and (plist-get pending :loaded)
       (not (seq-some (lambda (dep)
                        (seq-some (lambda (p) (eq (plist-get p :name) dep))
                                  keel-exec-deferred))
                      (plist-get (plist-get pending :unit) :after)))))

(defun keel-exec--pump-deferred ()
  "Run every deferred unit that is ready, until none is."
  (let ((ready (seq-find #'keel-exec--deferred-ready-p keel-exec-deferred)))
    (while ready
      (keel-exec-deferred-forget (plist-get ready :name))
      (let ((result (keel-exec--run-body (plist-get ready :unit) (plist-get ready :report))))
        (keel-exec--event :event 'unit :result result)
        (run-hook-with-args 'keel-exec-deferred-result-functions result))
      (setq ready (seq-find #'keel-exec--deferred-ready-p keel-exec-deferred)))))

(defun keel-exec-package (entry report results)
  "Install package ENTRY under planner REPORT given RESULTS so far."
  (let ((blockers (keel-exec--not-ok results (plist-get entry :deps))))
    (if blockers
        (keel-exec--blocked-result report blockers)
      (keel-exec--run-step
       report
       (lambda () (funcall keel-exec-package-installer entry))))))

(defun keel-exec-run (plan packages units)
  "Execute PLAN over declaration lists PACKAGES and UNITS; return a `keel-run'."
  (let ((started (current-time))
        results)
    (keel-exec--event :event 'start :plan plan)
    ;; Planner verdicts that are not ok carry over unchanged.
    (dolist (report (keel-plan-reports plan))
      (unless (keel-report-ok-p report)
        (push (keel-result-from-report report) results)))
    (dolist (name (keel-plan-package-order plan))
      (let* ((entry (keel-find-entry packages name))
             (report (keel-find-report (keel-plan-packages plan) name))
             (result (keel-exec-package entry report results)))
        (push result results)
        (keel-exec--event :event 'package :result result)))
    (dolist (name (keel-plan-unit-order plan))
      (let* ((entry (keel-find-entry units name))
             (report (keel-find-report (keel-plan-units plan) name))
             (result (keel-exec-unit entry report results)))
        (push result results)
        (keel-exec--event :event 'unit :result result)))
    (let ((run (keel-run-make
                :plan plan
                :results (mapcar (lambda (r) (keel-find-report results (keel-report-name r)))
                                 (keel-plan-reports plan))
                :started started
                :elapsed (float-time (time-since started)))))
      (keel-exec--event :event 'finish :run run)
      run)))

(provide 'keel-exec)
;;; keel-exec.el ends here
