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

(defun keel-run-result (run name &optional kind)
  "The result named NAME in RUN; KIND defaults to `unit'."
  (keel-find-report (keel-run-results run) name (or kind 'unit)))

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

(defun keel-exec--not-ok (results names kind)
  "Those of NAMES whose result of KIND in RESULTS is not ok."
  (seq-remove (lambda (n) (keel-report-ok-p (keel-find-report results n kind))) names))

(defun keel-exec--pending-p (results names)
  "Non-nil when any unit of NAMES is deferred and has not run yet."
  (seq-some (lambda (n)
              (let ((r (keel-find-report results n 'unit)))
                (and r (eq (keel-report-reason r) 'deferred))))
            names))

(defun keel-exec--skipped-result (report skip)
  "A skipped result for REPORT from preflight verdict SKIP, (REASON . DETAILS)."
  (keel-result-make :name (keel-report-name report)
                    :kind (keel-report-kind report)
                    :status 'skipped
                    :reason (car skip)
                    :details (cdr skip)
                    :source (keel-report-source report)))

(defun keel-exec--run-body (unit report results)
  "Probe UNIT's environment against package RESULTS, then run its body.
Loads `:requires' first.  Returns a result."
  (let ((skip (keel-plan-unit-preflight results unit)))
    (if skip
        (keel-exec--skipped-result report skip)
      (keel-exec--run-step
       report
       (lambda ()
         (dolist (feature (plist-get unit :requires))
           (require feature))
         (funcall keel-exec-unit-runner unit (plist-get unit :thunk)))))))

(defun keel-exec-unit (unit report results)
  "Execute UNIT under planner REPORT given RESULTS so far; return its result.
A failed dependency blocks the unit.  Right before the body runs, the
unit's `:requires', `:env' and `:executable' are probed, so a feature a
package or an earlier unit just made available counts.  A unit with
`:defer' is registered to run when its feature loads and its `:after'
units have run; its result is ok with reason `deferred' until then."
  (let ((blockers (append
                   (keel-exec--not-ok results (plist-get unit :after) 'unit)
                   (keel-exec--not-ok results
                                      (seq-filter
                                       (lambda (f) (keel-find-report results f 'package))
                                       (plist-get unit :requires))
                                      'package))))
    (cond
     (blockers (keel-exec--blocked-result report blockers))
     ((funcall keel-exec-unit-filter unit)
      (keel-result-make :name (keel-report-name report)
                        :kind (keel-report-kind report)
                        :status 'ok
                        :reason (funcall keel-exec-unit-filter unit)
                        :source (keel-report-source report)))
     ;; A unit after a deferred unit cannot run before it: it waits too.
     ((or (plist-get unit :defer)
          (keel-exec--pending-p results (plist-get unit :after)))
      (keel-exec--defer unit report results))
     (t (keel-exec--run-body unit report results)))))

;;;; Deferred units

(defvar keel-exec-deferred nil
  "Units waiting to run: plists (:name :unit :report :feature :loaded).
A unit with no :feature only waits for its `:after' units.")

(defvar keel-exec-deferred-results nil
  "Alist name -> result of deferred units that have run since the last run.")

(defvar keel-exec-deferred-result-functions nil
  "Abnormal hook run with the result of a deferred unit once it has run.")

(defun keel-exec--defer (unit report results)
  "Register UNIT to run later; return its interim result.
It runs when its `:defer' feature (if any) has loaded and every `:after'
unit has run, and is probed then against the package RESULTS of this run."
  (let ((name (keel-entry-name unit))
        (feature (plist-get unit :defer)))
    (keel-exec-deferred-forget name)
    (push (list :name name :unit unit :report report :feature feature
                :loaded (null feature)
                :packages (seq-filter (lambda (r) (eq (keel-report-kind r) 'package))
                                      results))
          keel-exec-deferred)
    (when feature
      (with-eval-after-load feature
        (keel-exec--deferred-feature-loaded name)))
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

(defun keel-exec-deferred-reset ()
  "Forget every pending deferred unit and every deferred result."
  (setq keel-exec-deferred nil
        keel-exec-deferred-results nil))

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

(defun keel-exec--deferred-blockers (pending)
  "The `:after' units of PENDING that ran deferred and did not succeed."
  (seq-filter (lambda (dep)
                (let ((r (alist-get dep keel-exec-deferred-results)))
                  (and r (not (keel-report-ok-p r)))))
              (plist-get (plist-get pending :unit) :after)))

(defun keel-exec--pump-deferred ()
  "Run every deferred unit that is ready, until none is.
A unit whose deferred dependency failed is skipped as blocked."
  (let ((ready (seq-find #'keel-exec--deferred-ready-p keel-exec-deferred)))
    (while ready
      (keel-exec-deferred-forget (plist-get ready :name))
      (let* ((blockers (keel-exec--deferred-blockers ready))
             (result (if blockers
                         (keel-exec--blocked-result (plist-get ready :report) blockers)
                       (keel-exec--run-body (plist-get ready :unit)
                                            (plist-get ready :report)
                                            (plist-get ready :packages)))))
        (setf (alist-get (plist-get ready :name) keel-exec-deferred-results) result)
        (keel-exec--event :event 'unit :result result)
        (run-hook-with-args 'keel-exec-deferred-result-functions result))
      (setq ready (seq-find #'keel-exec--deferred-ready-p keel-exec-deferred)))))

(defun keel-exec-package (entry report results)
  "Install package ENTRY under planner REPORT given RESULTS so far."
  (let ((blockers (keel-exec--not-ok results (plist-get entry :deps) 'package)))
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
    (setq keel-exec-deferred-results nil)
    ;; Planner verdicts that are not ok carry over unchanged.
    (dolist (report (keel-plan-reports plan))
      (unless (keel-report-ok-p report)
        (push (keel-result-from-report report) results)))
    (dolist (name (keel-plan-package-order plan))
      (let* ((entry (keel-find-entry packages name))
             (report (keel-find-report (keel-plan-packages plan) name 'package))
             (result (keel-exec-package entry report results)))
        (push result results)
        (keel-exec--event :event 'package :result result)))
    (dolist (name (keel-plan-unit-order plan))
      (let* ((entry (keel-find-entry units name))
             (report (keel-find-report (keel-plan-units plan) name 'unit))
             (result (keel-exec-unit entry report results)))
        (push result results)
        (keel-exec--event :event 'unit :result result)))
    (let ((run (keel-run-make
                :plan plan
                :results (mapcar (lambda (r)
                                   (keel-find-report results (keel-report-name r)
                                                     (keel-report-kind r)))
                                 (keel-plan-reports plan))
                :started started
                :elapsed (float-time (time-since started)))))
      (keel-exec--event :event 'finish :run run)
      run)))

(provide 'keel-exec)
;;; keel-exec.el ends here
