;;; keel-exec-test.el --- Tests for keel-exec -*- lexical-binding: t; -*-

(require 'ert)
(require 'keel-plan)
(require 'keel-exec)

(defvar keel-exec-test-log nil)

(defmacro keel-exec-test-with (&rest body)
  "Run BODY with empty registries, a permissive planner and a logging runner."
  `(let ((keel-packages nil)
         (keel-units nil)
         (keel-exec-test-log nil)
         (keel-plan-probes
          (list :getenv #'ignore
                :executable-find #'ignore
                :feature-available-p (lambda (_) t)))
         (keel-exec-package-installer
          (lambda (entry)
            (push (list 'install (keel-entry-name entry)) keel-exec-test-log)
            (when (eq (keel-entry-name entry) 'broken) (error "install failed"))))
         (keel-exec-unit-runner
          (lambda (unit thunk)
            (push (list 'run (keel-entry-name unit)) keel-exec-test-log)
            (funcall thunk))))
     ,@body))

(defun keel-exec-test-run ()
  "Plan and run the current declarations."
  (let ((pkgs (keel-declared-packages))
        (units (keel-declared-units)))
    (keel-exec-run (keel-plan-compute pkgs units) pkgs units)))

(defun keel-exec-test-status (run name)
  (let ((r (keel-run-result run name)))
    (list (keel-report-status r) (keel-report-reason r))))

(ert-deftest keel-exec-runs-in-plan-order-and-times ()
  (keel-exec-test-with
   (package! b :deps (a))
   (package! a)
   (config-unit! u2 :after (u1) :config (push 'u2-body keel-exec-test-log))
   (config-unit! u1 :config (push 'u1-body keel-exec-test-log))
   (let ((run (keel-exec-test-run)))
     (should (equal (reverse keel-exec-test-log)
                    '((install a) (install b) (run u1) u1-body (run u2) u2-body)))
     (should (null (keel-run-problems run)))
     (should (numberp (keel-result-elapsed (keel-run-result run 'u1))))
     (should (numberp (keel-run-elapsed run))))))

(ert-deftest keel-exec-failed-unit-skips-dependents-only ()
  (keel-exec-test-with
   (config-unit! bad :config (error "boom"))
   (config-unit! child :after (bad) :config nil)
   (config-unit! grandchild :after (child) :config nil)
   (config-unit! other :config (push 'other-ran keel-exec-test-log))
   (let* ((run (keel-exec-test-run))
          (bad (keel-run-result run 'bad))
          (child (keel-run-result run 'child)))
     (should (equal (keel-exec-test-status run 'bad) '(failed error)))
     (should (equal (plist-get (keel-report-details bad) :message) "boom"))
     (should (equal (car (keel-result-error bad)) 'error))
     (should (equal (keel-exec-test-status run 'child) '(skipped blocked)))
     (should (equal (plist-get (keel-report-details child) :blockers) '(bad)))
     (should (equal (keel-exec-test-status run 'grandchild) '(skipped blocked)))
     (should (equal (keel-exec-test-status run 'other) '(ok nil)))
     (should (memq 'other-ran keel-exec-test-log)))))

(ert-deftest keel-exec-failed-package-blocks-dependents-and-requiring-units ()
  (keel-exec-test-with
   (package! broken)
   (package! leaf :deps (broken))
   (package! subr-x)
   (config-unit! needs-broken :requires (broken) :config nil)
   (config-unit! needs-fine :requires (subr-x) :config nil)
   (let ((run (keel-exec-test-run)))
     (should (equal (keel-exec-test-status run 'broken) '(failed error)))
     (should (equal (keel-exec-test-status run 'leaf) '(skipped blocked)))
     (should (equal (keel-exec-test-status run 'subr-x) '(ok nil)))
     (should (equal (keel-exec-test-status run 'needs-broken) '(skipped blocked)))
     (should (equal (keel-exec-test-status run 'needs-fine) '(ok nil))))))

(ert-deftest keel-exec-planner-verdicts-carry-over ()
  (keel-exec-test-with
   (config-unit! dup :config nil)
   (config-unit! dup :config nil)
   (config-unit! loop :after (loop) :config nil)
   (let ((run (keel-exec-test-run)))
     (should (equal (keel-exec-test-status run 'dup) '(invalid duplicate-name)))
     (should (equal (keel-exec-test-status run 'loop) '(invalid cycle)))
     (should (equal (length (keel-run-results run)) 3))
     (should (null keel-exec-test-log)))))

(ert-deftest keel-exec-requires-are-loaded-before-body ()
  (keel-exec-test-with
   (config-unit! u :requires (subr-x) :config (push (featurep 'subr-x) keel-exec-test-log))
   (keel-exec-test-run)
   (should (equal (car keel-exec-test-log) t))))

(ert-deftest keel-exec-events-fire ()
  (keel-exec-test-with
   (let* ((events nil)
          (keel-exec-event-functions
          (list (lambda (e) (push (plist-get e :event) events)))))
     (package! p)
     (config-unit! u :config nil)
     (keel-exec-test-run)
     (should (equal (reverse events) '(start package unit finish))))))
(defmacro keel-exec-test-with-feature (feature &rest body)
  "Run BODY, then make sure FEATURE is not left provided."
  (declare (indent 1))
  `(unwind-protect (progn ,@body)
     (setq features (delq ,feature features))
     (setq after-load-alist (assq-delete-all ,feature after-load-alist))))

(ert-deftest keel-exec-deferred-unit-runs-when-feature-loads ()
  (keel-exec-test-with
   (keel-exec-test-with-feature 'keel-exec-test-feat
     (let* ((keel-exec-deferred nil)
            (seen nil)
            (keel-exec-deferred-result-functions
            (list (lambda (r) (push (keel-report-status r) seen)))))
       (config-unit! lazy :defer keel-exec-test-feat :config (push 'lazy-body keel-exec-test-log))
       (let ((run (keel-exec-test-run)))
         (should (equal (keel-exec-test-status run 'lazy) '(ok deferred)))
         (should-not (memq 'lazy-body keel-exec-test-log))
         (should (equal (length keel-exec-deferred) 1))
         (provide 'keel-exec-test-feat)
         (should (memq 'lazy-body keel-exec-test-log))
         (should (equal seen '(ok)))
         (should (null keel-exec-deferred)))))))

(ert-deftest keel-exec-deferred-unit-waits-for-deferred-after ()
  (keel-exec-test-with
   (keel-exec-test-with-feature 'keel-exec-test-feat-a
     (keel-exec-test-with-feature 'keel-exec-test-feat-b
       (let ((keel-exec-deferred nil)
             (keel-exec-deferred-result-functions nil))
         (config-unit! first :defer keel-exec-test-feat-a :config (push 'first keel-exec-test-log))
         (config-unit! second :defer keel-exec-test-feat-b :after (first)
           :config (push 'second keel-exec-test-log))
         (keel-exec-test-run)
         (setq keel-exec-test-log nil)
         (provide 'keel-exec-test-feat-b)
         (should (null keel-exec-test-log))
         (provide 'keel-exec-test-feat-a)
         (should (equal (seq-filter #'symbolp keel-exec-test-log) '(second first))))))))

(ert-deftest keel-exec-deferred-unit-with-loaded-feature-runs-at-once ()
  (keel-exec-test-with
   (let ((keel-exec-deferred nil)
         (keel-exec-deferred-result-functions nil))
     (config-unit! now :defer cl-lib :config (push 'now keel-exec-test-log))
     (keel-exec-test-run)
     (should (memq 'now keel-exec-test-log))
     (should (null keel-exec-deferred)))))

(ert-deftest keel-exec-deferred-forget-drops-a-pending-unit ()
  (keel-exec-test-with
   (keel-exec-test-with-feature 'keel-exec-test-feat-c
     (let ((keel-exec-deferred nil)
           (keel-exec-deferred-result-functions nil))
       (config-unit! gone :defer keel-exec-test-feat-c :config (push 'gone keel-exec-test-log))
       (keel-exec-test-run)
       (keel-exec-deferred-forget 'gone)
       (provide 'keel-exec-test-feat-c)
       (should-not (memq 'gone keel-exec-test-log))))))

(provide 'keel-exec-test)
;;; keel-exec-test.el ends here
