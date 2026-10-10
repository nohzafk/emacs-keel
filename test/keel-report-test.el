;;; keel-report-test.el --- Tests for keel-report -*- lexical-binding: t; -*-

(require 'ert)
(require 'keel-report)

(defvar keel-report-test-hook nil)

(defmacro keel-report-test-with (&rest body)
  `(let ((keel-packages nil)
         (keel-units nil)
         (keel-effects-registry nil)
         (keel-last-run nil)
         (keel-last-lint nil)
         (keel-report-test-hook nil)
         (keel-after-run-hook nil)
         (keel-unit-body-analyzer #'keel-static-analyze)
         (keel-exec-unit-runner #'keel-effects-run-unit)
         (keel-plan-probes (list :getenv #'ignore :executable-find #'ignore
                                 :feature-available-p (lambda (_) t))))
     (unwind-protect (progn ,@body)
       (dolist (b '("*keel*" "*keel units*" "*keel: good*"))
         (when (get-buffer b) (kill-buffer b))))))

(defun keel-report-test-declare-and-run ()
  (let ((keel--current-source '(:file "/nonexistent/config.org" :heading "H" :line 4)))
    (eval '(config-unit! good :config (add-hook 'keel-report-test-hook #'ignore)) t)
    (eval '(config-unit! bad :after (good) :config (error "boom")) t)
    (eval '(config-unit! child :after (bad) :config nil) t)
    (eval '(config-unit! mapper :config (define-key some-mode-map "q" #'ignore)) t))
  (let ((units (keel-declared-units)))
    (setq keel-last-run (keel-exec-run (keel-plan-compute nil units) nil units)
          keel-last-lint (keel-static-lint units))))

(ert-deftest keel-report-renders-problems-units-and-lint ()
  (keel-report-test-with
   (keel-report-test-declare-and-run)
   (with-temp-buffer
     (keel-report-render keel-last-run keel-last-lint)
     (let ((text (buffer-string)))
       (should (string-match-p "Problems" text))
       (should (string-match-p "failed .*bad  error: boom" text))
       (should (string-match-p "skipped .*child  blocked by (bad)" text))
       (should (string-match-p "config.org:4 (H)" text))
       (should (string-match-p "Lint" text))
       (should (string-match-p "some-mode-map is used as a keymap" text))))))

(ert-deftest keel-report-reason-strings ()
  (should (equal (keel-report-reason-string
                  (keel-report-make :status 'skipped :reason 'env-missing
                                    :details '(:missing ("HOME"))))
                 "env unset: (HOME)"))
  (should (equal (keel-report-reason-string (keel-report-make :status 'ok)) "")))

(ert-deftest keel-report-show-if-needed-only-on-problems ()
  (keel-report-test-with
   (let ((keel-units nil))
     (eval '(config-unit! fine :config nil) t)
     (let ((units (keel-declared-units)))
       (setq keel-last-run (keel-exec-run (keel-plan-compute nil units) nil units)))
     (keel-report-show-if-needed)
     (should-not (get-buffer "*keel*"))
     (let ((keel-show-report-on-startup t))
       (keel-report-show-if-needed)
       (should (get-buffer "*keel*"))))))

(ert-deftest keel-report-explain-and-list ()
  (keel-report-test-with
   (keel-report-test-declare-and-run)
   (keel-explain 'good)
   (with-current-buffer "*keel: good*"
     (let ((text (buffer-string)))
       (should (string-match-p "config-unit! good" text))
       (should (string-match-p "needed by:  (bad)" text))
       (should (string-match-p "Effects (1)" text))
       (should (string-match-p "hook keel-report-test-hook -> ignore" text))))
   (keel-list-units)
   (with-current-buffer "*keel units*"
     (let ((entries (keel-list-units--entries)))
       (should (equal (length entries) 4))
       (should (equal (aref (cadr (assq 'bad entries)) 1) "failed"))))))

(ert-deftest keel-report-rerun-unit-retracts-then-reapplies ()
  (keel-report-test-with
   (keel-report-test-declare-and-run)
   (should (equal (length keel-report-test-hook) 1))
   (let ((result (keel-rerun-unit 'good)))
     (should (keel-report-ok-p result))
     (should (equal (length keel-report-test-hook) 1))
     (should (equal (length (keel-effects-of 'good)) 1))
     (should (numberp (keel-result-elapsed (keel-run-result keel-last-run 'good)))))))

(provide 'keel-report-test)
;;; keel-report-test.el ends here
