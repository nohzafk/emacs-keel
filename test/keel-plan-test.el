;;; keel-plan-test.el --- Tests for keel-plan -*- lexical-binding: t; -*-

(require 'ert)
(require 'keel-plan)

(defmacro keel-test-with-declarations (&rest body)
  "Run BODY with empty declaration registries and a neutral environment."
  `(let ((keel-packages nil)
         (keel-units nil)
         (keel-plan-probes
          (list :getenv (lambda (v) (and (equal v "HOME") "/home/x"))
                :executable-find (lambda (b) (and (equal b "git") "/usr/bin/git"))
                :feature-available-p (lambda (f) (memq f '(cl-lib subr-x))))))
     ,@body))

(defun keel-test-status (plan kind name)
  "Return (STATUS REASON) of NAME of KIND in PLAN."
  (let ((r (keel-find-report (if (eq kind 'package)
                                 (keel-plan-packages plan)
                               (keel-plan-units plan))
                             name)))
    (list (keel-report-status r) (keel-report-reason r))))

(ert-deftest keel-plan-declarations-record-order-and-source ()
  (keel-test-with-declarations
   (let ((keel--current-source '(:file "c.org" :heading "A" :line 3)))
     ;; Expansion happens inside `eval', as it does under the loader.
     (eval '(package! alpha) t)
     (eval '(package! beta :deps (alpha) :repo "x/beta") t))
   (config-unit! u1 :requires (alpha) :env (HOME) :executable (git) :config (ignore))
   (let ((pkgs (keel-declared-packages))
         (u (car (keel-declared-units))))
     (should (equal (mapcar #'keel-entry-name pkgs) '(alpha beta)))
     (should (equal (plist-get (cadr pkgs) :deps) '(alpha)))
     (should (equal (plist-get (cadr pkgs) :repo) "x/beta"))
     (should (equal (plist-get (car pkgs) :source) '(:file "c.org" :heading "A" :line 3)))
     (should (equal (plist-get u :requires) '(alpha)))
     (should (equal (plist-get u :env) '("HOME")))
     (should (equal (plist-get u :executable) '("git")))
     (should (equal (plist-get u :body) '((ignore))))
     (should (functionp (plist-get u :thunk))))))

(ert-deftest keel-plan-unknown-key-is-an-error ()
  (should-error (macroexpand '(package! a :bogus 1)))
  (should-error (macroexpand '(config-unit! u :nope 1 :config nil)))
  (should-error (macroexpand '(config-unit! u :after (x)))))

(ert-deftest keel-plan-orders-topologically-in-declaration-order ()
  (keel-test-with-declarations
   (package! c :deps (b))
   (package! a)
   (package! b :deps (a))
   (config-unit! u3 :after (u2) :config nil)
   (config-unit! u1 :config nil)
   (config-unit! u2 :after (u1) :config nil)
   (config-unit! u0 :config nil)
   (let ((plan (keel-plan-from-declarations)))
     (should (equal (keel-plan-package-order plan) '(a b c)))
     (should (equal (keel-plan-unit-order plan) '(u1 u2 u3 u0)))
     (should (null (keel-plan-problems plan))))))

(ert-deftest keel-plan-flags-duplicates ()
  (keel-test-with-declarations
   (package! a)
   (package! a)
   (let ((plan (keel-plan-from-declarations)))
     (should (equal (keel-test-status plan 'package 'a) '(invalid duplicate-name)))
     (should (null (keel-plan-package-order plan))))))

(ert-deftest keel-plan-flags-missing-reference-and-blocks-dependents ()
  (keel-test-with-declarations
   (package! a :deps (ghost))
   (package! b :deps (a))
   (config-unit! u1 :after (nothing) :config nil)
   (let* ((plan (keel-plan-from-declarations))
          (a (keel-find-report (keel-plan-packages plan) 'a))
          (b (keel-find-report (keel-plan-packages plan) 'b)))
     (should (equal (keel-test-status plan 'package 'a) '(invalid missing-reference)))
     (should (equal (plist-get (keel-report-details a) :missing) '(ghost)))
     (should (equal (keel-test-status plan 'package 'b) '(skipped blocked)))
     (should (equal (plist-get (keel-report-details b) :blockers) '(a)))
     (should (equal (keel-test-status plan 'unit 'u1) '(invalid missing-reference))))))

(ert-deftest keel-plan-flags-cycles-and-blocks-downstream ()
  (keel-test-with-declarations
   (config-unit! a :after (c) :config nil)
   (config-unit! b :after (a) :config nil)
   (config-unit! c :after (b) :config nil)
   (config-unit! d :after (c) :config nil)
   (config-unit! e :config nil)
   (let* ((plan (keel-plan-from-declarations))
          (a (keel-find-report (keel-plan-units plan) 'a)))
     (should (equal (keel-test-status plan 'unit 'a) '(invalid cycle)))
     (should (equal (keel-test-status plan 'unit 'c) '(invalid cycle)))
     (should (equal (sort (copy-sequence (plist-get (keel-report-details a) :members))
                          #'string<)
                    '(a b c)))
     (should (equal (keel-test-status plan 'unit 'd) '(skipped blocked)))
     (should (equal (keel-plan-unit-order plan) '(e))))))

(ert-deftest keel-plan-preflight-skips ()
  (keel-test-with-declarations
   (package! broken :deps (broken))
   (config-unit! needs-env :env (NOPE) :config nil)
   (config-unit! needs-exe :executable (nope) :config nil)
   (config-unit! needs-feature :requires (nope-feature) :config nil)
   (config-unit! needs-builtin :requires (cl-lib) :config nil)
   (config-unit! needs-broken :requires (broken) :config nil)
   (config-unit! after-skipped :after (needs-env) :config nil)
   (let ((plan (keel-plan-from-declarations t)))
     (should (equal (keel-test-status plan 'unit 'needs-env) '(skipped env-missing)))
     (should (equal (keel-test-status plan 'unit 'needs-exe) '(skipped executable-missing)))
     (should (equal (keel-test-status plan 'unit 'needs-feature) '(skipped requires-missing)))
     (should (equal (keel-test-status plan 'unit 'needs-builtin) '(ok nil)))
     (should (equal (keel-test-status plan 'unit 'needs-broken) '(skipped requires-blocked)))
     (should (equal (keel-test-status plan 'unit 'after-skipped) '(skipped blocked)))
     (should (equal (keel-plan-unit-order plan) '(needs-builtin))))))

(ert-deftest keel-plan-without-probe-is-structural-only ()
  "Boot plans without probing: a feature may appear only once packages
and earlier units have run, so the executor probes instead."
  (keel-test-with-declarations
   (config-unit! needs-env :env (NOPE) :config nil)
   (config-unit! needs-feature :requires (nope-feature) :config nil)
   (let ((plan (keel-plan-from-declarations)))
     (should (equal (keel-test-status plan 'unit 'needs-env) '(ok nil)))
     (should (equal (keel-test-status plan 'unit 'needs-feature) '(ok nil)))
     (should (equal (keel-plan-unit-order plan) '(needs-env needs-feature))))))

(ert-deftest keel-plan-declared-package-satisfies-requires-without-probe ()
  (keel-test-with-declarations
   (package! magit)
   (config-unit! u :requires (magit) :config nil)
   (should (equal (keel-test-status (keel-plan-from-declarations) 'unit 'u) '(ok nil)))))

(provide 'keel-plan-test)
;;; keel-plan-test.el ends here
