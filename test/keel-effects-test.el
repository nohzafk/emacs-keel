;;; keel-effects-test.el --- Tests for keel-effects -*- lexical-binding: t; -*-

(require 'ert)
(require 'keel-plan)
(require 'keel-exec)
(require 'keel-static)
(require 'keel-effects)

(defvar keel-effects-test-hook nil)
(defvar keel-effects-test-var 'original)
(defvar keel-effects-test-map (make-sparse-keymap))
(defun keel-effects-test-fn () 'plain)
(defun keel-effects-test-wrap (orig) (list 'wrapped (funcall orig)))

(defmacro keel-effects-test-with (&rest body)
  "Run BODY with fresh registries and effect capture on."
  `(let ((keel-units nil)
         (keel-effects-registry nil)
         (keel-effects-test-hook nil)
         (keel-effects-test-var 'original)
         (keel-effects-test-map (make-sparse-keymap))
         (keel-unit-body-analyzer #'keel-static-analyze))
     (unwind-protect (progn ,@body)
       (advice-remove 'keel-effects-test-fn #'keel-effects-test-wrap)
       (makunbound 'keel-effects-test-fresh))))

(defun keel-effects-test-run (name)
  "Run declared unit NAME under capture; return its value."
  (let ((unit (keel-find-entry keel-units name)))
    (keel-effects-run-unit unit (plist-get unit :thunk))))

(ert-deftest keel-effects-captures-hook-advice-key-variable ()
  (keel-effects-test-with
   (eval '(config-unit! u :config
            (add-hook 'keel-effects-test-hook #'ignore)
            (advice-add 'keel-effects-test-fn :around #'keel-effects-test-wrap)
            (keymap-set keel-effects-test-map "C-c t" #'ignore)
            (setq keel-effects-test-var 'changed))
         t)
   (keel-effects-test-run 'u)
   (should (memq #'ignore keel-effects-test-hook))
   (should (equal (keel-effects-test-fn) '(wrapped plain)))
   (should (eq (keymap-lookup keel-effects-test-map "C-c t") #'ignore))
   (should (eq keel-effects-test-var 'changed))
   (let ((kinds (mapcar (lambda (r) (plist-get r :kind)) (keel-effects-of 'u))))
     (should (equal kinds '(hook advice key variable))))
   (should (eq (plist-get (nth 2 (keel-effects-of 'u)) :map-name)
               'keel-effects-test-map))
   (keel-effects-retract-unit 'u)
   (should (null keel-effects-test-hook))
   (should (eq (keel-effects-test-fn) 'plain))
   (should (null (keymap-lookup keel-effects-test-map "C-c t")))
   (should (eq keel-effects-test-var 'original))
   (should (null (keel-effects-of 'u)))))

(ert-deftest keel-effects-captures-effects-from-loops-and-helpers ()
  (keel-effects-test-with
   (defun keel-effects-test-helper (h) (add-hook h #'ignore))
   (eval '(config-unit! u :config
            (dolist (h '(keel-effects-test-hook))
              (keel-effects-test-helper h)))
         t)
   (keel-effects-test-run 'u)
   (should (equal (length (keel-effects-of 'u)) 1))
   (keel-effects-retract-unit 'u)
   (should (null keel-effects-test-hook))))

(ert-deftest keel-effects-variable-first-old-value-wins-and-unbound-restores ()
  (keel-effects-test-with
   (eval '(config-unit! u :config
            (setq keel-effects-test-var 'one)
            (setq keel-effects-test-var 'two)
            (setq keel-effects-test-fresh 'new))
         t)
   (keel-effects-test-run 'u)
   (should (equal (length (keel-effects-of 'u)) 2))
   (should (eq keel-effects-test-fresh 'new))
   (keel-effects-retract-unit 'u)
   (should (eq keel-effects-test-var 'original))
   (should-not (boundp 'keel-effects-test-fresh))))

(ert-deftest keel-effects-key-retract-leaves-a-newer-binding-alone ()
  (keel-effects-test-with
   (eval '(config-unit! u :config (keymap-set keel-effects-test-map "a" #'ignore)) t)
   (keel-effects-test-run 'u)
   (keymap-set keel-effects-test-map "a" #'identity)
   (keel-effects-retract-unit 'u)
   (should (eq (keymap-lookup keel-effects-test-map "a") #'identity))))

(ert-deftest keel-effects-failed-body-is-retracted ()
  (keel-effects-test-with
   (eval '(config-unit! u :config
            (add-hook 'keel-effects-test-hook #'ignore)
            (setq keel-effects-test-var 'changed)
            (error "boom"))
         t)
   (should-error (keel-effects-test-run 'u))
   (should (null keel-effects-test-hook))
   (should (eq keel-effects-test-var 'original))
   (should (null (keel-effects-of 'u)))))

(ert-deftest keel-effects-capture-is-off-outside-a-body ()
  (keel-effects-test-with
   (eval '(config-unit! u :config (setq keel-effects-test-var 'changed)) t)
   (keel-effects-test-run 'u)
   (setq keel-effects-test-var 'later)
   (add-hook 'keel-effects-test-hook #'ignore)
   (should (equal (length (keel-effects-of 'u)) 1))
   (remove-hook 'keel-effects-test-hook #'ignore)))

(ert-deftest keel-effects-executor-integration ()
  (keel-effects-test-with
   (let ((keel-packages nil)
         (keel-exec-unit-runner #'keel-effects-run-unit)
         (keel-plan-probes (list :getenv #'ignore :executable-find #'ignore
                                 :feature-available-p (lambda (_) t))))
     (eval '(config-unit! u :config (add-hook 'keel-effects-test-hook #'ignore)) t)
     (let ((run (keel-exec-run (keel-plan-from-declarations) nil (keel-declared-units))))
       (should (keel-report-ok-p (keel-run-result run 'u)))
       (should (equal (length (keel-effects-of 'u)) 1))))))

(provide 'keel-effects-test)
;;; keel-effects-test.el ends here
