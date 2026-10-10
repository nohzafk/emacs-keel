;;; keel-static-test.el --- Tests for keel-static -*- lexical-binding: t; -*-

(require 'ert)
(require 'keel-plan)
(require 'keel-static)

(defun keel-static-test-unit (name requires &rest body)
  "Declare and return a unit NAME with REQUIRES and BODY, analyzed."
  (let ((keel-units nil)
        (keel-unit-body-analyzer #'keel-static-analyze))
    (eval `(config-unit! ,name :requires ,requires :config ,@body) t)
    (car keel-units)))

(ert-deftest keel-static-collects-setq-targets ()
  (let ((s (plist-get (keel-static-test-unit
                       'u nil
                       '(setq a 1 b 2)
                       '(setq-default c 3)
                       '(setopt d 4)
                       '(customize-set-variable 'e 5)
                       '(let ((x 1)) (setq f x))
                       '(setq-local not-global 1))
                      :static)))
    (should (equal (plist-get s :setq) '(a b c d e f)))))

(ert-deftest keel-static-collects-hooks-advice-keys ()
  (let ((s (plist-get (keel-static-test-unit
                       'u nil
                       '(add-hook 'prog-mode-hook #'flymake-mode)
                       '(dolist (h '(a-hook b-hook)) (add-hook h #'ignore))
                       '(advice-add 'find-file :around #'my-wrap)
                       '(keymap-set global-map "C-c a" #'foo)
                       '(define-key magit-mode-map (kbd "q") #'bar)
                       '(global-set-key (kbd "C-c b") #'baz))
                      :static)))
    (should (equal (plist-get s :hooks) '(prog-mode-hook)))
    (should (equal (plist-get s :advice) '(find-file)))
    (should (equal (plist-get s :keys)
                   '((global-map . "C-c a")
                     (magit-mode-map . (kbd "q"))
                     (global-map . (kbd "C-c b")))))
    (should (equal (plist-get s :maps) '(magit-mode-map)))
    (should (member 'flymake-mode (plist-get s :modes)))))

(ert-deftest keel-static-lint-map-without-requires ()
  (let* ((u (keel-static-test-unit 'u nil '(define-key magit-mode-map "q" #'bar)))
         (f (keel-static-lint-requires u)))
    (should (equal (length f) 1))
    (should (eq (plist-get (car f) :kind) 'map-without-requires))
    (should (eq (plist-get (car f) :level) 'warning))
    (should (eq (plist-get (car f) :unit) 'u))))

(ert-deftest keel-static-lint-requires-satisfied-by-prefix ()
  (let ((u (keel-static-test-unit 'u '(magit) '(define-key magit-mode-map "q" #'bar))))
    (should (null (keel-static-lint-requires u)))))

(ert-deftest keel-static-lint-unused-requires ()
  (let* ((u (keel-static-test-unit 'u '(magit org) '(setq org-directory "~/org")))
         (f (keel-static-lint-requires u)))
    (should (equal (length f) 1))
    (should (eq (plist-get (car f) :kind) 'unused-requires))
    (should (string-match-p "magit" (plist-get (car f) :message)))))

(ert-deftest keel-static-lint-compile-reports-free-variable-and-unknown-function ()
  (let* ((u (keel-static-test-unit 'u nil
                                   '(setq x keel-static-test-undefined-var)
                                   '(keel-static-test-undefined-fn 1)))
         (f (keel-static-lint-compile u))
         (kinds (mapcar (lambda (x) (plist-get x :kind)) f)))
    (should (memq 'free-variable kinds))
    (should (memq 'unresolved-function kinds))
    (should (eq (plist-get (seq-find (lambda (x) (eq (plist-get x :kind) 'unresolved-function)) f)
                           :level)
                'info))))

(ert-deftest keel-static-lint-compile-clean-body-has-no-findings ()
  (let ((u (keel-static-test-unit 'u nil '(add-hook 'prog-mode-hook #'ignore))))
    (should (null (keel-static-lint-compile u)))))

(ert-deftest keel-static-lint-compile-wrong-arity ()
  (let* ((u (keel-static-test-unit 'u nil '(add-hook)))
         (f (keel-static-lint-compile u)))
    (should (seq-some (lambda (x) (eq (plist-get x :kind) 'wrong-arity)) f))))

(provide 'keel-static-test)
;;; keel-static-test.el ends here
