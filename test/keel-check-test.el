;;; keel-check-test.el --- Tests for keel-check -*- lexical-binding: t; -*-

(require 'ert)
(require 'keel-check)

(defmacro keel-check-test-with-config (content &rest body)
  "Run BODY with a temp config dir holding config.el CONTENT."
  (declare (indent 1))
  `(let* ((keel-config-directory (make-temp-file "keel-check" t))
          (keel-config-org-file nil)
          (keel-config-file nil)
          (keel-packages nil)
          (keel-units nil)
          (keel-plan-probes (list :getenv #'ignore :executable-find #'ignore
                                  :feature-available-p (lambda (f) (eq f 'cl-lib)))))
     (unwind-protect
         (progn
           (with-temp-file (expand-file-name "config.el" keel-config-directory)
             (insert ";; -*- lexical-binding: t; -*-\n" ,content))
           ,@body)
       (delete-directory keel-config-directory t))))

(ert-deftest keel-check-clean-config-is-0 ()
  (keel-check-test-with-config "(config-unit! a :requires (cl-lib) :config (cl-incf x))"
    (let ((r (keel-check)))
      (should (equal (plist-get r :code) 0))
      (should (keel-plan-p (plist-get r :plan))))))

(ert-deftest keel-check-problems-are-1 ()
  (keel-check-test-with-config "(config-unit! a :after (ghost) :config nil)"
    (should (equal (plist-get (keel-check) :code) 1))))

(ert-deftest keel-check-lint-warning-is-0-unless-strict ()
  (keel-check-test-with-config "(config-unit! a :config (define-key foo-mode-map \"q\" #'ignore))"
    (should (equal (plist-get (keel-check) :code) 0))
    (should (equal (plist-get (keel-check t) :code) 1))))

(ert-deftest keel-check-missing-config-is-2 ()
  (let ((keel-config-directory (make-temp-file "keel-check-empty" t))
        (keel-config-org-file nil)
        (keel-config-file nil))
    (unwind-protect
        (should (equal (plist-get (keel-check) :code) 2))
      (delete-directory keel-config-directory t))))

(ert-deftest keel-check-output-formats ()
  (keel-check-test-with-config "(config-unit! a :after (ghost) :config nil)"
    (let ((r (keel-check)))
      (should (string-match-p "invalid  a +refers to undeclared (ghost)"
                              (with-output-to-string (keel-check--print-human r))))
      (let ((sexp (read (with-output-to-string (keel-check--print-sexp r)))))
        (should (equal (plist-get sexp :code) 1))
        (should (eq (plist-get (car (plist-get sexp :reports)) :status) 'invalid))))))

(provide 'keel-check-test)
;;; keel-check-test.el ends here
