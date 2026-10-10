;;; keel-config-test.el --- Tests for keel-config -*- lexical-binding: t; -*-

(require 'ert)
(require 'keel-config)

(defvar keel-config-test-hook nil)
(defvar keel-config-test-var 'original)
(defvar keel-config-test-log nil)

(defmacro keel-config-test-with-home (&rest body)
  "Run BODY with `keel-config-directory' bound to a fresh temp directory."
  `(let* ((keel-config-directory (make-temp-file "keel-home" t))
          (keel-config-org-file nil)
          (keel-config-file nil)
          (keel-env-file nil)
          (keel-packages nil)
          (keel-units nil)
          (keel-effects-registry nil)
          (keel-last-run nil)
          (keel-config-test-hook nil)
          (keel-config-test-var 'original)
          (keel-config-test-log nil)
          (keel-exec-unit-runner #'keel-effects-run-unit)
          (keel-plan-probes (list :getenv #'ignore :executable-find #'ignore
                                  :feature-available-p (lambda (_) t))))
     (unwind-protect (progn ,@body)
       (delete-directory keel-config-directory t))))

(defun keel-config-test-write (name content)
  "Write CONTENT to NAME in the test home."
  (with-temp-file (expand-file-name name keel-config-directory)
    (insert content)))

(defconst keel-config-test-org
  "#+title: test

* Editing

#+begin_src emacs-lisp
(config-unit! editing
  :config
  (add-hook 'keel-config-test-hook #'ignore))
#+end_src

* Variables

Some prose.

#+begin_src emacs-lisp
(config-unit! vars
  :after (editing)
  :config
  (setq keel-config-test-var 'changed))
#+end_src

#+begin_src emacs-lisp :tangle no
(error \"never tangled\")
#+end_src
")

(ert-deftest keel-config-tangle-and-load-record-org-provenance ()
  (keel-config-test-with-home
   (keel-config-test-write "config.org" keel-config-test-org)
   (keel-config-load-declarations)
   (let* ((units (keel-declared-units))
          (editing (keel-find-entry units 'editing))
          (vars (keel-find-entry units 'vars))
          (src (plist-get vars :source)))
     (should (equal (mapcar #'keel-entry-name units) '(editing vars)))
     (should (equal (file-name-nondirectory (plist-get src :file)) "config.org"))
     (should (equal (plist-get src :heading) "Variables"))
     (should (equal (plist-get src :line) 16))
     (should (equal (plist-get (plist-get editing :source) :line) 6))
     (should (plist-get vars :static)))))

(ert-deftest keel-config-plain-file-when-no-org ()
  (keel-config-test-with-home
   (keel-config-test-write "config.el"
                           ";; -*- lexical-binding: t; -*-\n\n(config-unit! only :config nil)\n")
   (keel-config-load-declarations)
   (let ((src (plist-get (car (keel-declared-units)) :source)))
     (should (equal (file-name-nondirectory (plist-get src :file)) "config.el"))
     (should (equal (plist-get src :line) 3))
     (should (null (plist-get src :heading))))))

(ert-deftest keel-config-missing-signals ()
  (keel-config-test-with-home
   (should-error (keel-config-load-target) :type 'keel-config-missing)))

(ert-deftest keel-config-env-file-injects ()
  (keel-config-test-with-home
   (keel-config-test-write "env" "(\"KEEL_TEST_VAR=hello\" \"PATH=/keel/bin:/usr/bin\")")
   (let ((process-environment (copy-sequence process-environment))
         (exec-path exec-path))
     (should (equal (keel-config-load-env) 2))
     (should (equal (getenv "KEEL_TEST_VAR") "hello"))
     (should (equal (car exec-path) "/keel/bin/")))))

(ert-deftest keel-config-boot-runs-and-records ()
  (keel-config-test-with-home
   (keel-config-test-write "config.org" keel-config-test-org)
   (let ((run (keel-boot)))
     (should (eq run keel-last-run))
     (should (null (keel-run-problems run)))
     (should (memq #'ignore keel-config-test-hook))
     (should (eq keel-config-test-var 'changed))
     (should (equal (length (keel-effects-of 'vars)) 1)))))

(ert-deftest keel-config-reload-retracts-changed-and-leaves-unchanged ()
  (keel-config-test-with-home
   (keel-config-test-write "config.org" keel-config-test-org)
   (keel-boot)
   (keel-config-test-write
    "config.org"
    (concat (replace-regexp-in-string "'changed" "'again" keel-config-test-org)
            "\n* Added\n#+begin_src emacs-lisp\n(config-unit! added :config (push 'added keel-config-test-log))\n#+end_src\n"))
   (let ((run (keel-reload)))
     (should (eq (keel-report-reason (keel-run-result run 'editing)) 'unchanged))
     (should (null (keel-report-reason (keel-run-result run 'vars))))
     (should (eq keel-config-test-var 'again))
     (should (equal keel-config-test-log '(added)))
     ;; editing's hook survived untouched; vars has one fresh record.
     (should (equal (length (keel-effects-of 'editing)) 1))
     (should (equal (length (keel-effects-of 'vars)) 1)))
   ;; Remove vars: its variable goes back to the original.
   (keel-config-test-write "config.org"
                           (substring keel-config-test-org 0
                                      (string-match "^\\* Variables" keel-config-test-org)))
   (keel-reload)
   (should (eq keel-config-test-var 'original))
   (should (null (keel-effects-of 'vars)))
   (should (memq #'ignore keel-config-test-hook))))

(ert-deftest keel-config-diff-units ()
  (let ((old (list (list :name 'a :body '(1)) (list :name 'b :body '(2))))
        (new (list (list :name 'a :body '(1)) (list :name 'b :body '(3))
                   (list :name 'c :body '(4)))))
    (should (equal (keel-config-diff-units old new)
                   '(:unchanged (a) :changed (b) :added (c) :removed nil)))
    (should (equal (plist-get (keel-config-diff-units new old) :removed) '(c)))))

(provide 'keel-config-test)
;;; keel-config-test.el ends here
