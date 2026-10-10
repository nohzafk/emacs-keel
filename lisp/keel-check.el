;;; keel-check.el --- Validate a config without running it -*- lexical-binding: t; -*-

;; `keel check' runs the same loader, planner and lint that boot does, and
;; stops before execution.  Meant for `emacs --batch' in CI.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'keel-plan)
(require 'keel-static)
(require 'keel-config)
(require 'keel-report)

(defun keel-check (&optional strict)
  "Load and plan the config; return (:code N :plan PLAN :lint FINDINGS).
Code 0 means clean (lint allowed unless STRICT), 1 means an invalid
declaration or, with STRICT, a lint warning; 2 a harness error such as a
missing config.  Units skipped by preflight (env, executable) are not
problems: they depend on the machine, not the config."
  (condition-case err
      (let* ((file (keel-config-load-declarations))
             (plan (keel-plan-from-declarations))
             (lint (keel-static-lint (keel-declared-units) t))
             (invalid (seq-filter (lambda (r) (eq (keel-report-status r) 'invalid))
                                  (keel-plan-reports plan)))
             (warnings (seq-filter (lambda (f) (eq (plist-get f :level) 'warning)) lint)))
        (list :code (cond (invalid 1)
                          ((and strict warnings) 1)
                          (t 0))
              :file file :plan plan :lint lint))
    (error (list :code 2 :error err))))

(defun keel-check--print-human (result)
  "Print RESULT for a terminal."
  (if (eq (plist-get result :code) 2)
      (princ (format "keel check: %s\n" (error-message-string (plist-get result :error))))
    (let* ((plan (plist-get result :plan))
           (reports (keel-plan-reports plan))
           (problems (keel-plan-problems plan))
           (lint (plist-get result :lint)))
      (princ (format "keel check: %s\n  %d packages, %d units, %d problems, %d lint findings\n"
                     (plist-get result :file)
                     (length (keel-plan-packages plan)) (length (keel-plan-units plan))
                     (length problems) (length lint)))
      (dolist (r reports)
        (unless (keel-report-ok-p r)
          (princ (format "  %-8s %-24s %s\n    at %s\n"
                         (keel-report-status r) (keel-report-name r)
                         (keel-report-reason-string r)
                         (keel-report-source-string (keel-report-source r))))))
      (dolist (f lint)
        (princ (format "  %-8s %s: %s\n    at %s\n"
                       (plist-get f :level) (plist-get f :unit) (plist-get f :message)
                       (keel-report-source-string (plist-get f :source))))))))

(defun keel-check--print-sexp (result)
  "Print RESULT as one readable s-expression."
  (let ((print-length nil) (print-level nil))
    (prin1
     (if (eq (plist-get result :code) 2)
         (list :code 2 :error (error-message-string (plist-get result :error)))
       (list :code (plist-get result :code)
             :file (plist-get result :file)
             :reports (mapcar (lambda (r)
                                (list :name (keel-report-name r) :kind (keel-report-kind r)
                                      :status (keel-report-status r)
                                      :reason (keel-report-reason r)
                                      :details (keel-report-details r)
                                      :source (keel-report-source r)))
                              (keel-plan-reports (plist-get result :plan)))
             :lint (plist-get result :lint))))
    (terpri)))

;;;###autoload
(defun keel-check-batch ()
  "Batch entry point; exits with the check's code.
Usage: emacs --batch -l keel -f keel-check-batch [--strict] [--format sexp]"
  (let ((args command-line-args-left)
        strict format)
    (setq command-line-args-left nil)
    (while args
      (pcase (pop args)
        ("--strict" (setq strict t))
        ("--format" (setq format (pop args)))
        (arg (princ (format "keel check: unknown argument %s\n" arg))
             (kill-emacs 2))))
    (let ((result (keel-check strict)))
      (if (equal format "sexp")
          (keel-check--print-sexp result)
        (keel-check--print-human result))
      (kill-emacs (plist-get result :code)))))

(provide 'keel-check)
;;; keel-check.el ends here
