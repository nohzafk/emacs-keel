;;; keel.el --- Deterministic, dependency-aware Emacs configuration -*- lexical-binding: t; -*-

;; Author: nohzafk
;; Version: 0.1.0
;; Package-Requires: ((emacs "31.1"))
;; Keywords: convenience, lisp
;; URL: https://github.com/nohzafk/emacs-keel

;;; Commentary:

;; keel turns an Emacs config into a declared dependency graph: planned
;; before anything runs, executed eagerly in deterministic order, every
;; failure traced to its source line.  See README.md.
;;
;;   (require 'keel)
;;   (keel-boot)

;;; Code:

(require 'keel-plan)
(require 'keel-exec)
(require 'keel-static)
(require 'keel-effects)
(require 'keel-config)
(require 'keel-report)
(require 'keel-check)
(require 'keel-package)

(provide 'keel)
;;; keel.el ends here
