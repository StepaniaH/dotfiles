;;; init.el --- Load the full configuration -*- lexical-binding: t  -*-
;;; Commentary:

;; This file bootstraps the configuration, which is divided into
;; a number of other files.

;;; Code:

;; Produce backtraces when errors occur: can be helpful to diagnose startup issues
;;(setq debug-on-error t)

(let ((minver "27.1"))
  (when (version< emacs-version minver)
    (error "Your Emacs is too old -- this config requires v%s or higher" minver)))
(when (version< emacs-version "28.1")
  (message "Your Emacs is old, and some functionality in this config will be disabled. Please upgrade if possible."))

;; Add lisp directory to load path
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
(require 'init-benchmarking) ;; Measure startup time

;l; Constants
(defconst *spell-check-support-enable* nil) ;; Enable with t if you prefer
(defconst *is-a-mac* (eq system-type 'darwin))

;; Save custom settings to a separate file
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
;; Create this file if it is not exist
(unless (file-exists-p custom-file)
  (write-region "" nil custom-file))
;; Load it
(load custom-file)

;; Core modules
(require 'init-package) ;; Package management must be loaded first
(require 'init-base)    ;; Basic options
(require 'init-ui)      ;; UI
(require 'init-keys)    ;; Global key-bindings

;; Functionality modules
(require 'init-lib)     ;; Basic tools
(require 'init-editor)  ;; Editor behavior
(require 'init-completion)   ;; Complete framework
(require 'init-dev)
(require 'init-lsp)
(require 'init-org)

(provide 'init)
;;; init.el ends here
