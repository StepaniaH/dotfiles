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


;; Some keybindings
(setq mac-command-modifier 'super)
(global-set-key (kbd "s-a") 'mark-whole-buffer) ;;对应Windows上面的Ctrl-a 全选
(global-set-key (kbd "s-c") 'kill-ring-save) ;;对应Windows上面的Ctrl-c 复制
(global-set-key (kbd "s-s") 'save-buffer) ;; 对应Windows上面的Ctrl-s 保存
(global-set-key (kbd "s-v") 'yank) ;对应Windows上面的Ctrl-v 粘贴
(global-set-key (kbd "s-z") 'undo) ;对应Windows上面的Ctrol-z 撤销
(global-set-key (kbd "s-x") 'kill-region) ;对应Windows上面的Ctrol-x 剪切




(provide 'init)
;;; init.el ends here

