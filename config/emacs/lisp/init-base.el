;;; init-base.el --- Basic settings -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; Basic options
(setq confirm-kill-emacs #'yes-or-no-p)
(setq inhibit-startup-message t)
(setq make-backup-files nil)
(global-auto-revert-mode t)
(savehist-mode 1)

;; Programming mode settings
(add-hook 'prog-mode-hook #'show-paren-mode)
(add-hook 'prog-mode-hook #'hs-minor-mode)
(setq display-line-numbers-type 'relative)

;; Global mode
(column-number-mode t)
(delete-selection-mode t)
(global-display-line-numbers-mode 1)

;; Bracket pairing
(electric-pair-mode t)


(provide 'init-base)
;;; init-base.el ends here
