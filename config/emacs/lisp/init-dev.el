;;; init-dev.el --- Development relative configurations  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; Hiding structured data
;;
;; zm hide-all
;; zr show-all
;; za toggle-fold
;; zo show-block
;; zc hide-block
(use-package hideshow
  :ensure nil
  :hook (prog-mode . hs-minor-mode)
  :config
  (defconst hideshow-folded-face '((t (:inherit 'font-lock-comment-face :extend t :weight light))))

  (defface hideshow-border-face
    '((t (:inherit font-lock-function-name-face :extend t)))
    "Face used for hideshow fringe."
    :group 'hideshow)

  (define-fringe-bitmap 'hideshow-folded-fringe
    (vector #b11111100
            #b11111100
            #b01111111
            #b00111111
            #b00111111
            #b00011111
            #b00000111
            #b00000011
            #b00000011
            #b00000111
            #b00011111
            #b00111111
            #b00111111
            #b01111111
            #b11111100
            #b11111100))

  (defun hideshow-folded-overlay-fn (ov)
    "Display a folded region indicator."
    (when (eq 'code (overlay-get ov 'hs))
      ;; fringe indicator
      (overlay-put ov 'before-string (propertize " " 'display '(left-fringe hideshow-folded-fringe hideshow-border-face)))
      ;; delete overlay if the region is removed
      (overlay-put ov 'evaporate t)
      ;; folding indicator
      (overlay-put ov 'face hideshow-folded-face)
      (overlay-put ov 'display (propertize " [...] " 'face hideshow-folded-face))))

  :custom
  (hs-hide-comments-when-hiding-all nil)
  (hs-set-up-overlay #'hideshow-folded-overlay-fn))


;; TODO: Non-copy from this line
(use-package tiny
 :ensure t
 ;; 可选绑定快捷键，笔者个人感觉不绑定快捷键也无妨
 :bind
 ("C-;" . tiny-expand))

(use-package flycheck
 :ensure t
 :config
 (setq truncate-lines nil) ; 如果单行信息很长会自动换行
 :hook
 (prog-mode . flycheck-mode))

(use-package projectile
 :ensure t
 :bind (("C-c p" . projectile-command-map))
 :config
 (setq projectile-mode-line "Projectile")
 (setq projectile-track-known-projects-automatically nil))

(use-package counsel-projectile
 :ensure t
 :after (projectile)
 :init (counsel-projectile-mode))

(use-package magit
  :ensure t)

(use-package treemacs
 :ensure t
 :defer t
 :config
 (treemacs-tag-follow-mode)
 :bind
 (:map global-map
    ("M-0"    . treemacs-select-window)
    ("C-x t 1"  . treemacs-delete-other-windows)
    ("C-x t t"  . treemacs)
    ("C-x t B"  . treemacs-bookmark)
    ;; ("C-x t C-t" . treemacs-find-file)
    ("C-x t M-t" . treemacs-find-tag))
 (:map treemacs-mode-map
	("/" . treemacs-advanced-helpful-hydra)))

(use-package treemacs-projectile
 :ensure t
 :after (treemacs projectile))

(provide 'init-dev)
;;; init-dev.el ends here
