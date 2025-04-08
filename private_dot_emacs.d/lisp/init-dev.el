;;; init-dev.el --- Development relative configurations  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

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
