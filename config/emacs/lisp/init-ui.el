;;; init-ui.el --- UI related settings -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; Interface elements
(tool-bar-mode -1)
(when (display-graphic-p) (toggle-scroll-bar -1))

;; Font settings
(set-face-attribute 'default nil
                    :family "Hack Nerd Font Mono"
                    :height 150
                    :weight 'normal
                    :width 'normal)

;; Theme
(use-package catppuccin-theme
  :ensure t
  :init
  (load-theme 'catppuccin :no-confirm)
  (setq catppuccin-flavor 'mocha)
  (catppuccin-reload))

;; Daseboard
(use-package dashboard
 :ensure t
 :config
 (setq dashboard-banner-logo-title "Welcome to Emacs!") ;; 个性签名，随读者喜好设置
 ;; (setq dashboard-projects-backend 'projectile) ;; 读者可以暂时注释掉这一行，等安装了 projectile 后再使用
 (setq dashboard-startup-banner 'official) ;; 也可以自定义图片
 (setq dashboard-items '((recents . 5)  ;; 显示多少个最近文件
  (bookmarks . 5) ;; 显示多少个最近书签
  (projects . 10))) ;; 显示多少个最近项目
 (dashboard-setup-startup-hook))

;; Rainbow delimiters
(use-package rainbow-delimiters
 :ensure t
 :hook (prog-mode . rainbow-delimiters-mode))

(provide 'init-ui)
;;; init-ui.el ends here
