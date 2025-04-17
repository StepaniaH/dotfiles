;;; init-org.el --- Org mode configuration -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; 基本 org-mode 配置
(use-package org
  :ensure t
  :config
  ;; 这里可以添加 org-mode 的基本配置
  (setq org-hide-leading-stars t)           ; 隐藏前导星号
  (setq org-startup-indented t)             ; 缩进模式
  (setq org-startup-folded 'content)        ; 启动时折叠到内容级别
  (setq org-log-done 'time)                 ; 任务完成时记录时间
  )

(setq org-directory (file-truename "~/Documents/helloworld/"))

;; 确保目录存在
(unless (file-exists-p org-directory)
  (make-directory org-directory t))
(unless (file-exists-p (concat org-directory "roam/"))
  (make-directory (concat org-directory "roam/") t))

(use-package org-roam
  :ensure t
  :after org
  :init
  (setq org-roam-v2-ack t) ;; Acknowledge V2 upgrade
  :custom
  (org-roam-directory (concat org-directory "roam/")) ; 设置 org-roam 目录
  :config
  (org-roam-db-autosync-mode 1) ; 替代已废弃的 org-roam-setup
  :bind
  (("C-c n f" . org-roam-node-find)
   :map org-mode-map
   
   ("C-c n i" . org-roam-node-insert)
   ("C-c n o" . org-id-get-create)
   ("C-c n t" . org-roam-tag-add)
   ("C-c n a" . org-roam-alias-add)
   ("C-c n l" . org-roam-buffer-toggle)))

(provide 'init-org)
;;; init-org.el ends here
