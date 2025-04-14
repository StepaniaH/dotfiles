;;; init-completion.el --- Completion framework -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; Ivy Complete Framework
(use-package counsel
  :ensure t)
(use-package ivy
 :ensure t
 :init
 (ivy-mode 1)
 (counsel-mode 1)
 :config
 (setq ivy-use-virtual-buffers t)
 (setq search-default-mode #'char-fold-to-regexp)
 (setq ivy-count-format "(%d/%d) ")
 :bind
 (("C-s" . 'swiper)
  ("C-x b" . 'ivy-switch-buffer)
  ("C-c v" . 'ivy-push-view)
  ("C-c s" . 'ivy-switch-view)
  ("C-c V" . 'ivy-pop-view)
  ("C-x C-@" . 'counsel-mark-ring)
  ("C-x C-SPC" . 'counsel-mark-ring)
  :map minibuffer-local-map
  ("C-r" . counsel-minibuffer-history)))

(use-package amx
 :ensure t
 :init (amx-mode))

;; Comments and documentation enhancement
(use-package marginalia
 :ensure t
 :init (marginalia-mode)
 :bind (:map minibuffer-local-map
	     ("M-A" . marginalia-cycle)))

;; Company mode
(use-package company
 :ensure t
 :init (global-company-mode)
 :config
 (setq company-minimum-prefix-length 1) ; 只需敲 1 个字母就开始进行自动补全
 (setq company-tooltip-align-annotations t)
   (setq company-idle-delay 0.0)
 (setq company-show-numbers t) ;; 给选项编号 (按快捷键 M-1、M-2 等等来进行选择).
 (setq company-selection-wrap-around t)
 (setq company-transformers '(company-sort-by-occurrence))) ; 根据选择的频率进行排序，读者如果不喜欢可以去掉

;; Code snippets
(use-package yasnippet
 :ensure t
 :hook
 (prog-mode . yas-minor-mode)
 :config
 (yas-reload-all)
 ;; add company-yasnippet to company-backends
 (defun company-mode/backend-with-yas (backend)
  (if (and (listp backend) (member 'company-yasnippet backend))
   backend
   (append (if (consp backend) backend (list backend))
        '(:with company-yasnippet))))
 (setq company-backends (mapcar #'company-mode/backend-with-yas company-backends))
 ;; unbind <TAB> completion
 (define-key yas-minor-mode-map [(tab)]    nil)
 (define-key yas-minor-mode-map (kbd "TAB")  nil)
 (define-key yas-minor-mode-map (kbd "<tab>") nil)
 :bind
 (:map yas-minor-mode-map ("S-<tab>" . yas-expand)))

(use-package yasnippet-snippets
 :ensure t
 :after yasnippet)

;; Global completion
(global-set-key (kbd "M-/") 'hippie-expand)

(provide 'init-completion)
;;; init-completion.el ends here
