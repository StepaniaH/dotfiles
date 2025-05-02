;;; init.el --- Load the full configuration -*- lexical-binding: t  -*-
;;; Commentary:

;; This file bootstraps the configuration, which is divided into
;; a number of other files.

;;; Code:

;; A big contributor to startup times is garbage collection. We up the gc
;; threshold to temporarily prevent it from running, and then reset it by the
;; `gcmh' package.
(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6)

;; Increase how much is read from processes in a single chunk (default is 4kb).
;; `lsp-mode' benefits from that.
;;
;; `cat /proc/sys/fs/pipe-max-size` to check the max value (on Linux).
;; `sysctl -a` to check the system information (on macOS).
(setq read-process-output-max (* 4 1024 1024))


(require 'package)
(setq package-archives
      '(("melpa"  . "https://melpa.org/packages/")
        ("gnu"    . "https://elpa.gnu.org/packages/")
        ("nongnu" . "https://elpa.nongnu.org/nongnu/")))
(package-initialize)

;; Bootstrap `use-package'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-and-compile
  (setq use-package-always-ensure nil)
  (setq use-package-always-defer nil)
  (setq use-package-always-demand nil)
  (setq use-package-expand-minimally nil)
  (setq use-package-enable-imenu-support t))
(eval-when-compile
  (require 'use-package))

;; Keep ~/.config/emacs/ clean.
(use-package no-littering
  :ensure t
  :demand t)

;; Bootstrap `quelpa'.
(use-package quelpa
  :ensure t
  :commands quelpa
  :custom
  (quelpa-git-clone-depth 1)
  (quelpa-self-upgrade-p nil)
  (quelpa-update-melpa-p nil)
  (quelpa-checkout-melpa-p nil))

;; --debug-init implies `debug-on-error'.
(setq debug-on-error init-file-debug)

;; Add Emacs Lisp directories to load path
(let ((dir (locate-user-emacs-file "lisp")))
  (add-to-list 'load-path (file-name-as-directory dir))
  (add-to-list 'load-path (file-name-as-directory (expand-file-name "lang" dir))))

;; Save custom settings to a separate file
(setq custom-file (locate-user-emacs-file "custom.el"))

;; Create custom file if it doesn't exist
(unless (file-exists-p custom-file)
  (write-region "" nil custom-file))



(require 'init-benchmarking) ;; Measure startup time


;; Core modules
(require 'init-base)
(require 'init-utils)
(require 'init-ui)
(require 'init-tools)
(require 'init-lsp)
(require 'init-git)
(require 'init-dev)
(require 'init-dired)
(require 'init-minibuffer)
(require 'init-keybindings)    ;; Global key-bindings

;; Functionality modules
(require 'init-editor)  ;; Editor behavior
(require 'init-completion)   ;; Complete framework


(require 'init-org)

;; standalone apps
(require 'init-spell)


(defun open-init-file ()
  "Open the user's init file of configuration."
  (interactive)
  (find-file user-init-file))

;;让鼠标滚动更好用
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1) ((control) . nil)))
(setq mouse-wheel-progressive-speed nil)



;; macOS 文件和目录外部操作功能
(defun consult-directory-externally (file)
  "Open FILE or its directory externally using macOS Finder."
  (interactive "fOpen externally: ")
  (let ((path (expand-file-name file)))
    (if (file-directory-p path)
        ;; 如果是目录，直接打开
        (shell-command (format "open %s" (shell-quote-argument path)))
      ;; 如果是文件，打开其所在目录并选中该文件
      (shell-command (format "open -R %s" (shell-quote-argument path))))))

(defun my-open-current-directory ()
  "Open current directory in Finder."
  (interactive)
  (shell-command (format "open %s" (shell-quote-argument default-directory))))

(defun open-with-default-app (file)
  "Open FILE with its default application."
  (interactive "fOpen file with default app: ")
  (shell-command (format "open %s" (shell-quote-argument (expand-file-name file)))))

(defun reveal-in-finder (file)
  "Reveal FILE in Finder."
  (interactive (list (or (buffer-file-name) default-directory)))
  (shell-command (format "open -R %s" (shell-quote-argument (expand-file-name file)))))

(defun open-terminal-here ()
  "Open Terminal.app at current directory."
  (interactive)
  (shell-command (format "open -a Terminal %s" (shell-quote-argument default-directory))))

(defun open-kitty-here ()
  "Open Kitty terminal at current directory."
  (interactive)
  (let ((dir (shell-quote-argument (expand-file-name default-directory))))
    (start-process "kitty" nil "kitty" "--single-instance" "--directory" dir)))

;; 集成到 Embark (如果已安装)
(with-eval-after-load 'embark
  (define-key embark-file-map (kbd "E") #'consult-directory-externally)
  (define-key embark-file-map (kbd "O") #'open-with-default-app)
  (define-key embark-file-map (kbd "R") #'reveal-in-finder)
  (define-key embark-file-map (kbd "T") #'open-terminal-here)
  (define-key embark-file-map (kbd "I") #'open-iterm-here))

;; 全局键绑定
(global-set-key (kbd "C-c o d") #'my-open-current-directory)
(global-set-key (kbd "C-c o f") #'reveal-in-finder)
(global-set-key (kbd "C-c o t") #'open-terminal-here)

;; Load custom settings at the end to avoid some unexpected error.
(when (file-exists-p custom-file)
  (load custom-file))

(provide 'init)
;;; init.el ends here
