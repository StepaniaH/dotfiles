;;; init-package.el --- Packagement -*- lexical-binding:t -*-
;;; Commentary:
;;; Code:

;; Package settings
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

;; Use use-package
(eval-when-compile
  (require 'use-package))

;; Performance optimization
(setq gc-cons-threshold (* 128 1024 1024))
(setq read-process-output-max (* 4 1024 1024))
(setq process-adaptive-read-buffering nil)

(provide 'init-package)
;;; init-package.el ends here
