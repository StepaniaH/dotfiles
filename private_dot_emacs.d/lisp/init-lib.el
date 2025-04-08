;;; init-lib.el --- Library dependencies -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package hydra
 :ensure t)

(use-package use-package-hydra
 :ensure t
 :after hydra)

(use-package wakatime-mode
  :ensure t
  :config
  (global-wakatime-mode))

(provide 'init-lib)
;;; init-lib.el ends here
