;;; projectile-config.el --- projectile config
;;; Commentary:

;;; Code:

(use-package projectile
  :demand
  :diminish projectile-mode
  :init
  (setq projectile-completion-system 'helm)
  (setq projectile-enable-caching t)
  :bind ("<f5>" . projectile-compile-project)
  :config
  (projectile-mode))

(use-package helm-projectile
  :demand
  :init
  (setq helm-projectile-fuzzy-match nil)
  :config
  (helm-projectile-on))

(provide 'projectile-config)
;;; projectile-config)el ends here
