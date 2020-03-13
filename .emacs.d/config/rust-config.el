;;; rust-config.el --- rust config
;;; Commentary:

;;; Code:

(use-package rust-mode)

(use-package cargo
  :hook (rust-mode . cargo-minor-mode)
  :bind (:map rust-mode-map
         ("<f5>" . cargo-process-run)))

(evil-define-key 'normal rust-mode-map (kbd "öö") 'rust-format-buffer)

(use-package flycheck-rust
  :hook (flycheck-mode . flycheck-rust-setup))

(provide 'rust-config)
;;; rust-config ends here
