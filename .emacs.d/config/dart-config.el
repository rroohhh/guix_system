;;; code-config.el --- general coding config
;;; Commentary:

;;; Code:

(use-package lsp-mode :ensure t)
(use-package lsp-dart
  :ensure t
  :hook (dart-mode . lsp))

(use-package lsp-ui :ensure t)
(use-package hover :ensure t)

;;; code-config.el ends here
