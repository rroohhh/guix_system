;;; latex-config.el --- my latex config
;;; Commentary:

;;; Code:

(setq font-latex-fontify-script nil)

(use-package company-auctex
  :demand
  :config
  (company-auctex-init))

(setq-default TeX-engine 'xetex)
(setq-default TeX-PDF-mode t)

(add-hook 'LaTeX-mode-hook
          '(lambda ()
            (define-key LaTeX-mode-map (kbd "$") 'self-insert-command)))

(provide 'latex-config)
;;; latex-config.el ends here
