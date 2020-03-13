;;; company-config.el --- company config
;;; Commentary:

;;; Code:
(use-package company
  :demand
  :diminish company-mode
  :init
  :config
  (setq company-tooltip-align-annotations t)
  (global-company-mode 1)
  (define-key company-active-map (kbd "RET") 'company-complete-selection)
  (global-set-key (kbd "<C-return>") 'company-complete)
  (global-set-key (kbd "C-RET") 'company-complete)
  (add-hook 'after-init-hook
            (lambda ()
              (add-to-list 'company-backends 'company-elisp)
              (setq company-idle-delay 0.2)))
  (setq evil-complete-next-func (lambda (arg) (company-complete)))
  (setq evil-complete-previous-func (lambda (arg) (company-complete))))

(use-package company-statistics
  :init
  (setq company-statistics-auto-save t)
  :config
  (add-hook 'after-init-hook 'company-statistics-mode))

(use-package company-tern
  :config
  (add-to-list 'company-backends 'company-tern))

; (use-package company-quickhelp
;   :config
;   (company-quickhelp-mode))

(use-package company-lsp
  :demand
  :config
  (add-to-list 'company-backends 'company-lsp))

(setq lsp-rust-analyzer-server-command "/data/.cargo/bin/rust-analyzer")

(provide 'company-config)
;;; company-config.el ends here
