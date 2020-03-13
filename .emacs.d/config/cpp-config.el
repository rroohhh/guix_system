;;; cpp-config.el --- cpp/c config
;;; Commentary:

;;; Code:

;; (add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))

;; (use-package rtags
;;   :ensure rtags
;;   :init
;;   (setq rtags-completions-enabled t)
;;   (setq rtags-use-helm t)
;;   :config
;;   (rtags-enable-standard-keybindings))

;; (use-package company-rtags
;;   :config
;;   (eval-after-load 'company
;;     '(add-to-list
;;       'company-backend 'company-rtags)))

;; ;; (defun my-flycheck-rtags-setup ()
;; ;;   "Flycheck rtags setup."
;; ;;   (flycheck-select-checker 'rtags))

;; ;; (use-package flycheck-rtags
;; ;;   :demand
;; ;;   :ensure rtags
;; ;;   :config
;; ;;   (add-hook 'c-mode-common-hook #'my-flycheck-rtags-setup))

;; (use-package irony
;;   :demand
;;   :config
;;   (add-hook 'c-mode-common-hook 'irony-mode))

;; (use-package company-irony
;;   :demand
;;   :config
;;   (eval-after-load 'company
;;     '(add-to-list 'company-backends 'company-irony))
;;   (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options))

;; (use-package company-irony-c-headers
;;   :demand
;;   :config
;;   (eval-after-load 'company
;;     '(add-to-list 'company-backends 'company-irony-c-headers)))

;; ;; (use-package flycheck-irony
;; ;;   :demand
;; ;;   :config
;; ;;   (eval-after-load 'flycheck
;; ;;     '(add-hook 'flycheck-mode-hook #'flycheck-irony-setup)))

;; ;; (use-package irony-eldoc
;; ;;   :demand
;; ;;   :config
;; ;;   (add-hook 'irony-mode-hook 'irony-eldoc))

;; (use-package cmake-ide
;;   :demand
;;   :config
;;   (cmake-ide-setup)
;;   (add-hook 'c-mode-common-hook (lambda ()
;;                                   (global-set-key [remap projectile-compile-project] 'cmake-ide-compile))))

;; (use-package clang-format
;;   :demand
;;   :init
;;   (setq clang-format-style "file")
;;   :config
;;   (evil-define-key 'normal c-mode-base-map (kbd "öö") 'clang-format-buffer))

;; (use-package modern-cpp-font-lock
;;   :config
;;   (modern-c++-font-lock-global-mode t))

;; (use-package commenter
;;   :demand
;;   :config
;;   (setq comment-style 'extra-line)
;;   (add-hook 'c++-mode-hook
;;             (lambda ()
;;               (setq-local commenter-config
;;                           '((single
;;                              . ((comment-start      . "//")
;;                                 (comment-end        . "")
;;                                 (comment-start-skip . "\\(//+\\|/\\*+\\)\\s *")))
;;                             (multi
;;                              . ((comment-start      . "/* ")
;;                                 (comment-end        . " */")
;;                                 (comment-start-skip . "/\\*")
;;                                 (comment-end-skip   . "\\*/")
;;                                 (comment-continue   . " * ")
;;                                 (comment-padding    . " ")
;;                                 (comment-multi-line . t)))))
;;               (commenter-setup)))
;;   )

;; (commenter-vars-setup)

;; (defun c-setup ()
;;   "Change some indentions to fit clang-format."
;;   (c-set-offset 'brace-list-entry 0)
;;   (c-set-offset 'brace-list-intro 0)
;;   (c-set-offset 'brace-list-close '-))
;; (add-hook 'c-mode-common-hook 'c-setup)

;; ;; linux coding style
;; (setq-default c-basic-offset 4 c-default-style "linux")
;; (setq-default tab-width 4)

(provide 'cpp-config)
;;; cpp-config.el ends here
