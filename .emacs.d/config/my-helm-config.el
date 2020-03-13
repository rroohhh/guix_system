;;; my-helm-config.el --- helm config
;;; Commentary:

;;; Code:
(use-package helm
  :demand
  :diminish helm-mode
  :init
  (setq helm-mode-fuzzy-match t)
  (setq helm-completion-in-region-fuzzy-match t)
  (setq helm-M-x-fuzzy-match t)
  (setq helm-multi-swoop-edit-save t)
  (setq helm-split-window-in-side-p           t)
  (setq helm-move-to-line-cycle-in-source     t)
  :bind (([remap execute-extended-command] . helm-M-x)
         ([remap find-file] . helm-find-files)
         ([remap list-all-buffers] . helm-buffers-list)
         ("M-y" . helm-show-kill-ring)
         ("M-x" . helm-M-x))
  :config
  (helm-mode 1)
  (helm-autoresize-mode 1))

(use-package helm-ag
  :config
  (custom-set-variables
   '(helm-ag-base-command "rg --vimgrep --no-heading")))

(use-package helm-swoop
  :bind (([remap isearch-forward] . helm-swoop)
         ([remap isearch-backward] . helm-swoop)))

(provide 'my-helm-config)
;;; my-helm-config.el ends here
