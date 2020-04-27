;;; python-config.el --- Python config               -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(defun py-outline-level ()
  (let (buffer-invisibility-spec)
    (save-excursion
      (skip-chars-forward "\t ")
      (current-column))))

(defun my-pythonFold-hook ()
  (setq outline-regexp "[^ \t\n]\\|[ \t]*\\(def[ \t]+\\|class[ \t]+\\)")
  (setq outline-level 'py-outline-level)
  (outline-minor-mode t))

(add-hook 'python-mode-hook 'my-pythonFold-hook)

(use-package company-jedi
  :init
  :config
  (setq jedi:server-command `("python3" ,(concat jedi:source-dir "jediepcserver.py")))
  (defun python-mode-hook ()
    (add-to-list 'company-backends 'company-jedi))

  ;; (add-hook 'python-mode-hook 'jedi:setup)
  (add-hook 'python-mode-hook 'python-mode-hook)
  (setq jedi:complete-on-dot t))

(provide 'python-config)
;;; python-config.el ends here
