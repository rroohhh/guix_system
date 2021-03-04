;;; theme.el --- my theme, gruvbox with some tweaks
;;; Commentary:

;;; Code:

(add-to-list 'custom-theme-load-path "~/.emacs.d/config/")

(use-package gruvbox-theme
  :ensure t)

(load-theme 'gruvbox t)

(use-package powerline
  :demand t
  :init
  (setq powerline-default-separator 'utf-8)
  :config
  (powerline-default-theme))

(use-package spaceline
  :demand
  :init
  (setq spaceline-highlight-face-func 'spaceline-highlight-face-evil-state)
  :config
  (spaceline-emacs-theme)
  (spaceline-helm-mode)

  (spaceline-toggle-minor-modes-off)
  (spaceline-toggle-buffer-position-off)
  (spaceline-toggle-input-method-off)
  (spaceline-toggle-buffer-encoding-abbrev-off)
  (spaceline-toggle-flycheck-error-off)
  (spaceline-toggle-flycheck-warning-off)
  (spaceline-toggle-flycheck-info-off)

  (spaceline-toggle-evil-state)

  (modify-face 'spaceline-evil-normal "#282828" "#a09880" nil t nil t nil nil)
  (modify-face 'spaceline-evil-insert "#282828" "#80a490" nil t nil t nil nil)
  (modify-face 'spaceline-evil-visual "#282828" "#f0bc30" nil t nil t nil nil)
  (modify-face 'spaceline-evil-replace "#282828" "#88bc78" nil t nil t nil nil))

(make-face 'font-lock-fixme-face)
(make-face 'font-lock-note-face)

(modify-face 'font-lock-fixme-face "#ff0000" nil nil t nil t nil nil)
(modify-face 'font-lock-note-face "Dark Green" nil nil t nil t nil nil)

(modify-face 'font-lock-comment-delimiter-face "#79740e")

;; tweaks, mainly for evil-goggles
(modify-face 'diff-added   "#ebdbb2" "#79740e")
(modify-face 'diff-changed "#ebdbb2" "#427b58")
(modify-face 'diff-removed "#ebdbb2" "#9d0006")

(modify-face 'rainbow-delimiters-depth-1-face "#fb4933")
(modify-face 'rainbow-delimiters-depth-2-face "#b8bb26")
(modify-face 'rainbow-delimiters-depth-3-face "#fe8019")
(modify-face 'rainbow-delimiters-depth-4-face "#83a598")
(modify-face 'rainbow-delimiters-depth-5-face "#fabd2f")
(modify-face 'rainbow-delimiters-depth-6-face "#d3869b")
(modify-face 'rainbow-delimiters-depth-7-face "#cc241d")
(modify-face 'rainbow-delimiters-depth-8-face "#98971a")
(modify-face 'rainbow-delimiters-depth-9-face "#d65d0e")


(modify-face 'helm-buffer-directory "#83a598")
(modify-face 'helm-buffer-process   "#83a598")
(modify-face 'helm-moccur-buffer    "#83a598")
(modify-face 'helm-ff-directory     "#458588")
(modify-face 'helm-buffer-size      "#fabd2f")

;; use a highlight line
(global-hl-line-mode 1)
(set-face-background 'hl-line "midnight blue")

;; some color tweaks
(set-cursor-color "#ff0033")

;; default font
(set-face-attribute 'default nil
                    :family "Hack"
                    :height 110
                    :weight 'normal
                    :width 'normal)

(provide 'theme)
;;; theme.el ends here
