;;; main-config --- main non mode / package specific configuation
;;; Commentary:
;;; Code:
;; open files as sudo
(require 'secrets)

(defun sudo-find-files ()
  "Use sudo to edit files as root."
  (interactive)
  (helm-find-files-1 "/sudo::/"))

(defun kill-other-buffers ()
  "Kill all other buffers."
  (interactive)
  (mapc 'kill-buffer
        (delq (current-buffer)
              (remove-if-not 'buffer-file-name (buffer-list)))))

(defun indent-buffer ()
  "Indent the currently visited buffer."
  (interactive)
  (indent-region (point-min) (point-max)))

(defun indent-region-or-buffer ()
  "Indent a region if selected, otherwise the whole buffer."
  (interactive)
  (save-excursion
    (if (region-active-p)
        (progn
          (indent-region (region-beginning) (region-end))
          (message "Indented selected region."))
      (progn
        (indent-buffer)
        (message "Indented buffer.")))))

(setq global-mark-ring-max 5000             ; increase mark ring to contains 5000 entries
      mark-ring-max 5000                    ; increase kill ring to contains 5000 entries
      mode-require-final-newline t          ; add a newline to end of file
      tab-width 4                          ; default to 4 visible spaces to display a tab
      create-lockfiles nil  ; disable lockfiles, I only use emacs as server -> no clashes
      ;; use a global backup directory to prevent the backup from being scatted all over the place
      backup-directory-alist '(("." . "~/.emacs.d/backup"))
      backup-by-copying t    ; Don't delink hardlinks
      version-control t      ; Use version numbers on backups
      delete-old-versions t  ; Automatically delete excess backups
      kept-new-versions 20   ; how many of the newest versions to keep
      kept-old-versions 5    ; and how many of the old
      initial-scratch-message ""
      inhibit-startup-message t
      inhibit-startup-echo-area-message "robin"
      xterm-query-timeout nil
      indent-tabs-mode nil
      fill-column 120
      tab-width 4)


(save-place-mode 1)

;; M-Arrows for navigating windows
;; TODO(robin): find some key I can actually use
(windmove-default-keybindings 'meta)

(global-set-key (kbd "C-M-s") 'windmove-left)
(global-set-key (kbd "C-M-l") 'windmove-right)
(global-set-key (kbd "C-M-n") 'windmove-down)
(global-set-key (kbd "C-M-t") 'windmove-up)

(global-set-key (kbd "C-l") 'next-buffer)
(global-set-key (kbd "C-s") 'previous-buffer)

;; set utf8 as default
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-language-environment "UTF-8")
(prefer-coding-system 'utf-8)

;; auto indent on return
(global-set-key (kbd "RET") 'newline-and-indent)

;; disable menu bar
(menu-bar-mode -1)

;; disable tool bar
(tool-bar-mode -1)

;; disable scroll bar
(scroll-bar-mode -1)

;; enable time in modeline
(display-time-mode 1)

;; Prevent the cursor from blinking
(blink-cursor-mode 0)
(setq visible-cursor nil)

;; override selection
(delete-selection-mode 1)

;; always show matching brackets
(show-paren-mode 1)

;; highlight whole expression; makes it easier to find the matching braket and debug missing brakets
(setq show-paren-style 'expression)
(set-face-foreground 'show-paren-match nil)
(set-face-attribute 'show-paren-match nil
                    :weight 'normal :underline nil :overline nil :slant 'normal)

;; use abbrev-mode
(setq default-abbrev-mode t)
(diminish 'abbrev-mode)

;; whitespace config
(setq whitespace-display-mappings
      '((space-mark   ?\     [?·])
        (newline-mark ?\n    [?$ ?\n])
        (tab-mark     ?\t    [?— ?— ?— ?>])))

(setq show-trailing-whitespace 1)
(setq whitespace-style '(face trailing tabs newline tab-mark newline-mark empty))
(setq whitespace-line-column 120)

(global-whitespace-mode 1)

(setq remember-notes-auto-save-visited-file-name t
      remember-notes-buffer-name "*scratch*")

(setq initial-buffer-choice
      (lambda () (kill-buffer remember-notes-buffer-name)
        (remember-notes)))


(with-eval-after-load 'whitespace
  (set-face-foreground 'whitespace-newline  "#646464")
  (set-face-background 'whitespace-newline  nil)
  (set-face-attribute  'whitespace-newline  nil :weight 'bold)
  (set-face-foreground 'whitespace-trailing "red")
  (set-face-background 'whitespace-trailing "red")
  (set-face-foreground 'whitespace-tab      "red")
  (set-face-background 'whitespace-tab      nil)
  (set-face-attribute  'whitespace-tab      nil :weight 'bold))

;; now some packages

(use-package beacon
  :init
  (setq beacon-push-mark 0)
  (setq beacon-color "#666600")
  (setq beacon-blink-when-focused t)
  :config
  (beacon-mode 1))

(use-package real-auto-save
  :diminish real-auto-save-mode
  :hook (change-major-mode . real-auto-save-mode)
  :init
  (setq real-auto-save-interval 10)) ;; in seconds

(use-package git-gutter
  :config
  (git-gutter:linum-setup)
  (global-git-gutter-mode +1))

;; ispell config
(use-package ispell
  :config
  (add-to-list 'ispell-hunspell-dictionary-alist
               '("english-hunspell"
                 "[[:alpha:]]"
                 "[^[:alpha:]]"
                 "[']"
                 t
                 ("-d" "en_US")
                 nil
                 iso-8859-1))
  (setq ispell-program-name "hunspell"          ; Use hunspell to correct mistakes
        ispell-dictionary   "english"))         ; Default dictionary to use

(use-package flyspell)

(use-package which-key
  :config
  (which-key-mode))

(use-package dts-mode)

(use-package yaml-mode)

(use-package scad-mode)

(use-package slack
  :commands (slack-start)
  :init
  (setq slack-buffer-emojify t) ;; if you want to enable emoji, default nil
  (setq slack-prefer-current-team t)
  :config
  (slack-register-team
   :name "emacs-slack"
   :default t
   :token slack-token
   :full-and-display-names t))

;; remove trailing whitespace
(use-package ws-butler
  :diminish ws-butler-mode
  :config
  (ws-butler-global-mode))

(use-package undo-tree
  :diminish undo-tree-mode
  :init
  (setq undo-limit 10000)
  (setq undo-tree-visualizer-diff t)
  (setq undo-tree-auto-save-history t)
  (setq undo-tree-history-directory-alist
        `(("." . ,(concat "/home/robin/.cache/" "undo"))))
  :config
  (global-undo-tree-mode))

(use-package hungry-delete
  :diminish hungry-delete-mode
  :config
  (global-hungry-delete-mode 1))

(use-package fancy-battery
  :hook (after-init . fancy-battery-mode))

(use-package rainbow-mode
  :hook (prog-mode . rainbow-mode))

(use-package csv-mode)


(require 'col-highlight)
(column-highlight-mode)
(set-face-background 'col-highlight "midnight blue")

(provide 'main-config)
;;; main-config.el ends here
