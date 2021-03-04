;;; Init.el --- my init file for emacs
;;; Commentary:

;;; Code:

(add-to-list 'load-path "~/.emacs.d/config/")

(load "/home/robin/.emacs.d/keylogger.el")
(keylogger-go)

(require 'packages)
(require 'main-config)
(require 'magit-config)
(require 'evil-config)
(require 'code-config)
(require 'company-config)
(require 'cpp-config)
(require 'latex-config)
(require 'mail-config)
(require 'gnuplot-config)
(require 'web-config)
(require 'my-helm-config)
(require 'rust-config)
(require 'scheme-config)
(require 'org-config)
(require 'python-config)

(require 'theme)


;;; init.el ends here
(provide 'init)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(csv-separators '(";"))
 '(evil-collection-setup-minibuffer t)
 '(helm-ag-base-command "rg --vimgrep --no-heading")
 '(helm-minibuffer-history-key "M-p")
 '(indent-tabs-mode nil)
 '(lsp-enable-snippet t)
 '(package-selected-packages
   '(tide typescript-mode nix-mode glsl-mode vline swift-mode elm-mode realgud eglot gruvbox hover lsp-ui lsp-dart csv-mode helm-rg yaml-mode ws-butler wolfram-mode web-mode web-beautify vlf term-cursor spaceline smartparens smart-comment slack scad-mode real-auto-save rainbow-mode rainbow-delimiters racer quelpa-use-package persistent-scratch ox-pandoc org-bullets oberon ob-rust notmuch nlinum-relative modern-cpp-font-lock magit json-mode jedi irony-eldoc hungry-delete highlight-sexp helm-swoop helm-projectile helm-ag gruvbox-theme go-mode gnuplot-mode git-gutter fzf flycheck-ycmd flycheck-rust flycheck-irony fancy-battery expand-region evil-terminal-cursor-changer evil-surround evil-org evil-mc evil-matchit evil-leader evil-iedit-state evil-goggles evil-god-state evil-collection dts-mode diminish counsel company-ycmd company-try-hard company-tern company-statistics company-rtags company-quickhelp company-lsp company-jedi company-irony-c-headers company-irony company-auctex commenter cmake-mode cmake-ide clang-format cargo beacon avy auto-package-update))
 '(safe-local-variable-values
   '((git-commit-major-mode . git-commit-elisp-text-mode)
     (eval ignore-errors
           (require 'whitespace)
           (whitespace-mode 1))
     (whitespace-style face indentation)
     (eval progn
           (c-set-offset 'case-label '0)
           (c-set-offset 'innamespace '0)
           (c-set-offset 'inline-open '0))
     (eval let
           ((root-dir-unexpanded
             (locate-dominating-file default-directory ".dir-locals.el")))
           (when root-dir-unexpanded
             (let*
                 ((root-dir
                   (expand-file-name root-dir-unexpanded))
                  (root-dir*
                   (directory-file-name root-dir)))
               (unless
                   (boundp 'geiser-guile-load-path)
                 (defvar geiser-guile-load-path 'nil))
               (make-local-variable 'geiser-guile-load-path)
               (require 'cl-lib)
               (cl-pushnew root-dir* geiser-guile-load-path :test #'string-equal))))
     (eval setq-local guix-directory
           (locate-dominating-file default-directory ".dir-locals.el"))
     (encoding . utf-8)
     (cmake-ide-dir . "build")
     (eval modify-syntax-entry 43 "'")
     (eval modify-syntax-entry 36 "'")
     (eval modify-syntax-entry 126 "'")))
 '(undo-limit 160000))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(evil-goggles-change-face ((t (:inherit diff-removed))))
 '(evil-goggles-delete-face ((t (:inherit diff-removed))))
 '(evil-goggles-paste-face ((t (:inherit diff-added))))
 '(evil-goggles-undo-redo-add-face ((t (:inherit diff-added))))
 '(evil-goggles-undo-redo-change-face ((t (:inherit diff-changed))))
 '(evil-goggles-undo-redo-remove-face ((t (:inherit diff-removed))))
 '(evil-goggles-yank-face ((t (:inherit diff-changed)))))
