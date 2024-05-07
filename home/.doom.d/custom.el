(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("467dc6fdebcf92f4d3e2a2016145ba15841987c71fbe675dcfe34ac47ffb9195" "e3daa8f18440301f3e54f2093fe15f4fe951986a8628e98dcd781efbec7a46f2" default))
 '(magit-todos-insert-after '(bottom) nil nil "Changed by setter of obsolete option `magit-todos-insert-at'")
 '(safe-local-variable-values
   '((geiser-guile-binary "guix" "repl")
     (geiser-insert-actual-lambda)
     (eval ignore-errors
      (require 'whitespace)
      (whitespace-mode 1))
     (whitespace-style face indentation)
     (eval progn
      (c-set-offset 'case-label '0)
      (c-set-offset 'innamespace '0)
      (c-set-offset 'inline-open '0))
     (geiser-repl-per-project-p . t)
     (eval progn
      (require 'lisp-mode)
      (defun emacs27-lisp-fill-paragraph
          (&optional justify)
        (interactive "P")
        (or
         (fill-comment-paragraph justify)
         (let
             ((paragraph-start
               (concat paragraph-start "\\|\\s-*\\([(;\"]\\|\\s-:\\|`(\\|#'(\\)"))
              (paragraph-separate
               (concat paragraph-separate "\\|\\s-*\".*[,\\.]$"))
              (fill-column
               (if
                   (and
                    (integerp emacs-lisp-docstring-fill-column)
                    (derived-mode-p 'emacs-lisp-mode))
                   emacs-lisp-docstring-fill-column fill-column)))
           (fill-paragraph justify))
         t))
      (setq-local fill-paragraph-function #'emacs27-lisp-fill-paragraph))
     (eval modify-syntax-entry 43 "'")
     (eval modify-syntax-entry 36 "'")
     (eval modify-syntax-entry 126 "'")
     (eval let
      ((root-dir-unexpanded
        (locate-dominating-file default-directory ".dir-locals.el")))
      (when root-dir-unexpanded
        (let*
            ((root-dir
              (file-local-name
               (expand-file-name root-dir-unexpanded)))
             (root-dir*
              (directory-file-name root-dir)))
          (unless
              (boundp 'geiser-guile-load-path)
            (defvar geiser-guile-load-path 'nil))
          (make-local-variable 'geiser-guile-load-path)
          (require 'cl-lib)
          (cl-pushnew root-dir* geiser-guile-load-path :test #'string-equal))))
     (eval with-eval-after-load 'yasnippet
      (let
          ((guix-yasnippets
            (expand-file-name "etc/snippets/yas"
                              (locate-dominating-file default-directory ".dir-locals.el"))))
        (unless
            (member guix-yasnippets yas-snippet-dirs)
          (add-to-list 'yas-snippet-dirs guix-yasnippets)
          (yas-reload-all))))
     (eval setq-local guix-directory
      (locate-dominating-file default-directory ".dir-locals.el"))
     (eval add-to-list 'completion-ignored-extensions ".go"))))
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
 '(evil-goggles-yank-face ((t (:inherit diff-changed))))
 '(ts-fold-replacement-face ((t (:foreground unspecified :box nil :inherit font-lock-comment-face :weight light)))))
