;;; code-config.el --- general coding config
;;; Commentary:

;;; Code:

(use-package yasnippet
  :diminish yas-minor-mode
  :config
  (yas-global-mode)
  (setq yas-triggers-in-field t))

(use-package flycheck
  :diminish flycheck-mode
  :config
  (global-flycheck-mode))

(use-package smartparens
  :diminish smartparens-mode
  :config
  (add-hook 'prog-mode-hook #'smartparens-mode))

(use-package smart-comment
  :config
  (evil-define-key 'normal global-map (kbd ";") 'smart-comment)
  (evil-define-key 'visual global-map (kbd ";") 'smart-comment))

(use-package hideshow
  :diminish hs-minor-mode
  :hook (prog-mode . hs-minor-mode)
  :config
  ;; evil hack, because hideshow sucks
  (evil-define-key 'normal global-map (kbd "hh") '(lambda () (interactive) (hs-toggle-hiding))))

(use-package rainbow-delimiters
  :demand
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package cmake-mode
  :mode ("CMakeLists\\.txt\\'" "\\.cmake\\'"))

(use-package json-reformat)

(auto-insert-mode)
(define-auto-insert
  '("\\.\\(CC?\\|cc\\|cxx\\|cpp\\|c++\\)\\'" . "C++ skeleton")
  '("Short description: "
    "/*" \n
    "Copyright (C) "(format-time-string "%Y")" by Robin Heinemann. All Rights Reserved." \n
    (file-name-nondirectory (buffer-file-name))
    " -- " (skeleton-read "Short description: ") \n
    " */" > \n \n
    "#include \""(file-name-sans-extension (file-name-nondirectory (buffer-file-name)))".h\"" \n \n \n))
(define-auto-insert
  '("\\.\\([Hh]\\|hh\\|hpp\\)\\'" . "C++ header skeleton")
  '("Short description: "
    "/*" \n
    "Copyright (C) "(format-time-string "%Y")" by Robin Heinemann. All Rights Reserved." \n
    (file-name-nondirectory (buffer-file-name))
    " -- " (skeleton-read "Short description: ") \n
    " */" > \n \n
    "#ifndef __"(upcase (file-name-sans-extension (file-name-nondirectory (buffer-file-name))))"_H_"\n
    "#define __"(upcase (file-name-sans-extension (file-name-nondirectory (buffer-file-name))))"_H_"\n \n
    > _ \n \n
    "#endif"))

(global-set-key (kbd "<f6>") 'next-error)

(electric-pair-mode 1)
(add-to-list 'electric-pair-pairs '(?\$ . ?\$))

(global-prettify-symbols-mode)
(setq prettify-symbols-unprettify-at-point t)

(setq fixme-modes '(c++-mode c-mode emacs-lisp-mode scheme-mode prog-mode))

(mapc (lambda (mode)
        (font-lock-add-keywords
         mode
         '(("\\<\\(TODO\\)" 1 'font-lock-fixme-face t)
           ("\\<\\(ToDo\\)" 1 'font-lock-fixme-face t)
           ("\\<\\(NOTE\\)" 1 'font-lock-note-face t)
           ("\\<\\(Note\\)" 1 'font-lock-note-face t))))
      fixme-modes)


(provide 'code-config)
;;; code-config.el ends here
