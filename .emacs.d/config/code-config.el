;;; code-config.el --- general coding config
;;; Commentary:

;;; Code:

(use-package yasnippet
  :diminish yas-minor-mode
  :config
  (yas-global-mode)
  (setq yas-triggers-in-field t))

; (use-package flycheck
;   :diminish flycheck-mode
;   :config
;   (global-flycheck-mode))

;; (use-package smartparens
;;   :diminish smartparens-mode
;;   :config
;;   (add-hook 'prog-mode-hook #'smartparens-mode))

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


(use-package lsp-mode
  :ensure t
  :hook (rust-mode . lsp)
  :init
  (setq lsp-auto-guess-root t)
  (setq lsp-enable-snippet t)
  :config '(require lsp-clients)
  (setq lsp-auto-guess-root t)
  (setq lsp-enable-snippet t))

(use-package lsp-dart
  :ensure t
  :init (setq lsp-dart-sdk-dir "/data/projects/dias/dart-sdk/flutter/bin/cache/dart-sdk/"))

(use-package lsp-ui
  :ensure t
  :config
  (defun lsp-ui-doc--inline-width-string (string)
    "Returns numbers of characters that are display in STRING.
Use because `string-width' counts invisible characters."
    (with-temp-buffer
      (insert string)
      (goto-char (point-max))
      (current-column)))

  (defun lsp-ui-doc--truncate (len s &optional suffix)
    (let ((suffix (or suffix "")))
      (if (> (lsp-ui-doc--inline-width-string s) len)
          (format (concat "%s" suffix) (substring s 0 (max (- len (length suffix)) 0)))
        s)))

  (defun lsp-ui-doc--inline-width-string (string)
    "Returns numbers of characters that are display in STRING.
Use because `string-width' counts invisible characters."
    (with-temp-buffer
      (insert string)
      (goto-char (point-max))
      (current-column)))

  (defun lsp-ui-doc--inline-line-number-width ()
    "Return the line number width."
    (+ (if (bound-and-true-p display-line-numbers-mode)
           (+ 2 (line-number-display-width))
         0)
       (if (bound-and-true-p linum-mode)
           (cond ((stringp linum-format) linum-format)
                 ((eq linum-format 'dynamic)
                  (+ 2 (length (number-to-string
                                (count-lines (point-min) (point-max)))))))
         0)))


  (defun lsp-ui-doc--inline-zip (s1 s2)
    (let* ((width (- (window-body-width) (lsp-ui-doc--inline-line-number-width) 2))
           (max-s1 (- width lsp-ui-doc--inline-width 2))
           (spaces (- width (length s1) (lsp-ui-doc--inline-width-string s2))))
      (lsp-ui-doc--truncate
       width
       (concat (lsp-ui-doc--truncate max-s1 s1) (make-string (max spaces 0) ?\s) s2)))))

(use-package hover
  :ensure t)

(use-package realgud
  :ensure t)

(provide 'code-config)
;;; code-config.el ends here
