;;; org-config.el --- my org config
;;; Commentary:

;;; Code:



(add-hook 'org-mode 'turn-on-auto-fill)

(with-eval-after-load 'org
  (progn
    (setcdr (assoc "\\.pdf\\'" org-file-apps) "evince %s")))

(with-eval-after-load 'ox-latex
  (add-to-list 'org-latex-classes
               '("koma-article"
                 "\\documentclass{scrartcl}"
                 ("\\section{%s}" . "\\section*{%s}")
                 ("\\subsection{%s}" . "\\subsection*{%s}")
                 ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                 ("\\paragraph{%s}" . "\\paragraph*{%s}")
                 ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))))

(setq org-src-fontify-natively t)
(setq org-latex-listings 'minted)
(setq org-latex-minted-options
      '(("tabsize" "4")))

(setq org-confirm-babel-evaluate nil)

(org-babel-do-load-languages
 'org-babel-load-languages
 '((python . t) (rust . t) (C . t) (gnuplot . t)))

(use-package org-bullets)

(use-package ob-rust)

(defun org-if-not-special (action)
  "Run ACTION only at trivial \"org-mode\" positions."
  `(lambda ()
     (if (not (or (org-in-item-p) (org-on-heading-p)))
         (funcall ',action))))

(add-hook 'org-shiftmetaright-hook (org-if-not-special (lambda ()
                                                         (next-buffer))))
(add-hook 'org-shiftmetaleft-hook (org-if-not-special (lambda ()
                                                        (previous-buffer))))

(add-hook 'org-metaup-hook (org-if-not-special 'windmove-up))
(add-hook 'org-metaleft-hook (org-if-not-special 'windmove-left))
(add-hook 'org-metadown-hook (org-if-not-special 'windmove-down))
(add-hook 'org-metaright-hook (org-if-not-special 'windmove-right))

(defun yas-org-very-safe-expand ()
  (let ((yas-fallback-behavior 'return-nil)) (yas-expand-from-trigger-key)))

(setq yas-verbosity 0)

(add-hook 'org-mode-hook
          (lambda ()
            (make-variable-buffer-local 'yas-trigger-key)
            (setq yas-trigger-key [tab])
            (add-to-list 'org-tab-first-hook 'yas-org-very-safe-expand)
            (define-key yas-keymap [tab] 'yas-next-field-or-maybe-expand)
            (rainbow-delimiters-mode-enable)))

(defun org-move-tree (filename)
  "Move the sub-tree which contain the point to FILENAME and replace it with a link to the newly created file."
  (interactive "F")
  (org-mark-subtree)
  (let
      ((name (buffer-substring (region-beginning) (save-excursion (end-of-line) (point))))
       (xxx (buffer-substring (region-beginning) (region-end))))
    (setq name (replace-regexp-in-string "^[*]+ *" "" name))
    (delete-region (region-beginning) (region-end))
    (insert (format "#+INCLUDE: \"%s\" :minlevel 1\n" filename))
    (find-file-other-window filename)
    (insert xxx)
    (save-buffer)))

(defun fix-inline-org-images ()
  (when org-inline-image-overlays
    (org-redisplay-inline-images)))

(eval-after-load 'org
  (add-hook 'org-babel-after-execute-hook 'fix-inline-org-images))

(provide 'org-config)
;;; org-config.el ends here
