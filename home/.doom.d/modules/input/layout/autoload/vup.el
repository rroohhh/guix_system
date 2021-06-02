;;; input/layout/autoload/vup.el -*- lexical-binding: t; -*-

;;;###autoload
(defun doom-vup-rotate-nt-bare-keymap (keymaps)
  "Rotate [jk] with [nt] in KEYMAP."
  (dolist (keymap keymaps)
    (evil-collection-translate-key nil keymap
      "n" "j"
      "N" "J"
      "t" "k"
      "T" "K"
      "m" "n"
      "M" "N"
      "j" "m"
      "J" "M"
      "k" "t"
      "K" "T"
      (kbd "C-n") (kbd "C-j")
      (kbd "C-t") (kbd "C-k")
      (kbd "C-j") (kbd "C-n")
      (kbd "C-k") (kbd "C-t"))))

;;;###autoload
(defun doom-vup-rotate-sl-bare-keymap (keymaps)
  "Rotate [hl] with [sl] in KEYMAP."
  (dolist (keymap keymaps)
    (progn
      (evil-collection-translate-key nil keymap
        "s" "h"
        "S" "H"
        "h" "s"
        "H" "S"))))

;;;###autoload
(defun doom-vup-rotate-bare-keymap (keymaps)
  "Rotate [hjkl] with [sntl] in KEYMAP."
  (doom-vup-rotate-sl-bare-keymap keymaps)
  (doom-vup-rotate-nt-bare-keymap keymaps))

;;;###autoload
(defun doom-vup-rotate-evil-keymap ()
  "Remap evil-{normal,operator,motion,...}-state-map
  to be more natural with vup keyboard layout."
  (doom-vup-rotate-bare-keymap '(evil-normal-state-map evil-motion-state-map evil-visual-state-map evil-operator-state-map)))

;;;###autoload
(defun doom-vup-rotate-collection-keymaps-h-builder ()
  "Build a hook that remaps evil-collection customizations to be more natural
  with vup keyboard layout."
  (let* ((doom-bepo-hook (lambda (_mode mode-keymaps &rest _rest)
                           (dolist (keymap mode-keymaps)
                             (evil-collection-translate-key '(normal motion visual operator) keymap
                               "s" "h"
                               "S" "H"
                               "n" "j"
                               "N" "J"
                               "m" "n"
                               "M" "N"
                               "j" "m"
                               "J" "M"
                               "t" "k"
                               "T" "K"
                               "k" "t"
                               "K" "T"
                               "h" "s"
                               "H" "S")))))
    doom-bepo-hook))
