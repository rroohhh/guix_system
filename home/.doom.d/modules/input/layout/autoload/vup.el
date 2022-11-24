;;; input/layout/autoload/vup.el -*- lexical-binding: t; -*-
;;;###if (modulep! +vup)

;;;###autoload
(defun +layout-vup-rotate-nt-bare-keymap (modes keymaps)
  "Rotate [jk] with [nt] in KEYMAP."
   (evil-collection-translate-key modes keymaps
     "n" "j"
     "N" "J"
     "t" "k"
     "T" "K"
     "M" "N"
     "j" "m"
     "J" "M"
;    "m" "n"
     "k" "t"
     "K" "T"
     (kbd "C-n") (kbd "C-j")
     (kbd "C-t") (kbd "C-k")
     (kbd "C-j") (kbd "C-n")
     (kbd "C-k") (kbd "C-t")))

;;;###autoload
(defun +layout-vup-rotate-sl-bare-keymap (modes keymaps)
  "Rotate [hl] with [sl] in KEYMAP."
  (evil-collection-translate-key modes keymaps
     "s" "h"
     "S" "H"
     "h" "s"
     "H" "S"))

;;;###autoload
(defun +layout-vup-rotate-bare-keymaps (keymaps)
  "Rotate [hjkl] with [sntl] in KEYMAP."
  (+layout-vup-rotate-sl-bare-keymap nil keymaps)
  (+layout-vup-rotate-nt-bare-keymap nil keymaps))

(defun +layout-vup-rotate-keymaps (keymaps)
  "Rotate [hjkl] with [sntl] in KEYMAP."
  (+layout-vup-rotate-sl-bare-keymap '(normal motion visual operator) keymaps)
  (+layout-vup-rotate-nt-bare-keymap '(normal motion visual operator) keymaps))

;;;###autoload
(defun +layout-vup-rotate-evil-keymap ()
  "Remap evil-{normal,operator,motion,...}-state-map
  to be more natural with vup keyboard layout."
  (+layout-vup-rotate-bare-keymaps '(evil-normal-state-map evil-motion-state-map evil-visual-state-map evil-operator-state-map)))
