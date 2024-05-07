;;; input/layout/+vup.el -*- lexical-binding: t; -*-

;; NOTE: the evaluation loads the whole autoload/vup.el file but it doesn't really matter as other
;; functions are eagerly called in this block
;; NOTE: since this file is loaded before $DOOMDIR/config.el, the cr-rotation-style variable
;; if not default needs to be set up in $DOOMDIR/init.el

(defun +layout-remap-keys-for-vup-h ()
  (setq avy-keys '(?r ?i ?e ?a ?o ?d ?s ?n ?t ?l)
        lispy-avy-keys '(?r ?i ?e ?a ?o ?d ?s ?n ?t ?l ?y ?v ?u ?p ?o ?b ?g ?c ?f ?z ?k))
  ;; :ui window-select settings, ignoring +numbers flag for now
  (after! ace-window
    (setq aw-keys '(?r ?i ?e ?a ?o ?d ?s ?n ?t ?l)))
  (after! switch-window
    (setq switch-window-shortcut-style 'qwerty
          switch-window-qwerty-shortcuts '("r" "i" "e" "a" "o" "s" "n" "t" "l"))))

(defun +layout-remap-evil-keys-for-vup-h ()
    (setq evil-markdown-movement-bindings '((up . "t")
                                            (down . "n")
                                            (left . "s")
                                            (right . "l"))
          evil-org-movement-bindings '((up . "t")
                                       (down . "n")
                                       (left . "s")
                                       (right . "l")))

    ;; "ts" would be a little too common for an evil escape sequence
    ;; (setq evil-escape-key-sequence "sn")
    (+layout-vup-rotate-nt-bare-keymap nil '(read-expression-map))
    (+layout-vup-rotate-bare-keymaps '(evil-window-map))
    (+layout-vup-rotate-evil-keymap)

    (map! (:when (modulep! :editor multiple-cursors)
           :prefix "gz"
           :nv "n" #'evil-mc-make-cursor-move-next-line
           :nv "t" #'evil-mc-make-cursor-move-prev-line
           ;; the old toggle mapping (t) is made available both on "T" for mnemonics and
           ;; "j" as a "classic" rotation
           :nv "T" #'+multiple-cursors/evil-mc-toggle-cursors
           :nv "j" #'+multiple-cursors/evil-mc-toggle-cursors))
    ;; (after! treemacs
    ;;   (+layout-vup-rotate-nt-bare-keymap nil '(evil-treemacs-state-map)))
    (after! (:or helm ivy vertico icomplete)
      (+layout-vup-rotate-bare-keymaps
       '(minibuffer-local-map
         minibuffer-local-ns-map
         minibuffer-local-completion-map
         minibuffer-local-must-match-map
         minibuffer-local-isearch-map
         read-expression-map))
      (+layout-vup-rotate-keymaps
       '(minibuffer-local-map
         minibuffer-local-ns-map
         minibuffer-local-completion-map
         minibuffer-local-must-match-map
         minibuffer-local-isearch-map
         read-expression-map)))
    (after! ivy
      (+layout-vup-rotate-bare-keymaps '(ivy-minibuffer-map ivy-switch-buffer-map))
      (+layout-vup-rotate-keymaps '(ivy-minibuffer-map ivy-switch-buffer-map)))
    (after! helm
      (+layout-vup-rotate-bare-keymaps '(helm-map))
      (+layout-vup-rotate-keymaps '(helm-map)))
    (after! helm-rg
      (+layout-vup-rotate-bare-keymaps '(helm-rg-map))
      (+layout-vup-rotate-keymaps '(helm-rg-map)))
    (after! helm-files
      (+layout-vup-rotate-bare-keymaps '(helm-read-file-map))
      (+layout-vup-rotate-keymaps '(helm-read-file-map)))
    (after! company
      (+layout-vup-rotate-bare-keymaps '(company-active-map company-search-map)))
    (after! evil-snipe
      (+layout-vup-rotate-keymaps
       '(evil-snipe-local-mode-map evil-snipe-override-local-mode-map)))
    (after! lsp-ui
      (+layout-vup-rotate-nt-bare-keymap nil '(lsp-ui-peek-mode-map)))
    (after! (evil org evil-org-agenda)
      (+layout-vup-rotate-bare-keymaps '(org-agenda-keymap))
      (+layout-vup-rotate-keymaps '(evil-org-agenda-mode-map)))
    (after! notmuch
      ;; Without this, "s" is mapped to 'notmuch-search and
      ;; takes precedence over the evil command to go up one line
      (map! :map notmuch-common-keymap :n "s" nil)
      (map! :map notmuch-common-keymap "s" nil))
    (after! (evil info)
      (map! :map Info-mode-map
            "s" nil
            "t" nil))
    (after! (evil magit-section)
      (+layout-vup-rotate-nt-bare-keymap nil
        '(magit-section-mode-map)))
    (after! (evil magit-log)
      (+layout-vup-rotate-keymaps
        '(magit-log-read-revs-map
          magit-log-mode-map
          magit-cherry-mode-map)))
    (after! (evil magit-reflog)
      (+layout-vup-rotate-keymaps
        '(magit-reflog-mode-map)))
    (after! (evil magit-status)
      (+layout-vup-rotate-keymaps
        '(magit-status-mode-map
          magit-staged-section-map
          magit-unstaged-section-map
          magit-untracked-section-map)))
    (after! (evil magit-diff)
      (+layout-vup-rotate-keymaps
        '(magit-diff-mode-map
          magit-diff-section-base-map)))
    (after! (evil magit-process)
      (+layout-vup-rotate-keymaps
        '(magit-process-mode-map)))
    (after! (evil magit-refs)
      (+layout-vup-rotate-keymaps
        '(magit-refs-mode-map)))
    (after! (evil magit-blob)
      (+layout-vup-rotate-keymaps
        '(magit-blob-mode-map)))
    (after! (evil magit)
      (+layout-vup-rotate-keymaps
        '(magit-mode-map)))
    ;; (after! (evil magit)
      ;; Without this, "s" is mapped to 'magit-delete-thing (the old "k" for "kill") and
      ;; takes precedence over the evil command to go up one line
      ;; :nv doesn't work on this, needs to be the bare map.
      ;; This is the output of `describe-function 'magit-delete-thing` when we add :nv or :nvm
      ;; Key Bindings
      ;; evil-collection-magit-mode-map-backup-map <normal-state> x
      ;; evil-collection-magit-mode-map-backup-map <visual-state> x
      ;; evil-collection-magit-mode-map-backup-map k
      ;; evil-collection-magit-mode-map-normal-state-backup-map x
      ;; evil-collection-magit-mode-map-visual-state-backup-map x
      ;; magit-mode-map <normal-state> x
      ;; magit-mode-map <visual-state> x
      ;; magit-mode-map s
      ;; (map! :map magit-mode-map "s" nil)
      ;; (map! :map magit-mode-map "t" nil)
      ;; Even though magit bindings are part of evil-collection now,
      ;; the hook only runs on `evil-collection-magit-maps`, which is
      ;; way to short to cover all usages.
      ;; The hook is run manually on other maps
      ;; NOTE: magit-mode-map is last because other keymaps inherit from it.
      ;; Therefore to prevent a "double rotation" issue, magit-mode-map is
      ;; changed last
      ;; (+layout-vup-rotate-keymaps '(magit-mode-map)))
    (after! evil-easymotion
      ;; instead of using gs as the evilem-map we use gé to avoid conflicts with org-mode
      ;; down the road
      ;; (map! :nvm "gé" evilem-map)
      (+layout-vup-rotate-bare-keymaps '(evilem-map))))

(+layout-remap-keys-for-vup-h)
(when (modulep! :editor evil)
  (+layout-remap-evil-keys-for-vup-h)
  (add-hook! 'evil-collection-setup-hook
    (defun +layout-vup-rotate-evil-collection-keymap (_mode mode-keymaps &rest _rest)
      (+layout-vup-rotate-keymaps mode-keymaps))))
