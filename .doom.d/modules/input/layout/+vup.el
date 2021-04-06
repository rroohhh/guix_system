;;; input/layout/+bepo.el -*- lexical-binding: t; -*-

;; NOTE: the evaluation loads the whole autoload/vup.el file but it doesn't really matter as other
;; functions are eagerly called in this block
;; NOTE: since this file is loaded before $DOOMDIR/config.el, the cr-rotation-style variable
;; if not default needs to be set up in $DOOMDIR/init.el
(fset 'doom-vup--evil-collection-hook
      (doom-vup-rotate-collection-keymaps-h-builder))
(add-hook 'evil-collection-setup-hook #'doom-vup--evil-collection-hook)

(add-transient-hook! 'doom-init-modules-hook
  (setq avy-keys '(?r ?i ?e ?a ?o ?d ?s ?n ?t ?l)
        lispy-avy-keys '(?r ?i ?e ?a ?o ?d ?s ?n ?t ?l ?y ?v ?u ?p ?o ?b ?g ?c ?f ?z ?k))

  ;; :ui window-select settings, ignoring +numbers flag for now
  (after! ace-window
    (setq aw-keys '(?r ?i ?e ?a ?o ?d ?s ?n ?t ?l)))
  (after! switch-window
    (setq switch-window-shortcut-style 'qwerty
          switch-window-qwerty-shortcuts '("r" "i" "e" "a" "o" "s" "n" "t" "l"))))

(when (featurep! :editor evil)
  (add-transient-hook! 'doom-init-modules-hook
    ;; "ts" would be a little too common for an evil escape sequence
    (setq evil-escape-key-sequence "sn")
    (setq evil-markdown-movement-bindings '((up . "t")
                                            (down . "n")
                                            (left . "s")
                                            (right . "l"))
          evil-org-movement-bindings '((up . "t")
                                       (down . "n")
                                       (left . "s")
                                       (right . "l")))
    (doom-vup-rotate-nt-bare-keymap '(read-expression-map))
    (doom-vup-rotate-bare-keymap '(evil-window-map))
    (doom-vup-rotate-evil-keymap)

    (map! (:when (featurep! :editor multiple-cursors)
           :prefix "gz"
           :nv "n" #'evil-mc-make-cursor-move-next-line
           :nv "t" #'evil-mc-make-cursor-move-prev-line
           ;; the old toggle mapping (t) is made available both on "T" for mnemonics and
           ;; "j" as a "classic" rotation
           :nv "T" #'+multiple-cursors/evil-mc-toggle-cursors
           :nv "j" #'+multiple-cursors/evil-mc-toggle-cursors))
    (after! treemacs
      (doom-vup-rotate-nt-bare-keymap '(evil-treemacs-state-map)))
    (after! (:or helm ivy selectrum icomplete)
      (doom-vup-rotate-bare-keymap
       '(minibuffer-local-map
         minibuffer-local-ns-map
         minibuffer-local-completion-map
         minibuffer-local-must-match-map
         minibuffer-local-isearch-map
         read-expression-map)))
    (after! ivy
      (doom-vup-rotate-bare-keymap '(ivy-minibuffer-map ivy-switch-buffer-map)))
    (after! helm
      (doom-vup-rotate-bare-keymap '(helm-map)))
    (after! helm-rg
      (doom-vup-rotate-bare-keymap '(helm-rg-map)))
    (after! helm-files
      (doom-vup-rotate-bare-keymap '(helm-read-file-map)))
    (after! company
      (doom-vup-rotate-bare-keymap '(company-active-map company-search-map)))
    (after! evil-snipe
      (doom-vup--evil-collection-hook
       nil
       '(evil-snipe-local-mode-map evil-snipe-override-local-mode-map)))
    (after! lsp-ui
      (doom-vup-rotate-nt-bare-keymap '(lsp-ui-peek-mode-map)))
    (after! (evil org evil-org-agenda)
      (doom-vup-rotate-bare-keymap '(org-agenda-keymap))
      (doom-vup--evil-collection-hook nil '(evil-org-agenda-mode-map)))
    (after! notmuch
      ;; Without this, "s" is mapped to 'notmuch-search and
      ;; takes precedence over the evil command to go up one line
      (map! :map notmuch-common-keymap :n "s" nil)
      (map! :map notmuch-common-keymap "s" nil))
    (after! (evil magit)
      (doom-vup-rotate-nt-bare-keymap
       '(magit-mode-map
         magit-diff-section-base-map
         magit-staged-section-map
         magit-unstaged-section-map
         magit-untracked-section-map))
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
      (map! :map magit-mode-map "s" nil)
      (map! :map magit-mode-map "t" nil)
      ;; Even though magit bindings are part of evil-collection now,
      ;; the hook only runs on `evil-collection-magit-maps`, which is
      ;; way to short to cover all usages.
      ;; The hook is run manually on other maps
      ;; NOTE: magit-mode-map is last because other keymaps inherit from it.
      ;; Therefore to prevent a "double rotation" issue, magit-mode-map is
      ;; changed last
      (doom-vup--evil-collection-hook
       nil
       '(magit-cherry-mode-map
         magit-blob-mode-map
         magit-diff-mode-map
         magit-log-mode-map
         magit-log-select-mode-map
         magit-reflog-mode-map
         magit-status-mode-map
         magit-log-read-revs-map
         magit-process-mode-map
         magit-refs-mode-map
         magit-mode-map)))
    (after! evil-easymotion
      ;; instead of using gs as the evilem-map we use gé to avoid conflicts with org-mode
      ;; down the road
      ;; (map! :nvm "gé" evilem-map)
      (doom-vup-rotate-bare-keymap '(evilem-map)))))
