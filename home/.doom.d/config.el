;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!

;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.


(load! "keylogger.el")
(keylogger-go)

;; (defun lsp-booster--advice-json-parse (old-fn &rest args)
;;   "Try to parse bytecode instead of json."
;;   (or
;;    (when (equal (following-char) ?#)
;;      (let ((bytecode (read (current-buffer))))
;;        (when (byte-code-function-p bytecode)
;;          (funcall bytecode))))
;;    (apply old-fn args)))
;; (advice-add (if (progn (require 'json)
;;                        (fboundp 'json-parse-buffer))
;;                 'json-parse-buffer
;;               'json-read)
;;             :around
;;             #'lsp-booster--advice-json-parse)

;; (defun lsp-booster--advice-final-command (old-fn cmd &optional test?)
;;   "Prepend emacs-lsp-booster command to lsp CMD."
;;   (let ((orig-result (funcall old-fn cmd test?)))
;;     (if (and (not test?)                             ;; for check lsp-server-present?
;;              (not (file-remote-p default-directory)) ;; see lsp-resolve-final-command, it would add extra shell wrapper
;;              lsp-use-plists
;;              (not (functionp 'json-rpc-connection))  ;; native json-rpc
;;              (executable-find "emacs-lsp-booster"))
;;         (progn
;;           (when-let ((command-from-exec-path (executable-find (car orig-result))))  ;; resolve command from exec-path (in case not found in $PATH)
;;             (setcar orig-result command-from-exec-path))
;;           (message "Using emacs-lsp-booster for %s!" orig-result)
;;           (cons "emacs-lsp-booster" orig-result))
;;       orig-result)))
;; (advice-add 'lsp-resolve-final-command :around #'lsp-booster--advice-final-command)

(setq user-full-name "Robin Ole Heinemann"
      user-mail-address "robin.ole.heinemann@gmail.de")

;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
;; (setq doom-font (font-spec :family "monospace" :size 12 :weight 'semi-light)
;;       doom-variable-pitch-font (font-spec :family "sans" :size 13))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-gruvbox)
(add-hook! 'server-after-make-frame-hook #'(lambda () (set-background-color "#282828") (message "hello from here")))

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
;; (setq org-directory "~/org/")

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq! display-line-numbers-type 'relative
       auto-save-default t
       confirm-kill-emacs nil
       create-lockfiles nil
       ;; use a global backup directory to prevent the backup from being scatted all over the place
       backup-directory-alist '(("." . "~/.emacs.d/backup"))
       backup-by-copying t
       version-control t
       delete-old-versions t
       initial-scratch-message ""
       inhibit-startup-message t
       inhibit-startup-echo-area-message "robin"
       xterm-query-timeout nil
       indent-tabs-mode t
       fill-column 120
       tab-width 4
       geiser-default-implementation 'guile
;       geiser-guile-binary "/run/current-system/profile/bin/guile"
       notmuch-fcc-dirs "sent")



(after! notmuch
  (setq! notmuch-fcc-dirs "sent"))

; (message "%s" geiser-guile-binary)

(delete-selection-mode 1)

(map! :n ";" 'comment-dwim)
(map! :v ";" 'comment-dwim)
(map! :leader "," 'helm-mini)
(map! :leader "l" 'ff-find-other-file)

(after! helm-rg
  (setq! helm-rg-display-buffer-normal-method #'switch-to-buffer))

(after! evil
  (setq! evil-want-minibuffer nil)
  (setq! evil-want-C-w-delete t)
  (setq! evil-want-C-u-scroll t)
  (setq! evil-cross-lines t)
  (setq! evil-want-fine-undo t)
  (setq! evil-symbol-word-search t)
  (evil-set-undo-system 'undo-redo)
  (add-hook 'evil-insert-state-exit-hook
            (lambda () (unless (null evil-repeat-ring) (ignore-errors (if buffer-file-name (save-buffer))))))
  (map! :n "m" 'evil-ex-search-next)
  (map! :i "C-e" 'end-of-line)
  (map! :i "C-h" 'evil-delete-backward-char-and-join)
  (map! :i "C-u" 'evil-delete-line))

(setq show-paren-style 'expression)
(set-face-background 'show-paren-match "#234f46")
(set-face-foreground 'show-paren-match nil)
(set-face-attribute 'show-paren-match nil
                    :weight 'unspecified :underline nil :overline nil :slant 'normal)

(modify-face 'font-lock-comment-delimiter-face "#79740e")

(after! hl-line
  (set-face-attribute 'hl-line nil  :weight 'unspecified :background "midnight blue"))

(after! hl-todo
  (global-hl-todo-mode))

(after! notmuch
  (modify-face 'notmuch-tag-unread "#b8bb26"))

(add-hook! 'prog-mode-hook #'rainbow-delimiters-mode)

(use-package! protobuf-mode)

(use-package! beacon
  :init
  (setq beacon-push-mark 0)
  (setq beacon-color "#666600")
  (setq beacon-blink-when-focused t)
  :config
  (beacon-mode 1))


;; (after! ivy
;;   (setq! ivy-extra-directories '("../"))
;;   (setq! +ivy-buffer-preview t)
;;   (setq! counsel-find-file-ignore-regexp 'nil))

(use-package! evil-goggles
  :init
  (setq! evil-goggles-pulse t)
  (setq! evil-goggles-enable-yank t)
  (setq! evil-goggles-enable-delete t)
  (setq! evil-goggles-enable-change t)
  (setq! evil-goggles-enable-delete t)
  (setq! evil-goggles-async-duration 0.15)
  (setq! evil-goggles-blocking-duration 0.15)
  :config
  (evil-goggles-use-diff-faces)
  (modify-face 'diff-added   "#ebdbb2" "#79740e")
  (modify-face 'diff-changed "#ebdbb2" "#427b58")
  (modify-face 'diff-removed "#ebdbb2" "#9d0006")
  (evil-goggles-mode))

;; (set-background-color "#282828")

(remove-hook! 'tty-setup-hook #'xterm-mouse-mode)

(after! org
  (setq remember-notes-auto-save-visited-file-name t
        remember-notes-buffer-name "*scratch*"
        remember-notes-initial-major-mode 'text-mode))

(setq initial-buffer-choice
      (lambda () (kill-buffer remember-notes-buffer-name)
        (remember-notes)))

; (plist-put! +ligatures-extra-symbols
;             :not           "¬"
;             :return "⟼")

(setq send-mail-function 'sendmail-send-it
      sendmail-program "msmtp"
      mail-specify-envelope-from t
      message-sendmail-envelope-from 'header
      mail-envelope-from 'header)

(defadvice lsp-format-buffer (before disable-gc activate) (setq gc-cons-threshold most-negative-fixnum))
(defadvice lsp-format-buffer (after enable-gc activate) (run-at-time 1 nil lambda () (setq gc-cons-threshold doom-gc-cons-threshold)))

;; (add-hook! 'before-save-hook #'lsp-format-buffer)
(remove-hook! 'before-save-hook #'lsp-format-buffer)
(add-to-list 'auto-mode-alist '("\\.tcc\\'" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))
;; (setq +format-on-save-enabled-modes (append +format-on-save-enabled-modes '(c++-mode)))
;; (add-hook! 'after-change-major-mode-hook #'+format-enable-on-save-maybe-h)



(after! lsp-mode
  (setq lsp-modeline-diagnostics-enable t)
  (setq lsp-ui-sideline-enable nil)
  (setq! lsp-pyright-langserver-command "basedpyright")
  ;; (setq! lsp-pyright-langserver-command "~/.local/bin/pyright")
  (lsp-register-client
   (make-lsp-client :new-connection (lsp-tramp-connection '("singularity" "exec" "--app" "dls" "/containers/stable/latest" "clangd"))
                    :major-modes '(c++-mode c-mode)
                    :remote? t
                    :server-id 'clangd-remote))
  (lsp-register-client
   (make-lsp-client :new-connection (lsp-tramp-connection '("singularity" "exec" "--app" "dls" "/containers/stable/latest" "pylsp"))
                    :major-modes '(python-mode)
                    :remote? t
                    :server-id 'pyls-remote))
  (lsp-register-client
   (make-lsp-client :new-connection (lsp-tramp-connection '("singularity" "exec" "/containers/stable/asic_latest" "/hyperfast/home/rheinema/.cargo/bin/svls"))
                    :major-modes '(verilog-mode)
                    :remote? t
                    :server-id 'verilog-remote)))

(after! ein
  (setq! ein:polymode))
;; (message ein:$kernelspec-language)
(after! org
  (setcdr (assoc "\\.pdf\\'" org-file-apps) "sioyek %s"))

(after! recentf
  (setq recentf-max-saved-items 10000)
  (setq recentf-max-menu-items 10000))

(setq lsp-log-io nil)

;(after! evil-org
;  (map! :prefix "g" :map 'evil-org-mode-map "s h" nil))
;; (setq! debug-on-message "Key sequence.*")
(setq! debug-on-message "Reformatted.*")

(setq +tree-sitter-hl-enabled-modes t)

(after! verilog-mode
  (setq! verilog-indent-level 4)
  (setq! verilog-indent-level-module 4)
  (setq! verilog-indent-level-directive 0)
  (setq! verilog-indent-level-declaration 4)
  (setq! verilog-indent-level-behavioral 4)
  (setq! verilog-indent-lists nil)
  (setq! verilog-cexp-indent 4)
  (setq! verilog-auto-lineup 'all)
  (defun verilog-mode-config-hook ()
    (setq indent-tabs-mode t)
    (setq tab-width 4))
  (add-hook 'verilog-mode-hook 'verilog-mode-config-hook))

(defun c-cpp-mode-config-hook()
  (setq! c-basic-offset 'tab-width))

(add-hook 'c-mode-hook 'c-cpp-mode-config-hook)
(add-hook 'c++-mode-hook 'c-cpp-mode-config-hook)

(after! vertico
  (map! :map vertico-map "/" 'vertico-directory-enter))

(use-package! zotxt
  :init
  (defun zotxt-enable()
    (org-zotxt-mode))
  (add-hook! 'org-mode-hook 'zotxt-enable))

(use-package! org-caldav
  :init
  (setq! org-caldav-url "https://caldav.coroot.de/robin"
         org-caldav-calendar-id "abd56a4b-e0d9-3335-b894-8607e21f0bb3"
         org-caldav-inbox "/home/robin/org/caldav-inbox.org"
         org-caldav-files '("/home/robin/org/meetings.org")
         org-icalendar-timezone "Europe/Berlin"
         org-icalendar-use-scheduled '('event-if-todo 'event-if-not-todo)
         org-icalendar-with-timestamps t
         org-icalendar-include-sexps t
         org-caldav-debug-level 1
         org-caldav-autosync-idle-seconds 10)
  (org-caldav-autosync-mode 1))
;; (setq tramp-verbose 8)
(connection-local-set-profile-variables 'remote-with-singularity-dls
                                        '((tramp-remote-path . (tramp-own-remote-path tramp-default-remote-path "/hyperfast/home/rheinema/p/verilator/bin"))))
(connection-local-set-profiles
 '(:application tramp :machine "hel") 'remote-with-singularity-dls)

(connection-local-set-profiles
 '(:application tramp :machine "uranus") 'remote-with-singularity-dls)


(setq! tramp-connection-properties
       (list  (list (regexp-quote "/ssh:uranus:")
                    "tmpdir" '("/hyperfast/home/rheinema/tmp"))))

(set-face-attribute 'default nil
                    :family "Hack"
                    :height 105
                    :weight 'normal
                    :width 'normal)

(set-fontset-font t nil (font-spec :size 20 :name "Noto Sans"))

(setq auth-sources '("~/.authinfo.gpg" "~/.authinfo" "~/.netrc"))

(setq verilog-indent-lists 'nil)
