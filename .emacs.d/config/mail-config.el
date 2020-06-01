;;; mail-config.el --- my mail config using notmuch
;;; Commentary:

;;; Code:

(use-package notmuch
  :config
  (setq notmuch-search-sort-order 'newest-first)
  (evil-define-key 'normal notmuch-search-mode-map (kbd "n") 'evil-next-line)
  (evil-define-key 'normal notmuch-search-mode-map (kbd "t") 'evil-previous-line)

  (evil-define-key 'visual notmuch-search-mode-map (kbd "n") 'evil-next-line)
  (evil-define-key 'visual notmuch-search-mode-map (kbd "t") 'evil-previous-line))


(setq send-mail-function 'sendmail-send-it
      sendmail-program "msmtp"
      mail-specify-envelope-from t
      message-sendmail-envelope-from 'header
      mail-envelope-from 'header)

(provide 'mail-config)
;;; mail-config.el ends here
