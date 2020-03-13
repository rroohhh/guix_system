;;; Init.el --- my init file for emacs
;;; Commentary:

;;; Code:

(add-to-list 'load-path "~/.emacs.d/config/")

(load "/home/robin/.emacs.d/keylogger.el")
(keylogger-go)

(require 'packages)
(require 'main-config)
(require 'magit-config)
(require 'evil-config)
(require 'code-config)
(require 'company-config)
(require 'cpp-config)
(require 'latex-config)
(require 'mail-config)
(require 'gnuplot-config)
(require 'web-config)
(require 'my-helm-config)
(require 'rust-config)
(require 'scheme-config)
(require 'org-config)
(require 'python-config)

(require 'theme)


;;; init.el ends here
(provide 'init)
