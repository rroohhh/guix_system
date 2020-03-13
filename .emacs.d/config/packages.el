;;; packages --- loads all required packages
;;; Commentary:
;;; Code:

(require 'package)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("marmalade" . "https://marmalade-repo.org/packages/")
                         ("gnu" . "https://elpa.gnu.org/packages/")))

(package-initialize)

;; bootstrap use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)
(require 'use-package-ensure)
(setq use-package-always-ensure t)

;; (use-package auto-package-update
;;   :init
;;   (setq auto-package-update-delete-old-versions t)
;;   (setq auto-package-update-hide-results t)
;;   :config
;;   (auto-package-update-maybe))

(use-package diminish
  :demand)

(use-package quelpa-use-package
  :demand)

(require 'diminish)
(require 'bind-key)

(provide 'packages)
;;; packages.el ends here
