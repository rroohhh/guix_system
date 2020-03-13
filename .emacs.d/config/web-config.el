;;; web-config.el --- web development config
;;; Commentary:

;;; Code:

(use-package web-beautify
  :config
  (evil-define-key 'normal js-mode-map (kbd "öö") 'web-beautify-js)
  (evil-define-key 'normal css-mode-map (kbd "öö") 'web-beautify-css)
  (evil-define-key 'normal html-mode-map (kbd "öö") 'web-beautify-html))

(use-package web-mode
  :config
  ;; (add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
  ;; (add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
  ;; (add-to-list 'auto-mode-alist '("\\.[agj]sp\\'" . web-mode))
  ;; (add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
  ;; (add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
  ;; (add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
  ;; (add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
  ;; (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
  ;; (add-to-list 'auto-mode-alist '("\\.css?\\'" . web-mode))
  ;; (add-to-list 'auto-mode-alist '("\\.js?\\'" . web-mode))
  (setq web-mode-enable-auto-pairing t)
  (setq web-mode-enable-auto-closing t)
  (setq web-mode-enable-part-face t)
  (setq web-mode-enable-comment-keywords t)
  (setq web-mode-enable-heredoc-fontification t)
  (setq web-mode-enable-block-face t)
  (setq web-mode-enable-css-colorization t))

(provide 'web-config)
;;; web-config.el ends here
