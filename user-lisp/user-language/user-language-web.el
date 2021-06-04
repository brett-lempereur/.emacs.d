;;; user-language-web -- Web development environment.

;;; Commentary:

;; Provides an integrated web development environment for common web
;; development languages and frameworks.

;;; Code:

(require 'css-mode)

(require 'user-development)
(require 'user-editing)
(require 'user-hooks)

;; Additional stylesheet mode hooks
(add-hook 'css-mode-hook #'auto-fill-mode)
(add-hook 'css-mode-hook #'company-mode)
(add-hook 'css-mode-hook #'display-line-numbers-mode)
(add-hook 'css-mode-hook #'eldoc-mode)
(add-hook 'css-mode-hook #'electric-pair-mode)
(add-hook 'css-mode-hook #'emmet-mode)
(add-hook 'css-mode-hook #'flycheck-mode)
(add-hook 'css-mode-hook #'flyspell-prog-mode)
(add-hook 'css-mode-hook #'hl-todo-mode)
(add-hook 'css-mode-hook #'hungry-delete-mode)
(add-hook 'css-mode-hook #'npm-mode)
(add-hook 'css-mode-hook #'rainbow-delimiters-mode)
(add-hook 'css-mode-hook #'user-auto-fill-only-comments)
(add-hook 'css-mode-hook #'yas-minor-mode)

;; Node package manager support
(use-package npm-mode)

;; Typescript language support
(use-package typescript-mode
  :mode ("\\.ts\\'" "\\.tsx\\'")
  :init
  (add-hook 'typescript-mode-hook #'auto-fill-mode)
  (add-hook 'typescript-mode-hook #'company-mode)
  (add-hook 'typescript-mode-hook #'display-line-numbers-mode)
  (add-hook 'typescript-mode-hook #'eldoc-mode)
  (add-hook 'typescript-mode-hook #'electric-pair-mode)
  (add-hook 'typescript-mode-hook #'flycheck-mode)
  (add-hook 'typescript-mode-hook #'flyspell-prog-mode)
  (add-hook 'typescript-mode-hook #'hl-todo-mode)
  (add-hook 'typescript-mode-hook #'hungry-delete-mode)
  (add-hook 'typescript-mode-hook #'npm-mode)
  (add-hook 'typescript-mode-hook #'rainbow-delimiters-mode)
  (add-hook 'typescript-mode-hook #'user-auto-fill-only-comments)
  (add-hook 'typescript-mode-hook #'yas-minor-mode))

;; Typescript development environment
(use-package tide
  :init
  (add-hook 'typescript-mode-hook #'tide-setup)
  (add-hook 'typescript-mode-hook #'tide-hl-identifier-mode))

;; Javascript and typescript formatting
(use-package prettier-js
  :custom
  (prettier-js-width-mode 80))

;; Web and template language support
(use-package web-mode
  :mode ("\\.html?\\'" "\\.phtml\\'" "\\.tpl\\.php\\'" "\\.[agj]sp\\'"
         "\\.as[cp]x\\'" "\\.erb\\'" "\\.mustache\\'" "\\.djhtml\\'")
  :init
  (add-hook 'web-mode-hook #'auto-fill-mode)
  (add-hook 'web-mode-hook #'company-mode)
  (add-hook 'web-mode-hook #'display-line-numbers-mode)
  (add-hook 'web-mode-hook #'eldoc-mode)
  (add-hook 'web-mode-hook #'electric-pair-mode)
  (add-hook 'web-mode-hook #'emmet-mode)
  (add-hook 'web-mode-hook #'flycheck-mode)
  (add-hook 'web-mode-hook #'flyspell-prog-mode)
  (add-hook 'web-mode-hook #'hl-todo-mode)
  (add-hook 'web-mode-hook #'hungry-delete-mode)
  (add-hook 'web-mode-hook #'npm-mode)
  (add-hook 'web-mode-hook #'rainbow-delimiters-mode)
  (add-hook 'web-mode-hook #'yas-minor-mode)
  :custom
  (web-mode-enable-current-element-highlight t)
  (web-mode-enable-element-content-fontification t))

;; Zen coding for web development
(use-package emmet-mode
  :custom
  (emmet-move-cursor-between-quotes t))

(provide 'user-language-web)
;;; user-language-web.el ends here
