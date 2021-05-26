;;; user-language-markup -- Markup language support.

;;; Commentary:

;; This module adds support for documentation and configuration markup
;; languages.

;;; Code:

(require 'use-package)

(require 'user-editing)

;; Markdown language
(use-package markdown-mode
  :mode (("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode)
         ("README\\.md\\'" . gfm-mode))
  :init
  (add-hook 'markdown-mode-hook #'auto-fill-mode)
  (add-hook 'markdown-mode-hook #'flycheck-mode)
  (add-hook 'markdown-mode-hook #'flyspell-mode)
  :custom
  (markdown-asymmetric-header t)
  (markdown-command "multimarkdown")
  (markdown-enable-math t)
  (markdown-fontify-code-blocks-natively t)
  (markdown-header-scaling nil)
  (markdown-hide-urls t)
  (markdown-italic-underscore t))

;; JavaScript object notation language
(use-package json-mode
  :mode (("\\.json\\'" . json-mode))
  :init
  (add-hook 'json-mode-hook #'aggressive-indent-mode)
  (add-hook 'json-mode-hook #'auto-fill-mode)
  (add-hook 'json-mode-hook #'display-line-numbers-mode)
  (add-hook 'json-mode-hook #'flycheck-mode)
  (add-hook 'json-mode-hook #'flyspell-prog-mode)
  (add-hook 'json-mode-hook #'rainbow-delimiters-mode))

;; JavaScript object notation data templating language
(use-package jsonnet-mode
  :mode (("\\.jsonnet\\'" . jsonnet-mode)
         ("\\.libsonnet\\'" . jsonnet-mode))
  :init
  (add-hook 'jsonnet-mode-hook #'aggressive-indent-mode)
  (add-hook 'jsonnet-mode-hook #'auto-fill-mode)
  (add-hook 'jsonnet-mode-hook #'display-line-numbers-mode)
  (add-hook 'jsonnet-mode-hook #'flycheck-mode)
  (add-hook 'jsonnet-mode-hook #'flyspell-prog-mode)
  (add-hook 'jsonnet-mode-hook #'rainbow-delimiters-mode))

;; Yet another markup language
(use-package yaml-mode
  :mode (("\\.yml\\'" . yaml-mode)
         ("\\.yaml\\'" . yaml-mode))
  :init
  (add-hook 'yaml-mode-hook #'auto-fill-mode)
  (add-hook 'yaml-mode-hook #'display-line-numbers-mode)
  (add-hook 'yaml-mode-hook #'flycheck-mode)
  (add-hook 'yaml-mode-hook #'flyspell-prog-mode)
  (add-hook 'yaml-mode-hook #'hl-todo-mode)
  (add-hook 'yaml-mode-hook #'rainbow-delimiters-mode))

;; Tom's own markup language
(use-package toml-mode
  :mode (("\\.toml\\'" . toml-mode))
  :init
  (add-hook 'toml-mode-hook #'aggressive-indent-mode)
  (add-hook 'toml-mode-hook #'auto-fill-mode)
  (add-hook 'toml-mode-hook #'display-line-numbers-mode)
  (add-hook 'toml-mode-hook #'flycheck-mode)
  (add-hook 'toml-mode-hook #'flyspell-prog-mode)
  (add-hook 'toml-mode-hook #'hl-todo-mode)
  (add-hook 'toml-mode-hook #'rainbow-delimiters-mode))

;; Comma-separated value files
(use-package csv-mode
  :mode (("\\.csv\\'" . csv-mode))
  :init
  (add-hook 'csv-mode-hook #'display-line-numbers-mode)
  (add-hook 'csv-mode-hook #'flycheck-mode)
  (add-hook 'csv-mode-hook #'flyspell-prog-mode)
  (add-hook 'csv-mode-hook #'rainbow-delimiters-mode))

(provide 'user-language-markup)
;;; user-language-markup ends here
