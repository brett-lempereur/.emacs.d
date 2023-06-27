;;; user-language-json -- Javascript Object Notation support.

;;; Commentary:

;; Support for editing Javascript Object Notation files and buffers.

;;; Code:

(require 'use-package)

(require 'user-development)
(require 'user-editing)

;; JavaScript object notation language
(use-package json-mode
  :mode
  ("\\.json\\'" . json-mode)
  :hook
  (json-mode-hook . aggressive-indent-mode)
  (json-mode-hook . auto-fill-mode)
  (json-mode-hook . display-line-numbers-mode)
  (json-mode-hook . flycheck-mode)
  (json-mode-hook . flyspell-prog-mode)
  (json-mode-hook . rainbow-delimiters-mode))

;; JavaScript object notation data templating language
(use-package jsonnet-mode
  :mode
  ("\\.jsonnet\\'" . jsonnet-mode)
  ("\\.libsonnet\\'" . jsonnet-mode)
  :hook
  (jsonnet-mode-hook . aggressive-indent-mode)
  (jsonnet-mode-hook . auto-fill-mode)
  (jsonnet-mode-hook . display-line-numbers-mode)
  (jsonnet-mode-hook . flycheck-mode)
  (jsonnet-mode-hook . flyspell-prog-mode)
  (jsonnet-mode-hook . rainbow-delimiters-mode))

(provide 'user-language-json)
;;; user-language-json.el ends here
