;;; user-language-toml -- Tom's Own Markup Language support.

;;; Commentary:

;; Support for editing Tom's Own Markup Language files and buffers.

;;; Code:

(require 'use-package)

(require 'user-development)
(require 'user-editing)

;; Tom's own markup language
(use-package toml-mode
  :mode
  ("\\.toml\\'" . toml-mode)
  :hook
  (toml-mode-hook . aggressive-indent-mode)
  (toml-mode-hook . auto-fill-mode)
  (toml-mode-hook . display-line-numbers-mode)
  (toml-mode-hook . flycheck-mode)
  (toml-mode-hook . flyspell-prog-mode)
  (toml-mode-hook . hl-todo-mode)
  (toml-mode-hook . rainbow-delimiters-mode))

(provide 'user-language-toml)
;;; user-language-toml.el ends here
