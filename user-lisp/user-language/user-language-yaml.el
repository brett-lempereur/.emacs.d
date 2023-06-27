;;; user-language-yaml -- Yet Another Markup Language support.

;;; Commentary:

;; Support for editing Yet Another Markup Language files and buffers.

;;; Code:

(require 'use-package)

(require 'user-development)
(require 'user-editing)

;; Yet another markup language
(use-package yaml-mode
  :mode
  ("\\.yml\\'" . yaml-mode)
  ("\\.yaml\\'" . yaml-mode)
  :hook
  (yaml-mode-hook . auto-fill-mode)
  (yaml-mode-hook . display-line-numbers-mode)
  (yaml-mode-hook . flycheck-mode)
  (yaml-mode-hook . flyspell-prog-mode)
  (yaml-mode-hook . hl-todo-mode)
  (yaml-mode-hook . rainbow-delimiters-mode))

(provide 'user-language-yaml)
;;; user-language-yaml.el ends here
