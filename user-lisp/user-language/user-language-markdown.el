;;; user-language-markdown -- Markdown syntax support.

;;; Commentary:

;; This module adds support for editing markdown documentation.

;;; Code:

(require 'use-package)

(require 'user-development)
(require 'user-editing)

;; Markdown language support
(use-package markdown-mode
  :mode
  ("\\.md\\'" . markdown-mode)
  ("\\.markdown\\'" . markdown-mode)
  ("README\\.md\\'" . gfm-mode)
  :hook
  (markdown-mode-hook . auto-fill-mode)
  (markdown-mode-hook . flycheck-mode)
  (markdown-mode-hook . flyspell-mode)
  :custom
  (markdown-asymmetric-header t)
  (markdown-command "multimarkdown")
  (markdown-enable-math t)
  (markdown-fontify-code-blocks-natively t)
  (markdown-header-scaling nil)
  (markdown-hide-urls t)
  (markdown-italic-underscore t))

(provide 'user-language-markdown)
;;; user-language-markdown.el ends here
