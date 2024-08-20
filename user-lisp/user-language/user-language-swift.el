;;; user-language-swift -- Swift development environment.

;;; Commentary:

;; Provides an integrated development environment for Swift, with most
;; functionality provided by the language server protocol.

;;; Code:

(require 'use-package)

(require 'user-development)
(require 'user-editing)
(require 'user-hooks)

;; Swift language support.
(use-package swift-mode
  :mode "\\.swift\\'"
  :interpreter "swift"
  :hook
  ((swift-mode-hook . auto-fill-mode)
   (swift-mode-hook . company-mode)
   (swift-mode-hook . display-line-numbers-mode)
   (swift-mode-hook . eldoc-mode)
   (swift-mode-hook . flycheck-mode)
   (swift-mode-hook . flyspell-prog-mode)
   (swift-mode-hook . hl-todo-mode)
   (swift-mode-hook . hungry-delete-mode)
   (swift-mode-hook . lsp)
   (swift-mode-hook . rainbow-delimiters-mode)
   (swift-mode-hook . smartparens-mode)
   (swift-mode-hook . user-auto-fill-only-comments)
   (swift-mode-hook . yas-minor-mode))
  :custom
  (swift-mode:fill-paragraph-entire-comment-or-string t)
  (swift-mode:basic-offset 2)
  (swift-mode:switch-case-offset 2))

;; Language server protocol support.
(use-package lsp-sourcekit)

;; Documentation support.
(use-package swift-helpful)

(provide 'user-language-swift)
;;; user-language-swift.el ends here
