;;; user-language-rust -- Rust development environment.

;;; Commentary:

;; Provides an integrated development environment for Rust, with most
;; functionality provided by the language server protocol.

;;; Code:

(require 'use-package)

(require 'user-development)
(require 'user-editing)
(require 'user-hooks)

;; Rust language support
(use-package rust-mode
  :mode "\\.rs\\'"
  :bind
  (("C-c C-c c" . rust-compile)
   ("C-c C-c l" . rust-run-clippy)
   ("C-c C-c t" . rust-test))
  :hook
  ((rust-mode-hook . auto-fill-mode)
   (rust-mode-hook . cargo-minor-mode)
   (rust-mode-hook . company-mode)
   (rust-mode-hook . display-line-numbers-mode)
   (rust-mode-hook . eldoc-mode)
   (rust-mode-hook . flycheck-mode)
   (rust-mode-hook . flyspell-prog-mode)
   (rust-mode-hook . hl-todo-mode)
   (rust-mode-hook . hungry-delete-mode)
   (rust-mode-hook . lsp)
   (rust-mode-hook . rainbow-delimiters-mode)
   (rust-mode-hook . smartparens-mode)
   (rust-mode-hook . user-auto-fill-only-comments)
   (rust-mode-hook . yas-minor-mode))
  :custom
  (lsp-rust-server 'rust-analyzer)
  (rust-format-on-save t)
  (rust-indent-method-chain t))

;; Build system support
(use-package cargo
  :commands cargo-minor-mode)

(provide 'user-language-rust)
;;; user-language-rust.el ends here
