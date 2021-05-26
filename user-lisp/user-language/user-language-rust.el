;;; user-language-rust -- Rust development environment.

;;; Commentary:

;; Provides an integrated development environment for Rust, with most
;; functionality provided by the language server protocol module.

;;; Code:

(require 'use-package)

(require 'user-development)
(require 'user-editing)
(require 'user-hooks)

;; Rust language support
(use-package rust-mode
  :init
  (add-hook 'rust-mode-hook #'auto-fill-mode)
  (add-hook 'rust-mode-hook #'company-mode)
  (add-hook 'rust-mode-hook #'company-quickhelp-mode)
  (add-hook 'rust-mode-hook #'display-line-numbers-mode)
  (add-hook 'rust-mode-hook #'eldoc-mode)
  (add-hook 'rust-mode-hook #'electric-pair-mode)
  (add-hook 'rust-mode-hook #'hl-todo-mode)
  (add-hook 'rust-mode-hook #'hungry-delete-mode)
  (add-hook 'rust-mode-hook #'flycheck-mode)
  (add-hook 'rust-mode-hook #'flyspell-prog-mode)
  (add-hook 'rust-mode-hook #'lsp)
  (add-hook 'rust-mode-hook #'rainbow-delimiters-mode)
  (add-hook 'rust-mode-hook #'user-auto-fill-only-comments)
  (add-hook 'rust-mode-hook #'yas-minor-mode)
  :custom
  (rust-format-on-save t)
  (rust-indent-method-chain t))

;; Build support
(use-package cargo
  :init
  (add-hook 'rust-mode-hook #'cargo-minor-mode))

;;; Keyboard:

(with-eval-after-load 'rust-mode
  (define-key rust-mode-map (kbd "<f9>") #'rust-compile)
  (define-key rust-mode-map (kbd "<f10>") #'rust-run-clippy)
  (define-key rust-mode-map (kbd "<f12>") #'rust-test))

(with-eval-after-load 'cargo-mode
  (define-key cargo-minor-mode-map (kbd "<f11>") #'cargo-process-clean))

(provide 'user-language-rust)
;;; user-language-rust.el ends here
