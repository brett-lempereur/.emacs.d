;;; user-language-go -- Go development environment.

;;; Commentary:

;; Provides an integrated development environment for Go, with most
;; functionality provided by the language server protocol.

;;; Code:

(require 'use-package)

(require 'user-development)
(require 'user-editing)
(require 'user-hooks)

;; Go language support
(use-package go-mode
  :mode "\\.go\\'"
  :hook
  (go-mode-hook . auto-fill-mode)
  (go-mode-hook . company-mode)
  (go-mode-hook . display-line-numbers-mode)
  (go-mode-hook . eldoc-mode)
  (go-mode-hook . electric-pair-mode)
  (go-mode-hook . flycheck-mode)
  (go-mode-hook . flyspell-prog-mode)
  (go-mode-hook . hl-todo-mode)
  (go-mode-hook . hungry-delete-mode)
  (go-mode-hook . lsp)
  (go-mode-hook . rainbow-delimiters-mode)
  (go-mode-hook . user-auto-fill-only-comments)
  (go-mode-hook . user-lsp-go-install-save-hooks)
  (go-mode-hook . yas-minor-mode)
  :custom
  (godoc-reuse-buffer t)
  :config
  (require 'dap-dlv-go))

;; Go test support
(use-package gotest
  :bind
  ("C-c C-c t" . go-test-current-test)
  ("C-c C-c f" . go-test-current-file)
  ("C-c C-c p" . go-test-current-project))

;;; Hooks:

(defun user-lsp-go-install-save-hooks ()
  "Install hooks to format Go code into canonical style."
  (add-hook 'before-save-hook #'lsp-format-buffer t t)
  (add-hook 'before-save-hook #'lsp-organize-imports t t))

(provide 'user-language-go)
;;; user-language-go.el ends here
