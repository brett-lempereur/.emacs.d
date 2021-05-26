;;; user-development -- Common software development configuration.

;; Provides common integrated development environment functionality as
;; well as minor modes shared between multiple programming languages.

;;; Code:

(require 'use-package)

(require 'user-completion)

;; Function signature assistance
(setq eldoc-echo-area-use-multiline-p 'truncate-sym-name-if-fit)
(setq eldoc-timer 0)

;; Do not prompt to reload tag files
(setq tags-revert-without-query t)

;; Syntax checking and linting
(use-package flycheck
  :custom
  (flycheck-buffer-switch-check-intermediate-buffers t)
  (flycheck-check-syntax-automatically '(idle-buffer-switch idle-change save))
  (flycheck-completing-read-function #'ivy-completing-read)
  (flycheck-display-errors-delay 0.5)
  (flycheck-idle-buffer-switch-delay 0.1)
  (flycheck-idle-change-delay 0.1))

;; Snippet support
(use-package yasnippet
  :init
  (add-hook 'lsp-mode-hook 'yas-minor-mode))

;; Language server protocol client
(use-package lsp-mode
  :init
  (add-hook 'lsp-mode-hook 'lsp-enable-which-key-integration)
  :custom
  (lsp-before-save-edits t)
  (lsp-enable-file-watchers t)
  (lsp-enable-indentation t)
  (lsp-enable-on-type-formatting t)
  (lsp-enable-semantic-highlighting t)
  (lsp-enable-snippet t)
  (lsp-enable-xref t)
  (lsp-headerline-breadcrumb-enable nil)
  (lsp-keymap-prefix "C-c l")
  (lsp-prefer-flymake nil)
  (lsp-signature-render-documentation t))

;; Aggressive indentation
(use-package aggressive-indent)

;; Colour nested parentheses
(use-package rainbow-delimiters)

;; Highlight task, warning, and other comments
(use-package hl-todo)

;; Advanced parenthetical editing
(use-package paredit
  :config
  (eldoc-add-command 'paredit-backward-delete 'paredit-close-round))

;; Automatic spacing for operators
(use-package electric-spacing)

;;; Keyboard:

(with-eval-after-load 'lsp-mode
  (define-key lsp-mode-map (kbd "C-.") 'lsp-describe-thing-at-point))

(with-eval-after-load 'paredit
  (define-key paredit-mode-map (kbd "{") 'paredit-open-curly)
  (define-key paredit-mode-map (kbd "}") 'paredit-close-curly))

(provide 'user-development)
;;; user-development.el ends here
