;;; user-language-ruby -- Ruby development environment.

;;; Commentary:

;; Extends the built-in Ruby language support with better configuration,
;; completion, and development toolchain support.

;;; Code:

(require 'use-package)

(require 'user-completion)
(require 'user-development)
(require 'user-hooks)

;; Additional minor modes for the Ruby language
(add-hook 'ruby-mode-hook #'auto-fill-mode)
(add-hook 'ruby-mode-hook #'company-mode)
(add-hook 'ruby-mode-hook #'display-line-numbers-mode)
(add-hook 'ruby-mode-hook #'electric-pair-mode)
(add-hook 'ruby-mode-hook #'electric-spacing-mode)
(add-hook 'ruby-mode-hook #'flycheck-mode)
(add-hook 'ruby-mode-hook #'flyspell-prog-mode)
(add-hook 'ruby-mode-hook #'hl-todo-mode)
(add-hook 'ruby-mode-hook #'lsp)
(add-hook 'ruby-mode-hook #'rainbow-delimiters-mode)
(add-hook 'ruby-mode-hook #'user-auto-fill-only-comments)
(add-hook 'ruby-mode-hook #'yas-minor-mode)

;; Ruby language customisations
(setq ruby-insert-encoding-magic-comment nil)

;; Language server protocol customisations
(setq lsp-solargraph-use-bundler t)

;; Interactive session
(use-package inf-ruby
  :init
  (add-hook 'ruby-mode-hook 'inf-ruby-minor-mode)
  :custom
  (inf-ruby-default-implementation 'pry))

;; Ruby version manager
(use-package rvm)

;; Testing
(use-package rspec-mode
  :init
  (add-hook 'ruby-mode-hook 'rspec-mode)
  :custom
  (rspec-use-rvm t))

;;; Keyboard:

(with-eval-after-load 'inf-ruby-mode
  (define-key inf-ruby-mode-map (kbd "<f9>") #'inf-ruby-console-auto)
  (define-key inf-ruby-mode-map (kbd "<f10>") #'ruby-switch-to-inf)
  (define-key inf-ruby-mode-map (kbd "<f11>") #'ruby-send-definition))

(with-eval-after-load 'rspec-mode
  (define-key rspec-mode-map (kbd "<f12>") #'rspec-verify-all))

(provide 'user-language-ruby)
;;; user-language-ruby.el ends here
