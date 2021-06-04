;;; user-language-racket -- Racket development environment.

;;; Commentary:

;; Provides an integrated development environment for Racket, with an
;; interactive REPL and test runner support.

;;; Code:

(require 'use-package)

(require 'user-development)
(require 'user-editing)
(require 'user-hooks)

;; Racket language support
(use-package racket-mode
  :init
  (add-hook 'racket-mode-hook #'aggressive-indent-mode)
  (add-hook 'racket-mode-hook #'auto-fill-mode)
  (add-hook 'racket-mode-hook #'company-mode)
  (add-hook 'racket-mode-hook #'display-line-numbers-mode)
  (add-hook 'racket-mode-hook #'enable-paredit-mode)
  (add-hook 'racket-mode-hook #'flycheck-mode)
  (add-hook 'racket-mode-hook #'flyspell-prog-mode)
  (add-hook 'racket-mode-hook #'hl-todo-mode)
  (add-hook 'racket-mode-hook #'racket-xp-mode)
  (add-hook 'racket-mode-hook #'rainbow-delimiters-mode)
  (add-hook 'racket-mode-hook #'user-auto-fill-only-comments)
  (add-hook 'racket-mode-hook #'yas-minor-mode)
  :custom
  (racket-xp-after-change-refresh-delay 5))

;;; Keyboard:

(with-eval-after-load 'racket-mode
  (define-key racket-mode-map (kbd "<f9>") 'racket-repl)
  (define-key racket-mode-map (kbd "<f10>") 'racket-run-and-switch-to-repl)
  (define-key racket-mode-map (kbd "<f11>") 'racket-eval-last-sexp)
  (define-key racket-mode-map (kbd "<f12>") 'racket-test))

(provide 'user-language-racket)
;;; user-language-racket.el ends here
