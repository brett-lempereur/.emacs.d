;;; user-language-racket -- Racket development environment.

;;; Commentary:

;; Provides an integrated development environment for Racket, with an
;; interactive REPL and test runner support.

;;; Code:

(require 'use-package)

(require 'user-development)
(require 'user-editing)
(require 'user-hooks)
(require 'user-settings)

;; Racket language support
(use-package racket-mode
  :when user-setting-racket-program
  :mode "\\.rkt\\'"
  :hook
  (racket-mode-hook . aggressive-indent-mode)
  (racket-mode-hook . auto-fill-mode)
  (racket-mode-hook . company-mode)
  (racket-mode-hook . display-line-numbers-mode)
  (racket-mode-hook . flycheck-mode)
  (racket-mode-hook . flyspell-prog-mode)
  (racket-mode-hook . format-all-mode)
  (racket-mode-hook . hl-todo-mode)
  (racket-mode-hook . idle-highlight-mode)
  (racket-mode-hook . prettify-symbols-mode)
  (racket-mode-hook . racket-xp-mode)
  (racket-mode-hook . rainbow-delimiters-mode)
  (racket-mode-hook . smartparens-mode)
  (racket-mode-hook . smartparens-strict-mode)
  (racket-mode-hook . user-auto-fill-only-comments)
  (racket-mode-hook . yas-minor-mode)
  (racket-repl-mode-hook . company-mode)
  (racket-repl-mode-hook . racket-xp-mode)
  (racket-repl-mode-hook . rainbow-delimiters-mode)
  (racket-repl-mode-hook . smartparens-mode)
  (racket-repl-mode-hook . smartparens-strict-mode)
  (racket-repl-mode-hook . yas-minor-mode)
  :custom
  (racket-program user-setting-racket-program))

;; Documentation editing
(use-package scribble-mode
  :when user-setting-racket-program
  :mode "\\.scrbl\\'"
  :hook
  (scribble-mode-hook . aggressive-indent-mode)
  (scribble-mode-hook . auto-fill-mode)
  (scribble-mode-hook . company-mode)
  (scribble-mode-hook . display-line-numbers-mode)
  (scribble-mode-hook . flycheck-mode)
  (scribble-mode-hook . flyspell-mode)
  (scribble-mode-hook . format-all-mode)
  (scribble-mode-hook . hl-todo-mode)
  (scribble-mode-hook . prettify-symbols-mode)
  (scribble-mode-hook . rainbow-delimiters-mode)
  (scribble-mode-hook . smartparens-mode)
  (scribble-mode-hook . smartparens-strict-mode)
  (scribble-mode-hook . user-auto-fill-only-comments)
  (scribble-mode-hook . yas-minor-mode))

(provide 'user-language-racket)
;;; user-language-racket.el ends here
