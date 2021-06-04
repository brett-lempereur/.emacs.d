;;; user-language-haskell -- Haskell development environment.

;;; Commentary:

;; This module defines an integrated development environment for
;; Haskell, with an interactive REPL.

;;; Code:

(require 'use-package)

(require 'user-development)
(require 'user-editing)

;; Language support
(use-package haskell-mode
  :init
  (add-hook 'haskell-mode-hook #'auto-fill-mode)
  (add-hook 'haskell-mode-hook #'company-mode)
  (add-hook 'haskell-mode-hook #'display-line-numbers-mode)
  (add-hook 'haskell-mode-hook #'electric-pair-mode)
  (add-hook 'haskell-mode-hook #'flycheck-mode)
  (add-hook 'haskell-mode-hook #'flyspell-prog-mode)
  (add-hook 'haskell-mode-hook #'haskell-decl-scan-mode)
  (add-hook 'haskell-mode-hook #'haskell-doc-mode)
  (add-hook 'haskell-mode-hook #'haskell-indentation-mode)
  (add-hook 'haskell-mode-hook #'hl-todo-mode)
  (add-hook 'haskell-mode-hook #'hungry-delete-mode)
  (add-hook 'haskell-mode-hook #'interactive-haskell-mode)
  (add-hook 'haskell-mode-hook #'prettify-symbols-mode)
  (add-hook 'haskell-mode-hook #'rainbow-delimiters-mode)
  (add-hook 'haskell-mode-hook #'yas-minor-mode)
  :custom
  (haskell-electric-indentation-flag t)
  (haskell-process-auto-import-loaded-modules t)
  (haskell-process-log t)
  (haskell-process-suggest-remove-import-lines t)
  (haskell-process-type 'cabal-repl)
  (haskell-stylish-on-save t))

;; Snippets
(use-package haskell-snippets)

(provide 'user-language-haskell)
;;; user-language-haskell.el ends here.
