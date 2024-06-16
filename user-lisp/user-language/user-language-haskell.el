;;; user-language-haskell -- Haskell development environment.

;;; Commentary:

;; This module provides an integrated development environment for
;; Haskell, with an interactive REPL.

;;; Code:

(require 'use-package)

(require 'user-development)
(require 'user-editing)
(require 'user-hooks)

;; Language support
(use-package haskell-mode
  :mode
  ("\\.hsc\\'" . haskell-mode)
  ("\\.l[gh]s\\'" . haskell-literate-mode)
  ("\\.hsig\\'" . haskell-mode)
  ("\\.[gh]s\\'" . haskell-mode)
  :hook
  (haskell-literate-mode-hook . auto-fill-mode)
  (haskell-literate-mode-hook . company-mode)
  (haskell-literate-mode-hook . display-line-numbers-mode)
  (haskell-literate-mode-hook . electric-pair-mode)
  (haskell-literate-mode-hook . flycheck-mode)
  (haskell-literate-mode-hook . flyspell-prog-mode)
  (haskell-literate-mode-hook . haskell-decl-scan-mode)
  (haskell-literate-mode-hook . haskell-doc-mode)
  (haskell-literate-mode-hook . haskell-indentation-mode)
  (haskell-literate-mode-hook . hl-todo-mode)
  (haskell-literate-mode-hook . hungry-delete-mode)
  (haskell-literate-mode-hook . lsp)
  (haskell-literate-mode-hook . prettify-symbols-mode)
  (haskell-literate-mode-hook . rainbow-delimiters-mode)
  (haskell-literate-mode-hook . yas-minor-mode)
  (haskell-mode-hook . auto-fill-mode)
  (haskell-mode-hook . company-mode)
  (haskell-mode-hook . display-line-numbers-mode)
  (haskell-mode-hook . electric-pair-mode)
  (haskell-mode-hook . flycheck-mode)
  (haskell-mode-hook . flyspell-prog-mode)
  (haskell-mode-hook . haskell-decl-scan-mode)
  (haskell-mode-hook . haskell-doc-mode)
  (haskell-mode-hook . haskell-indentation-mode)
  (haskell-mode-hook . hl-todo-mode)
  (haskell-mode-hook . hungry-delete-mode)
  (haskell-mode-hook . lsp)
  (haskell-mode-hook . prettify-symbols-mode)
  (haskell-mode-hook . rainbow-delimiters-mode)
  (haskell-mode-hook . user-auto-fill-only-comments)
  (haskell-mode-hook . yas-minor-mode)
  :custom
  (haskell-electric-indentation-flag t)
  (haskell-process-auto-import-loaded-modules t)
  (haskell-process-log t)
  (haskell-process-suggest-remove-import-lines t)
  (haskell-process-type 'cabal-repl)
  (haskell-stylish-on-save t))

(provide 'user-language-haskell)
;;; user-language-haskell.el ends here
