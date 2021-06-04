;;; user-language-ocaml -- OCaml development environment.

;;; Commentary:

;; This module defines an integrated development environment for OCaml, with
;; an interactive REPL and support for external build systems.

;;; Code:

(require 'use-package)

(require 'user-development)
(require 'user-editing)
(require 'user-hooks)

;; Language support
(use-package tuareg
  :init
  (add-hook 'tuareg-mode-hook #'auto-fill-mode)
  (add-hook 'tuareg-mode-hook #'company-mode)
  (add-hook 'tuareg-mode-hook #'display-line-numbers-mode)
  (add-hook 'tuareg-mode-hook #'eldoc-mode)
  (add-hook 'tuareg-mode-hook #'electric-pair-mode)
  (add-hook 'tuareg-mode-hook #'flycheck-mode)
  (add-hook 'tuareg-mode-hook #'flyspell-prog-mode)
  (add-hook 'tuareg-mode-hook #'hl-todo-mode)
  (add-hook 'tuareg-mode-hook #'hungry-delete-mode)
  (add-hook 'tuareg-mode-hook #'prettify-symbols-mode)
  (add-hook 'tuareg-mode-hook #'rainbow-delimiters-mode)
  (add-hook 'tuareg-mode-hook #'user-auto-fill-only-comments)
  (add-hook 'tuareg-mode-hook #'yas-minor-mode)
  :custom
  (tuareg-browser #'eww-browse-url)
  (tuareg-electric-close-vector t)
  (tuareg-highlight-all-operators t)
  (tuareg-prettify-symbols-full t))

;; Completion and refactoring
(use-package merlin
  :init
  (add-hook 'tuareg-mode-hook #'merlin-mode)
  :custom
  (merlin-completion-with-doc t)
  (merlin-favourite-caml-mode 'tuareg-mode))

;; Build system
(use-package dune
  :init
  (add-hook 'dune-mode-hook #'aggressive-indent-mode)
  (add-hook 'dune-mode-hook #'auto-fill-mode)
  (add-hook 'dune-mode-hook #'cider-mode)
  (add-hook 'dune-mode-hook #'company-mode)
  (add-hook 'dune-mode-hook #'display-line-numbers-mode)
  (add-hook 'dune-mode-hook #'enable-paredit-mode)
  (add-hook 'dune-mode-hook #'flycheck-mode)
  (add-hook 'dune-mode-hook #'flyspell-prog-mode)
  (add-hook 'dune-mode-hook #'hl-todo-mode)
  (add-hook 'dune-mode-hook #'rainbow-delimiters-mode)
  (add-hook 'dune-mode-hook #'user-auto-fill-only-comments)
  (add-hook 'dune-mode-hook #'yas-minor-mode))

;; Interactive shell
(use-package utop
  :init
  (add-hook 'tuareg-mode-hook #'utop-minor-mode)
  :custom
  (utop-command "opam config exec -- dune utop . -- -emacs"))

;; Indentation
(use-package ocp-indent
  :init
  (add-hook 'tuareg-mode-hook #'user-tuareg-mode-ocp-indent-hook))

;;; Hooks:

(defun user-tuareg-mode-ocp-indent-hook ()
  "Enables custom indentation support for OCaml buffers."
  (add-hook 'before-save-hook #'ocp-indent-buffer nil t)
  (setq indent-line-function #'ocp-indent-line)
  (setq indent-region-function #'ocp-indent-region))

;;; Keyboard:

(with-eval-after-load 'utop
  (define-key utop-minor-mode-map (kbd "<f9>") 'utop))

(provide 'user-language-ocaml)
;;; user-language-ocaml.el ends here
