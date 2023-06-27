;;; user-language-common-lisp -- Common LISP development environment.

;;; Commentary:

;; Provides an integrated development environment for Common Lisp, with
;; an interactive REPL and documentation support.

;;; Code:

(require 'lisp-mode)
(require 'use-package)

(require 'user-development)
(require 'user-editing)
(require 'user-hooks)
(require 'user-settings)

;; Additional minor modes for Common LISP languages
(add-hook 'lisp-mode-hook #'aggressive-indent-mode)
(add-hook 'lisp-mode-hook #'auto-fill-mode)
(add-hook 'lisp-mode-hook #'company-mode)
(add-hook 'lisp-mode-hook #'display-line-numbers-mode)
(add-hook 'lisp-mode-hook #'flycheck-mode)
(add-hook 'lisp-mode-hook #'flyspell-prog-mode)
(add-hook 'lisp-mode-hook #'hl-todo-mode)
(add-hook 'lisp-mode-hook #'rainbow-delimiters-mode)
(add-hook 'lisp-mode-hook #'slime-mode)
(add-hook 'lisp-mode-hook #'smartparens-mode)
(add-hook 'lisp-mode-hook #'smartparens-strict-mode)
(add-hook 'lisp-mode-hook #'show-smartparens-mode)
(add-hook 'lisp-mode-hook #'user-auto-fill-only-comments)
(add-hook 'lisp-mode-hook #'yas-minor-mode)

;; Integrated development environment
(use-package slime
  :when user-setting-lisp-program
  :commands (slime slime-mode)
  :hook
  (slime-repl-mode-hook . aggressive-indent-mode)
  (slime-repl-mode-hook . company-mode)
  (slime-repl-mode-hook . rainbow-delimiters-mode)
  (slime-repl-mode-hook . smartparens-mode)
  (slime-repl-mode-hook . smartparens-strict-mode)
  (slime-repl-mode-hook . show-smartparens-mode)
  (slime-repl-mode-hook . yas-minor-mode)
  :custom
  (inferior-lisp-program user-setting-lisp-program)
  :config
  (add-to-list 'slime-contribs 'slime-asdf)
  (add-to-list 'slime-contribs 'slime-autodoc)
  (add-to-list 'slime-contribs 'slime-c-p-c)
  (add-to-list 'slime-contribs 'slime-editing-commands)
  (add-to-list 'slime-contribs 'slime-mdot-fu)
  (add-to-list 'slime-contribs 'slime-presentations)
  (add-to-list 'slime-contribs 'slime-references)
  (add-to-list 'slime-contribs 'slime-repl)
  (add-to-list 'slime-contribs 'slime-xref-browser))

(provide 'user-language-common-lisp)
;;; user-language-common-lisp.el ends here
