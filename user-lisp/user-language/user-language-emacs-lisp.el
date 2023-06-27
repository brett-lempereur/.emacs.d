;;; user-language-emacs-lisp -- Emacs lisp development environment.

;;; Commentary:

;; Configures and provides minor modes that make writing Emacs Lisp a
;; more pleasant experience.

;;; Code:

(require 'elisp-mode)
(require 'use-package)

(require 'user-development)
(require 'user-editing)

;; Hooks
(add-hook 'emacs-lisp-mode-hook #'aggressive-indent-mode)
(add-hook 'emacs-lisp-mode-hook #'auto-fill-mode)
(add-hook 'emacs-lisp-mode-hook #'company-mode)
(add-hook 'emacs-lisp-mode-hook #'display-line-numbers-mode)
(add-hook 'emacs-lisp-mode-hook #'flycheck-mode)
(add-hook 'emacs-lisp-mode-hook #'flyspell-prog-mode)
(add-hook 'emacs-lisp-mode-hook #'hl-todo-mode)
(add-hook 'emacs-lisp-mode-hook #'rainbow-delimiters-mode)
(add-hook 'emacs-lisp-mode-hook #'smartparens-mode)
(add-hook 'emacs-lisp-mode-hook #'show-smartparens-mode)
(add-hook 'emacs-lisp-mode-hook #'smartparens-strict-mode)

;; Better documentation browsing and rendering
(use-package helpful
  :bind
  (:map emacs-lisp-mode-map
        ("C-c C-d" . helpful-at-point)
        ("C-h f" . helpful-callable)
        ("C-h v" . helpful-variable)
        ("C-h k" . helpful-key))
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable))

;; Interactive code valuation
(use-package eros
  :hook
  ((emacs-lisp-mode-hook . eros-mode)))

(provide 'user-language-emacs-lisp)
;; user-language-emacs-lisp.el ends here
