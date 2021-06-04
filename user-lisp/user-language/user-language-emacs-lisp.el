;;; user-language-emacs-lisp -- Emacs lisp development environment.

;;; Commentary:

;; Configures and provides minor modes that make writing Emacs Lisp more
;; pleasant.

;;; Code:

(require 'use-package)

(require 'user-development)
(require 'user-editing)

;; Hooks
(add-hook 'emacs-lisp-mode-hook #'aggressive-indent-mode)
(add-hook 'emacs-lisp-mode-hook #'auto-fill-mode)
(add-hook 'emacs-lisp-mode-hook #'company-mode)
(add-hook 'emacs-lisp-mode-hook #'display-line-numbers-mode)
(add-hook 'emacs-lisp-mode-hook #'enable-paredit-mode)
(add-hook 'emacs-lisp-mode-hook #'flycheck-mode)
(add-hook 'emacs-lisp-mode-hook #'flyspell-prog-mode)
(add-hook 'emacs-lisp-mode-hook #'hl-todo-mode)
(add-hook 'emacs-lisp-mode-hook #'rainbow-delimiters-mode)

;; Documentation
(use-package helpful
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable))

;; Code evaluation
(use-package eros
  :init
  (add-hook 'emacs-lisp-mode-hook 'eros-mode))

;;; Keyboard:

(with-eval-after-load 'emacs-lisp-mode
  (define-key emacs-lisp-mode-map (kbd "C-c C-d") #'helpful-at-point)
  (define-key emacs-lisp-mode-map (kbd "C-h f") #'helpful-callable)
  (define-key emacs-lisp-mode-map (kbd "C-h v") #'helpful-variable)
  (define-key emacs-lisp-mode-map (kbd "C-h k") #'helpful-key))

(provide 'user-language-emacs-lisp)
;;; user-language-emacs-lisp.el ends here
