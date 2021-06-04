;;; user-language-clojure --- Clojure integrated development environment.

;;; Commentary:

;; This module defines an integrated development environment for Clojure, with
;; an interactive REPL and support for external test runners.

;;; Code:

(require 'use-package)

(require 'user-development)
(require 'user-editing)
(require 'user-hooks)

;; Clojure language support
(use-package clojure-mode
  :init
  (add-hook 'clojure-mode-hook #'aggressive-indent-mode)
  (add-hook 'clojure-mode-hook #'auto-fill-mode)
  (add-hook 'clojure-mode-hook #'cider-mode)
  (add-hook 'clojure-mode-hook #'company-mode)
  (add-hook 'clojure-mode-hook #'display-line-numbers-mode)
  (add-hook 'clojure-mode-hook #'enable-paredit-mode)
  (add-hook 'clojure-mode-hook #'flycheck-mode)
  (add-hook 'clojure-mode-hook #'flyspell-prog-mode)
  (add-hook 'clojure-mode-hook #'hl-todo-mode)
  (add-hook 'clojure-mode-hook #'rainbow-delimiters-mode)
  (add-hook 'clojure-mode-hook #'user-auto-fill-only-comments)
  (add-hook 'clojure-mode-hook #'yas-minor-mode)
  :custom
  (clojure-indent-style 'align-arguments))

;; Interactive clojure development
(use-package cider
  :defer t
  :init
  (add-hook 'cider-mode-hook #'cider-company-enable-fuzzy-completion)
  (add-hook 'cider-repl-mode-hook #'cider-company-enable-fuzzy-completion)
  (add-hook 'cider-repl-mode-hook #'enable-paredit-mode)
  (add-hook 'cider-repl-mode-hook #'rainbow-delimiters-mode)
  :custom
  (cider-font-lock-dynamically t)
  (cider-overlays-use-font-lock t)
  (cider-repl-display-in-current-window t)
  (cider-repl-display-help-banner nil)
  (cider-repl-pop-to-buffer-on-connect 'display-only)
  (cider-repl-prompt-function 'cider-repl-prompt-abbreviated)
  (cider-repl-require-ns-on-set t)
  (nrepl-hide-special-buffers t))

;;; Interactive functions:

(defun cider-jack-in-clj-with-profile (profile)
  "Starts and connects to a REPL with the specified profile."
  (interactive "sProfile: ")
  (message "Jacking in with profile %s" profile)
  (let* ((options (format "-A:%s" profile))
         (cider-clojure-cli-global-options options))
    (cider-jack-in-clj '())))

;;; Keyboard:

(with-eval-after-load 'clojure-mode
  (define-key clojure-mode-map (kbd "C-c M-a") 'cider-jack-in-clj-with-profile))

(with-eval-after-load 'cider-mode
  (define-key cider-mode-map (kbd "<f9>") 'cider-jack-in)
  (define-key cider-mode-map (kbd "<f10>") 'cider-switch-to-repl-buffer)
  (define-key cider-mode-map (kbd "<f11>") 'cider-eval-last-sexp)
  (define-key cider-mode-map (kbd "<f12>") 'cider-test-run-project-tests))

(provide 'user-language-clojure)
;;; user-language-clojure ends here
