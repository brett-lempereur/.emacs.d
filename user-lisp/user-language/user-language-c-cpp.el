;;; user-language-c-cpp -- C/C++ development environment.

;;; Commentary:

;; Provides an integrated development environment for C/C++, with
;; completion and syntax checking.

;;; Code:

(require 'use-package)

(require 'user-development)
(require 'user-editing)
(require 'user-hooks)

;; Additional minor modes for the C language.
(add-hook 'c-mode-hook #'auto-fill-mode)
(add-hook 'c-mode-hook #'company-mode)
(add-hook 'c-mode-hook #'display-line-numbers-mode)
(add-hook 'c-mode-hook #'electric-pair-mode)
(add-hook 'c-mode-hook #'electric-spacing-mode)
(add-hook 'c-mode-hook #'flycheck-mode)
(add-hook 'c-mode-hook #'flyspell-prog-mode)
(add-hook 'c-mode-hook #'hl-todo-mode)
(add-hook 'c-mode-hook #'irony-mode)
(add-hook 'c-mode-hook #'rainbow-delimiters-mode)
(add-hook 'c-mode-hook #'yas-minor-mode)
(add-hook 'c-mode-hook #'user-c-set-repository-style-hook)

;; Additional minor modes for the C++ language.
(add-hook 'c++-mode-hook #'auto-fill-mode)
(add-hook 'c++-mode-hook #'company-mode)
(add-hook 'c++-mode-hook #'display-line-numbers-mode)
(add-hook 'c++-mode-hook #'electric-pair-mode)
(add-hook 'c++-mode-hook #'electric-spacing-mode)
(add-hook 'c++-mode-hook #'flycheck-mode)
(add-hook 'c++-mode-hook #'flyspell-prog-mode)
(add-hook 'c++-mode-hook #'hl-todo-mode)
(add-hook 'c++-mode-hook #'irony-mode)
(add-hook 'c++-mode-hook #'rainbow-delimiters-mode)
(add-hook 'c++-mode-hook #'yas-minor-mode)
(add-hook 'c++-mode-hook #'user-c-set-repository-style-hook)

;; Repository specific and default indentation styles.
(setq user-c-repository-styles '(("emacs" . "gnu")))
(setq c-default-style '((c-mode . "k&r") (c++-mode . "k&r")))

;; Indentation width.
(setq-default c-basic-offset 4)

;; Highlighting for additional C++ standard library types.
(add-to-list 'c++-font-lock-extra-types "unique_ptr")
(add-to-list 'c++-font-lock-extra-types "shared_ptr")
(add-to-list 'c++-font-lock-extra-types "auto_ptr")
(add-to-list 'c++-font-lock-extra-types "weak_ptr")

;; Completion back-end support.
(use-package irony
  :init
  (add-hook 'irony-mode-hook #'irony-cdb-autosetup-compile-options))

;; Completion support.
(use-package company-irony
  :after irony-mode
  :custom
  (irony-duplicate-candidates-filter t)
  :config
  (add-to-list 'company-backends 'company-irony))

;; Syntax checking support.
(use-package flycheck-irony
  :init
  (add-hook 'flycheck-mode-hook #'flycheck-irony-setup))

;;; Functions:

(defun user-c-set-repository-style-hook ()
  "Set the style of code used by a repository."
  (when-let ((style (assoc-string (projectile-project-name)
                                  user-c-repository-styles)))
    (c-set-style style)))

;;; Keyboard:

(provide 'user-language-c-cpp)
;;; user-language-c-cpp.el ends here
