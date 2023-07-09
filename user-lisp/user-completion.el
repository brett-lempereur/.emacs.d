;;; user-completion -- Minibuffer and keyboard command completion.

;;; Commentary:

;; Uses lightweight replacements for the built-in incremental narrowing
;; frameworks to provide better minibuffer and keyboard command
;; completion.

;;; Code:

(require 'use-package)

;; Text and programming language completion
(use-package company
  :commands (company-mode)
  :custom
  (company-auto-commit nil)
  (company-minimum-prefix-length 1)
  (company-show-numbers t)
  (company-idle-delay 0.1))

;; Better incremental narrowing
(use-package vertico
  :custom
  (vertico-scroll-margin 3)
  (vertico-count 10)
  (vertico-resize t)
  (vertico-cycle t)
  :config
  (vertico-mode))

;; Better incremental narrowing completion
(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles partial-completion)))))

;; Inline documentation for incremental narrowing
(use-package marginalia
  :config
  (marginalia-mode))

;; Practical incremental narrowing commands
(use-package consult
  :bind
  (("C-x b" . consult-buffer)
   ("C-c p b" . consult-project-buffer)
   ("M-y" . consult-yank-pop)))

;; Contextual actions
(use-package embark
  :bind
  (("C-." . embark-act)
   (:map embark-identifier-map
         ("." . lsp-describe-thing-at-point)
         ("E" . lsp-treemacs-errors-list)
         ("R" . lsp-treemacs-references)
         ("I" . lsp-treemacs-implementations)
         ("H" . lsp-treemacs-call-hierarchy)
         ("T" . lsp-treemacs-type-hierarchy)))
  :custom
  (embark-indicators '(embark-minimal-indicator))
  (embark-prompter #'embark-completing-read-prompter))

;; Contextual actions completion integration
(use-package embark-consult
  :after (embark consult)
  :hook
  (embark-collect-mode-hook . consult-preview-at-point-mode))

;; Interactive guided keyboard command completion
(use-package which-key
  :demand t
  :bind
  (("C-c k k" . which-key-show-top-level)
   ("C-c k ," . which-key-show-major-mode)
   ("C-c k ." . which-key-show-minor-mode-keymap))
  :config
  (which-key-mode))

(provide 'user-completion)
;;; user-completion.el ends here
