;;; user-completion -- Buffer, minibuffer, and keyboard completion.

;;; Commentary:

;; Provides flexible buffer, minibuffer, and keyboard completion.

;;; Code:

(require 'use-package)

;; Better incremental narrowing.
(use-package selectrum
  :custom
  (selectrum-display-style '(vertical))
  :config
  (selectrum-mode))

;; Better filtering for incremental narrowing.
(use-package selectrum-prescient
  :custom
  (prescient-filter-method '(literal initialism))
  :config
  (prescient-persist-mode)
  (selectrum-prescient-mode))

;; Documentation for incremental narrowing.
(use-package marginalia
  :config
  (marginalia-mode))

;; Practical incremental narrowing commands.
(use-package consult)

;; Text and programming language completion.
(use-package company
  :defer t
  :custom
  (company-auto-commit nil)
  (company-minimum-prefix-length 1)
  (company-show-numbers t)
  (company-idle-delay 0.1))

;; Interactive keychord completion.
(use-package which-key
  :config
  (which-key-mode))

;;; Keyboard:

(global-set-key (kbd "C-S-a") 'embark-act)
(global-set-key (kbd "C-c k k") 'which-key-show-top-level)
(global-set-key (kbd "C-c k ,") 'which-key-show-major-mode)
(global-set-key (kbd "C-c k .") 'which-key-show-minor-mode-keymap)

(provide 'user-completion)
;;; user-completion.el ends here
