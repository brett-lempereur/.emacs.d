;;; user-version-control -- Version control integration.

;;; Commentary:


;;; Code:

(require 'use-package)

;; Use an easier to type prefix for merge commands
(setq smerge-command-prefix "C-c m")

;; Distributed revision control interface.
(use-package magit
  :init
  (add-hook 'after-save-hook 'magit-after-save-refresh-status t)
  :custom
  (magit-delete-by-moving-to-trash nil)
  (magit-prefer-push-default t)
  (magit-prefer-remote-upstream t)
  (magit-pull-or-fetch t)
  (magit-repository-directories user-setting-repository-path))

;; Highlight revision control changes in the gutter
(use-package diff-hl
  :after magit
  :init
  (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh)
  :config
  (global-diff-hl-mode))

;; Git ignore files and templates
(use-package gitignore-mode)
(use-package gitignore-templates)

;;; Keyboard:

(with-eval-after-load 'magit
  (global-set-key (kbd "<f6>") 'magit-status)
  (global-set-key (kbd "<f7>") 'magit-diff-unstaged)
  (global-set-key (kbd "<f8>") 'magit-blame-addition)
  (global-set-key (kbd "C-c C-g") 'magit-dispatch)
  (global-set-key (kbd "C-c g s") 'magit-status)
  (global-set-key (kbd "C-c g b") 'magit-blame-addition)
  (global-set-key (kbd "C-c g d") 'magit-diff-unstaged))

(provide 'user-version-control)
;;; user-version-control.el ends here
