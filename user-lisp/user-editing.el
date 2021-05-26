;;; user-editing -- Text and source code editing customisation.

;;; Commentary:

;; Provides common text editing behaviour for all major modes to be as
;; modern and unsurprising as possible.  Most of the customisations in
;; this module are default settings that can be overridden on a per-mode
;; basis.

;;; Code:

(require 'use-package)

;; Text encoding
(set-language-environment "UTF-8")
(set-default-coding-systems 'utf-8)

;; Auto-fill
(setq-default comment-column 72)
(setq-default comment-empty-lines t)
(setq-default fill-column 72)

;; Indentation
(setq-default indent-tabs-mode nil)
(setq-default standard-indent 4)
(setq-default tab-always-indent 'complete)
(setq-default tab-width 4)

;; Kill ring
(setq-default backward-delete-char-untabify-method 'all)
(setq-default kill-do-not-save-duplicates t)
(setq-default kill-ring-max 1024)
(setq-default save-interprogram-paste-before-kill t)
(setq-default yank-pop-change-selection t)

;; Automatically create newlines at the end of a file.
(setq next-line-add-newlines t)

;; Backups
(setq make-backup-files nil)

;; Saving
(setq auto-save-default nil)
(setq auto-save-list-file-prefix nil)

;; Refresh buffers
(setq auto-revert-interval 1)
(global-auto-revert-mode)

;; Typing when a region is selected should replace its contents
(delete-selection-mode)

;; When a file is saved delete trailing whitespace and ensure it ends with a
;; newline
(add-hook 'before-save-hook 'delete-trailing-whitespace)
(setq require-final-newline t)

;; Visual fill mode should respect fill-column
(use-package visual-fill-column
  :init
  (add-hook 'visual-line-mode-hook 'visual-fill-column-mode))

;; Hungry deletion minor mode
(use-package hungry-delete)

;; Simple undo and redo system
(use-package undo-tree
  :config
  (global-undo-tree-mode))

;; Focusing
(use-package focus)

;;; Keyboard:

(global-set-key (kbd "C-s") 'swiper)
(global-set-key (kbd "C-c f") 'focus-mode)
(global-set-key (kbd "M-c") 'capitalize-dwim)
(global-set-key (kbd "M-l") 'downcase-dwim)
(global-set-key (kbd "M-u") 'upcase-dwim)

(provide 'user-editing)
;;; user-editing.el ends here
