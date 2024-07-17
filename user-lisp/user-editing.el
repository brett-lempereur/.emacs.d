;;; user-editing -- Text and source code editing customisation.

;;; Commentary:

;; Provides common text editing behaviour for all major modes to be as
;; modern and unsurprising as possible.  Most of the customisations in
;; this module are default settings that can be overridden on a per-mode
;; basis.

;;; Code:

(require 'autorevert)
(require 'use-package)
(require 'winner)

;; Text encoding
(set-language-environment "UTF-8")
(set-default-coding-systems 'utf-8)

;; Auto-fill
(setq-default comment-column 72)
(setq-default comment-empty-lines t)
(setq-default fill-column 79)

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

;; Preserve and restore window layouts
(winner-mode t)

;; Case-changing commands should behave sensibly
(global-set-key (kbd "M-c") 'capitalize-dwim)
(global-set-key (kbd "M-l") 'downcase-dwim)
(global-set-key (kbd "M-u") 'upcase-dwim)

;; Visual-fill mode should respect the fill-column setting
(use-package visual-fill-column
  :hook
  (visual-line-mode-hook . visual-fill-column-mode))

;; Allow languages to opt-in to hungry-deletion
(use-package hungry-delete
  :commands hungry-delete-mode)

;; Cleanup whitespace at the end of lines when saving
(use-package ws-butler
  :commands ws-butler-mode)

;; Shortcut commands on selected text
(use-package selected
  :commands selected-minor-mode
  :bind
  ((:map selected-keymap
         ("q" . selected-off)
         ("u" . upcase-region)
         ("d" . downcase-region)
         ("c" . capitalize-region)
         ("w" . count-words-region)
         ("m" . apply-macro-to-region-lines))
   (:map selected-org-mode-map
         ("t" . org-table-convert-region)))
  :init
  (defvar selected-org-mode-map (make-sparse-keymap)))

;; Extension and replacement shortcut commands
(use-package crux
  :bind
  ("C-k" . crux-smart-kill-line)
  ("C-S-RET" . crux-smart-open-line-above)
  ("S-RET" . crux-smart-open-line)
  ("C-c b c" . crux-cleanup-buffer-or-region)
  ("C-c t" . crux-visit-term-buffer))

;; Use a simple undo and redo system
(use-package undo-tree
  :custom
  (undo-tree-history-directory-alist '(("." . "~/.emacs.d/local/data/undo-tree")))
  :config
  (global-undo-tree-mode))

;; Allow
(use-package focus
  :bind
  (("C-c f" . focus-mode)))

;; Easily navigate to other windows
(use-package ace-window
  :bind
  (("M-o" . ace-window))
  :custom
  (aw-scope 'frame))

;; Advanced buffer search
(use-package swiper
  :bind
  (("C-s" . swiper)))

;; Character and line navigation
(use-package avy
  :bind
  (("C-'" . avy-goto-char-timer)
   ("C-S-'" . avy-goto-line))
  :custom
  (avy-all-windows nil)
  (avy-background t)
  (avy-highlight-first t)
  (avy-style 'de-bruijn))

;; Navigate to a line number with a preview
(use-package goto-line-preview
  :bind
  (("A-g g" . goto-line-preview)))

(provide 'user-editing)
;;; user-editing.el ends here
