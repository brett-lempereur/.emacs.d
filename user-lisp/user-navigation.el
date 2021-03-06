;;; user-navigation -- Window, frame, and buffer navigation.

;;; Commentary:

;; Provides support for navigating windows, frames, and buffers without
;; using a mouse.

;;; Code:

(require 'use-package)

(require 'user-completion)

;; Window layout stack.
(winner-mode t)

;; Character and line navigation.
(use-package avy
  :custom
  (avy-all-windows nil)
  (avy-background t)
  (avy-highlight-first t)
  (avy-style 'de-bruijn))

;; Buffer navigation.
(use-package bufler
  :custom
  (bufler-columns '("Name" "VC" "Path"))
  (bufler-reverse t)
  (bufler-use-cache nil)
  (bufler-vc-refresh t)
  (bufler-vc-state t))

;; Window navigation.
(use-package ace-window
  :custom
  (aw-ignore-current t)
  (aw-scope 'frame))

;; Advanced search.
(use-package swiper)

;; Preview navigation to a line.
(use-package goto-line-preview)

;; Contextual actions.
(use-package embark)

;; Contextual actions completion integration.
(use-package embark-consult
  :init
  (add-hook 'embark-collect-mode-hook #'consult-preview-at-point-mode))

;;; Keyboard:

(global-set-key (kbd "M-o") #'ace-window)
(global-set-key (kbd "C-'") #'avy-goto-char-timer)
(global-set-key (kbd "C-S-'") #'avy-goto-line)
(global-set-key (kbd "C-x C-b") #'bufler)
(global-set-key (kbd "C-x b") #'bufler-switch-buffer)
(global-set-key (kbd "A-g g") #'goto-line-preview)

(provide 'user-navigation)
;;; user-navigation.el ends here
