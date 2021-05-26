;;; user-navigation -- Window, frame, and buffer navigation.

;;; Commentary:

;; Provides support for navigating windows, frames, and buffers without
;; using a mouse.

;;; Code:

(require 'use-package)

(require 'user-completion)

;;
(winner-mode t)

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

;;
(use-package embark)

;;
(use-package embark-consult
  :init
  (add-hook 'embark-collect-mode-hook #'consult-preview-at-point-mode))

;;; Keyboard

(global-set-key (kbd "M-o") #'ace-window)
(global-set-key (kbd "C-x C-b") #'bufler)
(global-set-key (kbd "C-x b") #'bufler-switch-buffer)
(global-set-key (kbd "A-g g") #'goto-line-preview)

(provide 'user-navigation)
;;; user-navigation.el ends here
