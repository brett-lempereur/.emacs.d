;;; user-appearance -- Frame, window, buffer, and modeline appearance.

;;; Commentary:

;; Provides a modern set of appearance customisations.

;;; Code:

(require 'use-package)

(require 'user-settings)

;; Hide messages when starting a new session.
(setq initial-scratch-message nil)
(setq inhibit-splash-screen t)
(setq inhibit-startup-message t)

;; Support the mouse in the console
(xterm-mouse-mode)

;; Don't blink the cursor.
(blink-cursor-mode 0)

;; Show column and line numbers in the mode line.
(column-number-mode)
(line-number-mode)

;; Use a better font in graphical frames.
(when (display-graphic-p)
  (set-frame-font user-setting-font nil t))

;; Use a custom theme in graphical frames.
(when (display-graphic-p)
  (when (and user-setting-theme-package
	     (not (require user-setting-theme-package nil 'noerror)))
    (package-install user-setting-theme-package)
    (require user-setting-theme-package))
  (load-theme user-setting-theme t))

;; Disable frame decorations.
(menu-bar-mode user-setting-menu-bar-mode)
(when (display-graphic-p)
  (scroll-bar-mode -1)
  (tool-bar-mode -1))

;; Do not use system tooltips when available.
(when (eq window-system 'x)
  (setq x-gtk-use-system-tooltips nil))

;; Increase the default graphical frame size.
(when (display-graphic-p)
  (set-frame-size (selected-frame) 85 42))

;; Show smaller gutter fringes.
(when (display-graphic-p)
  (fringe-mode '(10 . 10)))

;; Improve the appearance of the modeline
(use-package spaceline
  :custom
  (powerline-default-separator 'nil)
  :config
  (spaceline-emacs-theme)
  (spaceline-toggle-buffer-encoding-abbrev-off)
  (spaceline-toggle-buffer-size-off)
  (spaceline-toggle-flycheck-info-off)
  (spaceline-toggle-minor-modes-off)
  (spaceline-toggle-purpose-off)
  (spaceline-toggle-hud-off))

;; Highlight the current line when moving between pages
(use-package pulsar
  :hook
  (next-error-hook . pulsar-pulse-line)
  :custom
  (pulsar-pulse t)
  (pulsar-delay 0.055)
  (pulsar-iterations 10)
  (pulsar-face 'pulsar-magenta)
  (pulsar-highlight-face 'pulsar-yellow)
  :config
  (pulsar-global-mode 1))

(provide 'user-appearance)
;;; user-appearance.el ends here
