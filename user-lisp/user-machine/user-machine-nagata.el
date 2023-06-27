;;; user-machine-nagata -- Personal development laptop customisations.

;;; Commentary:

;; Configuration for usage on my personal development laptop.

;;; Code:

(require 'user-settings)

;; Appearance customisations
(customize-set-variable 'user-setting-font "Monaco-13")
(customize-set-variable 'user-setting-theme-package 'ample-theme)
(customize-set-variable 'user-setting-theme 'ample-flat)
(customize-set-variable 'user-setting-menu-bar-mode t)

;; Racket programming language customisations
(customize-set-variable 'user-setting-racket-program "/Users/brett/.nix-profile/bin/racket")

(provide 'user-machine-nagata)
;;; user-machine-nagata.el ends here
