;;; user-machine-nagata -- Personal development workstation customisations.

;;; Commentary:

;; Configuration for usage on my personal development workstation.

;;; Code:

(require 'user-settings)

;; Appearance customisations
(customize-set-variable 'user-setting-font "Source Code Pro-13")
(customize-set-variable 'user-setting-menu-bar-mode t)
(customize-set-variable 'user-setting-theme-package 'kanagawa-theme)
(customize-set-variable 'user-setting-theme 'kanagawa)

;; Racket programming language customisations
(customize-set-variable 'user-setting-racket-program "/opt/racket/bin/racket")

(provide 'user-machine-nagata)
;;; user-machine-nagata.el ends here
