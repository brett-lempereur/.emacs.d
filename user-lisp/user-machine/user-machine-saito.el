;;; user-machine-saito -- Personal development workstation customisations.

;;; Commentary:

;; Configuration for usage on my personal development workstation.

;;; Code:

(require 'user-settings)

;; Appearance customisations
(customize-set-variable 'user-setting-font "Source Code Pro-14")
(customize-set-variable 'user-setting-menu-bar-mode t)
(customize-set-variable 'user-setting-theme-package 'ef-themes)
(customize-set-variable 'user-setting-theme 'ef-kassio)

;; Racket programming language customisations
(customize-set-variable 'user-setting-racket-program "/opt/racket/bin/racket")

(provide 'user-machine-saito)
;;; user-machine-saito.el ends here
