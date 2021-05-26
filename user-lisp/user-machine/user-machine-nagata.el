;;; user-machine-nagata -- Apple laptop configuration.

;;; Commentary:

;; Configuration for development on my laptop.

;;; Code:

;; Use the home directory by default.
(setq default-directory (expand-file-name "~/"))

;; Appearance
(defconst user-setting-theme-package 'kaolin-themes)
(defconst user-setting-theme 'kaolin-valley-light)
(defconst user-setting-font "Monaco-11")
(defconst user-setting-menu-bar-mode t)

;; System integration
(defconst user-setting-load-path-from-shell t)

;; Project management
(defconst user-setting-project-indexing-method 'hybrid)
(defconst user-setting-project-search-path '("~/Projects" "~/Workspace"))

;; Revision control
(defconst user-setting-repository-path '(("~/Projects" . 1) ("~/Workspace" . 1)))

;; Keymap support
(setq ns-right-alternate-modifier (quote none))

(provide 'user-machine-nagata)
;;; user-machine-nagata.el ends here
