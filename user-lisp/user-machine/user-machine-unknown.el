;;; user-machine-unknown -- Fallback configuration for unknown machines.

;;; Commentary:

;; All new machine-specific variables must be added to this file, as it is both
;; a fallback configuration and template for new machines.

;;; Code:

;; Appearance
(defconst user-setting-theme-package 'dracula-theme)
(defconst user-setting-theme 'dracula)
(defconst user-setting-font "Fira Code-10")
(defconst user-setting-menu-bar-mode -1)

;; System integration
(defconst user-setting-load-path-from-shell nil)

;; Project management
(defconst user-setting-project-indexing-method 'hybrid)
(defconst user-setting-project-search-path '("~/Projects" "~/Workspace"))

;; Revision control
(defconst user-setting-repository-path '(("~/Projects" . 1) ("~/Workspace" . 1)))

;; Note taking
(defconst user-setting-notebook-directory "~/Documents/Notes/")

(provide 'user-machine-unknown)
;;; user-machine-unknown.el ends here
