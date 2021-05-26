;;; user-machine-motoko -- Configuration for my personal workstation.

;;; Commentary:

;; Configures the

;;; Code:

;; Appearance
(defconst user-setting-theme-package 'solarized-theme)
(defconst user-setting-theme 'solarized-dark)
(defconst user-setting-font "Fira Code-10")
(defconst user-setting-menu-bar-mode -1)

;; System integration
(defconst user-setting-load-path-from-shell nil)

;; Project management
(defconst user-setting-project-indexing-method 'hybrid)
(defconst user-setting-project-search-path '("~/Projects" "~/Workspace"))

;; Revision control
(defconst user-setting-repository-path '(("~/Projects" . 1) ("~/Workspace" . 1)))

(provide 'user-machine-motoko)
;;; user-machine-motoko.el ends here
