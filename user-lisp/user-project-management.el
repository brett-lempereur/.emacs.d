;;; user-project-management -- Project management configuration.

;;; Commentary:

;; Adds flexible project management and searching support.

;;; Code:

(require 'use-package)

(require 'user-completion)

;; Project management
(use-package projectile
  :custom
  (projectile-completion-system 'ivy)
  (projectile-enable-idle-timer t)
  (projectile-indexing-method user-setting-project-indexing-method)
  (projectile-project-search-path user-setting-project-search-path)
  (projectile-sort-order 'recently-active)
  :config
  (projectile-global-mode))

;; Search
(use-package ag
  :custom
  (ag-highlight-search t))

;; Search result formatting
(use-package winnow
  :hook (ag-mode . winnow-mode))

;;; Keyboard:

(with-eval-after-load 'projectile
  (global-set-key (kbd "C-c p") 'projectile-command-map)
  (global-set-key (kbd "<f1>") #'projectile-switch-project)
  (global-set-key (kbd "<f2>") #'projectile-find-file)
  (global-set-key (kbd "<f3>") #'projectile-ag)
  (global-set-key (kbd "<f4>") #'projectile-find-tag)
  (global-set-key (kbd "<f5>") #'projectile-dired)
  (global-set-key (kbd "C-c p") #'projectile-command-map))

(provide 'user-project-management)
;;; user-project-management.el ends here
