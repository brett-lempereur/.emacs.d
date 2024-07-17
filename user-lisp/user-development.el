;;; user-development -- Common software development configuration.

;;; Commentary:

;; Provides common integrated development environment functionality as
;; well as minor modes shared between multiple programming languages.

;;; Code:

(require 'eldoc)
(require 'etags)

(require 'use-package)

;; Function signature assistance
(setq eldoc-echo-area-use-multiline-p 'truncate-sym-name-if-fit)
(setq eldoc-timer 0)

;; Do not prompt to reload tag files
(setq tags-revert-without-query t)

;; Use an easier to type prefix for merge commands
(customize-set-variable 'smerge-command-prefix "C-c m")

;; Language server protocol client
(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :hook
  (lsp-mode-hook . lsp-enable-which-key-integration)
  :bind
  ("C-;" . lsp-describe-thing-at-point)
  ("C-," . lsp-find-definition)
  ("C-<" . lsp-treemacs-references)
  :custom
  (lsp-before-save-edits t)
  (lsp-enable-file-watchers t)
  (lsp-enable-indentation t)
  (lsp-enable-on-type-formatting t)
  (lsp-enable-semantic-highlighting t)
  (lsp-enable-snippet t)
  (lsp-enable-xref t)
  (lsp-headerline-breadcrumb-enable nil)
  (lsp-keymap-prefix "C-c l")
  (lsp-lens-enable nil)
  (lsp-prefer-flymake nil)
  (lsp-signature-render-documentation t))

;; Debug adapter protocol client
(use-package dap-mode
  :commands (dap-debug dap-debug-edit-template)
  :after lsp-mode
  :hook
  (dap-stopped-hook . user-dap-stopped-hook)
  :custom
  (dap-auto-configure-features '(sessions locals controls tooltip))
  (dap-auto-configure-mode))

;; Tree view support
(use-package treemacs)

;; Support for browsing code structures in a tree view
(use-package lsp-treemacs
  :commands
  (lsp-treemacs-errors-list
   lsp-treemacs-references
   lsp-treemacs-implementations
   lsp-treemacs-call-hierarchy
   lsp-treemacs-type-hierarchy)
  :custom
  (lsp-treemacs-error-list-current-project-only t))

;; Syntax checking and linting
(use-package flycheck
  :commands flycheck-mode
  :custom
  (flycheck-buffer-switch-check-intermediate-buffers t)
  (flycheck-check-syntax-automatically '(idle-buffer-switch idle-change save))
  (flycheck-completing-read-function #'ivy-completing-read)
  (flycheck-display-errors-delay 0.5)
  (flycheck-idle-buffer-switch-delay 0.1)
  (flycheck-idle-change-delay 0.1))

;; Snippet support and automatic completion
(use-package yasnippet
  :commands yas-minor-mode
  :bind
  (("C-c y e" . yas-expand)
   ("C-c y n" . yas-new-snippet)
   ("C-c y ." . yas-next-field-or-maybe-expand)
   ("C-c y ," . yas-prev-field)
   ("C-c y g" . yas-exit-snippet))
  :custom
  (yas-snippet-dirs (list (locate-user-emacs-file "user-snippets")))
  :config
  (yas-reload-all))

;; Better paranthetical editing support
(use-package smartparens
  :commands smartparens-mode
  :custom
  (sp-base-key-bindings 'paredit)
  (sp-hybrid-kill-entire-symbol t)
  :config
  (require 'smartparens-config))

;; Automatically format code buffers
(use-package format-all
  :bind
  (("C-c b f" . format-all-buffer)))

;; Automatically insert spaces around operators while typing
(use-package electric-spacing
  :commands electric-spacing-mode)

;; Automatically reindent code while typing
(use-package aggressive-indent
  :commands aggressive-indent-mode)

;; Highlight task, warning, and other comments
(use-package hl-todo
  :commands hl-todo-mode
  :custom
  (hl-todo-require-punctuation t)
  (hl-todo-wrap-movement t))

;; Visually differentiate between nested parentheses
(use-package rainbow-delimiters
  :commands rainbow-delimiters-mode)

;; Distributed revision control interface
(use-package magit
  :demand t
  :hook
  (after-save-hook . magit-after-save-refresh-status)
  :bind
  (("C-c C-g" . magit-dispatch)
   ("C-c g s" . magit-status)
   ("C-c g b" . magit-blame-addition)
   ("C-c g d" . magit-diff-unstaged))
  :custom
  (magit-delete-by-moving-to-trash nil)
  (magit-prefer-push-default t)
  (magit-prefer-remote-upstream t)
  (magit-pull-or-fetch t)
  (magit-repository-directories user-setting-project-paths))

;; Highlight revision control changes in the gutter.
(use-package diff-hl
  :after magit
  :demand t
  :hook
  (magit-post-refresh-hook . diff-hl-magit-post-refresh)
  :config
  (global-diff-hl-mode))

;; Git configuration and metadata file support
(use-package git-modes
  :mode
  (("/.gitattributes\\'" . gitattributes-mode)
   ("/.git/info/attributes\\'" . gitattributes-mode)
   ("/git/attributes\\'" . gitattributes-mode)
   ("/.gitconfig\\'" . gitconfig-mode)
   ("/.git/config\\'" . gitconfig-mode)
   ("/git/config\\'" . gitconfig-mode)
   ("/.gitignore\\'" . gitignore-mode)))

;; Git ignore configuration file templates
(use-package gitignore-templates
  :commands (gitignore-templates-insert gitignore-templates-new-file))

;; Project management
(use-package projectile
  :demand t
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :custom
  (projectile-enable-idle-timer t)
  (projectile-indexing-method user-setting-project-indexing-method)
  (projectile-project-search-path user-setting-project-paths)
  (projectile-sort-order 'recently-active)
  :config
  (projectile-mode))

;; Advanced project search support
(use-package ag
  :custom
  (ag-highlight-search t))

;; Better formatting of search results
(use-package winnow
  :hook
  (ag-mode . winnow-mode))

;; Developer documentation
(use-package devdocs
  :commands (devdocs-lookup)
  :bind
  ("C-h ," . devdocs-lookup))

;;; Hooks:

(defun user-dap-stopped-hook ()
  "Displays the debug interface when a breakpoint is triggered."
  (call-interactively #'dap-hydra))

(provide 'user-development)
;;; user-development.el ends here
