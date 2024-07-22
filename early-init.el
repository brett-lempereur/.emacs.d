;;; early-init -- Editor environment initialisation script.

;;; Commentary:

;; This initialisation script is evaluated before the main initialisation script
;; to ensure that package management is configured and the load path includes
;; user modules.

;;; Code:

(require 'comp)
(require 'package)

;; Configure native compilation.
(customize-set-variable 'native-comp-async-query-on-exit t)
(customize-set-variable 'native-comp-async-report-warnings-errors 'silent)

;; Native compilation
(when (boundp 'native-comp-eln-load-path)
  (startup-redirect-eln-cache
   (expand-file-name "local/eln-cache" user-emacs-directory)))

;; Never load site-specific files
(setq inhibit-default-init t)

;; Configure package archives
(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")))

;; Change the directory where packages are installed
(setq package-user-dir (locate-user-emacs-file "remote-lisp"))

;; Initialise the package management system
(package-initialize)

;; Keep the configuration directory clean
(setq no-littering-etc-directory (locate-user-emacs-file "local/config/"))
(setq no-littering-var-directory (locate-user-emacs-file "local/data/"))
(unless (package-installed-p 'no-littering)
  (package-refresh-contents nil)
  (package-install 'no-littering))
(require 'no-littering)

;; Ensure that higher-level package management support is installed
(unless (package-installed-p 'use-package)
  (package-refresh-contents nil)
  (package-install 'use-package))
(require 'use-package)

;; Always install packages when they are not already installed
(setq use-package-always-ensure t)

;; Do not attempt to be clever about hook names
(setq use-package-hook-name-suffix nil)

;; Add user modules to the load path
(add-to-list 'load-path (locate-user-emacs-file "user-lisp"))
(add-to-list 'load-path (locate-user-emacs-file "user-lisp/user-machine"))
(add-to-list 'load-path (locate-user-emacs-file "user-lisp/user-language"))

(provide 'early-init)
;;; early-init.el ends here
