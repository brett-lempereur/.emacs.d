;;; user-operating-system -- Operation system integration.

;;; Commentary:

;; Provides operating system integration, filesystem navigation and
;; management, and terminal support.

;;; Code:

(require 'use-package)

;; Increase the terminal buffer size
(setq-default term-buffer-maximum-size 8192)

;; Allow multiple terminal sessions
(use-package multi-term
  :config
  (setq multi-term-program-switches "--login"))

;; Multi-window filesystem management
(use-package ranger
  :config
  (setq ranger-cleanup-eagerly t)
  (setq ranger-hide-cursor t)
  (ranger-override-dired-mode t))

;; Load environment variables from the shell
(use-package exec-path-from-shell
  :if user-setting-load-path-from-shell
  :custom
  (exec-path-from-shell-variables '("PATH" "SHELL" "GOPATH"))
  :config
  (exec-path-from-shell-initialize))

;;; Keyboard:

(global-set-key (kbd "C-c r") 'ranger)
(global-set-key (kbd "C-c t") 'multi-term)
(global-set-key (kbd "C-c C-t") 'multi-term-dedicated-open)

(provide 'user-operating-system)
;;; user-operating-system.el ends here
