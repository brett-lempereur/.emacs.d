;;; user-operating-system -- Operating system integration.

;;; Commentary:

;; Provides operating system integration, filesystem navigation and
;; management, and terminal support.

;;; Code:

(require 'term)
(require 'use-package)

(require 'user-settings)

;; Increase the terminal buffer size
(setq-default term-buffer-maximum-size user-setting-terminal-buffer-size)

;; Add custom behaviour to the terminal
(advice-add 'term-handle-exit :after 'user-handle-terminal-exit)

;; Open a terminal with a simple keybinding
(global-set-key (kbd "C-c t") 'ansi-term)

;; Multi-window filesystem management
(use-package ranger
  :demand t
  :bind
  (("C-c r" . ranger))
  :custom
  (ranger-cleanup-eagerly t)
  (ranger-hide-cursor t)
  :config
  (ranger-override-dired-mode t))

;;; Hooks:

(defun user-open-local-terminal ()
  "Opens a local terminal with the default shell."
  (interactive)
  (ansi-term user-setting-shell))

(defun user-handle-terminal-exit (&optional process-name msg)
  "Closes terminal buffers and displays PROCESS-NAME and MSG."
  (message "Terminal: %s - %s" process-name msg)
  (kill-buffer (current-buffer)))

(provide 'user-operating-system)
;;; user-operating-system.el ends here
