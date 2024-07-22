;;; user-networking -- Network clients and debugging.

;;; Commentary:

;; This module provides tools for interacting with and debugging network
;; services.

;;; Code:

(require 'user-development)
(require 'user-editing)

;; Representational state transfer client and debugging mode.
(use-package restclient
  :mode (("\\.http\\'" . restclient-mode))
  :hook
  ((restclient-mode-hook . company-mode)
   (restclient-mode-hook . display-line-numbers-mode)
   (restclient-mode-hook . flyspell-prog-mode)
   (restclient-mode-hook . hl-todo-mode)
   (restclient-mode-hook . hungry-delete-mode)
   (restclient-mode-hook . rainbow-delimiters-mode)
   (restclient-mode-hook . smartparens-mode)
   (restclient-mode-hook . yas-minor-mode))
  :custom
  (restclient-same-buffer-response t)
  (restclient-same-buffer-response-name "*REST Response*")
  (restclient-inhibit-cookies t))

;; Hypertext transfer protocol reference.
(use-package know-your-http-well
  :bind
  (("C-c h h m" . http-method)
   ("C-c h h h" . http-header)
   ("C-c h h s" . http-status-code)
   ("C-c h h r" . http-relation)))

(provide 'user-networking)
;;; user-networking.el ends here
