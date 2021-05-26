;;; user-networking -- Transparent and interactive network clients.

;;; Commentary:

;; Provides transparent and explicit access to remote resources, as well
;; as documentation and other network utilities.

;;; Code:

(require 'use-package)

;; Transparent remote access
(setq tramp-default-method 'ssh)

;; Interactive web service interface
(use-package restclient)

;; Literal web server interface
(use-package verb
  :custom
  (verb-auto-show-headers-buffer 'when-empty)
  (verb-trim-body-end "[ \t\n\r]+"))

;; Hypertext transfer protocol reference
(use-package know-your-http-well)

;;; Interactive functions:

(defun restclient-scratch-buffer ()
  "Create a new scratch buffer for an interactive web session."
  (interactive)
  (let ((buffer (generate-new-buffer "*Rest-Scratch*")))
    (switch-to-buffer buffer)
    (restclient-mode)))

;;; Keyboard:

(global-set-key (kbd "C-c n r") 'restclient-scratch-buffer)

(provide 'user-networking)
;;; user-networking.el ends here
