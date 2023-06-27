;;; user-machine -- Machine-specific settings and customisation.

;;; Commentary:

;; Attempts to load a machine-specific configuration file.

;;; Code:

(defun user-machine-init ()
  "Return the machine-specific initialisation module."
  (intern (downcase (format "user-machine-%s"
                            (car (split-string (system-name) "\\."))))))

;; Attempt to include the machine-specific configuration file, and if
;; that fails warn the user.
(condition-case error
    (require (user-machine-init))
  (file-missing
   (warn "No machine-specific configuration found: '%s'"
         (user-machine-init))))

(provide 'user-machine)
;;; user-machine.el ends here
