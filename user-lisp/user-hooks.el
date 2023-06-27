;;; user-hooks -- Common hook declarations.

;;; Commentary:

;; This module provides common hook definitions used in multiple major
;; modes.

;;; Code:

(defun user-auto-fill-only-comments ()
  "Set the comment auto fill only comments flag."
  (setq-local comment-auto-fill-only-comments t))

(provide 'user-hooks)
;;; user-hooks.el ends here
