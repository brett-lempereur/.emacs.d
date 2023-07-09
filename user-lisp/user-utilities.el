;;; user-utilities -- Utility functions and macros.

;;; Commentary:

;; This module provides a collection of utility functions and macros
;; used by other modules.

;;; Code:

(require 'files)

(defun user-glob (path &rest filters)
  "Return a list of files matching FILTERS in PATH."
  (let ((path (file-truename (file-name-as-directory path))))
    (mapcan
     (lambda (p) (file-expand-wildcards p t))
     (mapcar (lambda (f) (concat path f)) filters))))

(defun user-file-as-string (name)
  "Return the contents of file NAME as a string."
  (with-temp-buffer
    (find-file name)
    (buffer-substring-no-properties (point-min) (point-max))))

(defun user-lowercase-letters ()
  "Return a list of the lowercase letters of the alphabet."
  (mapcar #'byte-to-string (number-sequence 97 122)))

(provide 'user-utilities)
;;; user-utilities.el ends here
