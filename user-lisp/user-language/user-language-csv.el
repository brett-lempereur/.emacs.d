;;; user-language-csv -- Comma-separated value support.

;;; Commentary:

;; Support for editing comma-separated value files and buffers.

;;; Code:

(require 'use-package)

(require 'user-development)
(require 'user-editing)

;; Comma-separated value files
(use-package csv-mode
  :mode
  ("\\.csv\\'" . csv-mode)
  :hook
  (csv-mode-hook . display-line-numbers-mode)
  (csv-mode-hook . flycheck-mode)
  (csv-mode-hook . flyspell-prog-mode)
  (csv-mode-hook . rainbow-delimiters-mode))

(provide 'user-language-csv)
;;; user-language-csv.el ends here
