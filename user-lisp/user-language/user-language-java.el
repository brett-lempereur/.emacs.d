;;; user-language-java -- Java integrated development environment.

;;; Commentary:

;; Provides an integrated development environment for Java that uses the
;; Eclipse JDT LSP for most functionality.

;;; Code:

(require 'cc-mode)
(require 'use-package)

(require 'user-development)
(require 'user-editing)
(require 'user-hooks)
(require 'user-settings)

;; Additional minor modes for Java programming
(add-hook 'java-mode-hook #'auto-fill-mode)
(add-hook 'java-mode-hook #'company-mode)
(add-hook 'java-mode-hook #'display-line-numbers-mode)
(add-hook 'java-mode-hook #'eldoc-mode)
(add-hook 'java-mode-hook #'flycheck-mode)
(add-hook 'java-mode-hook #'flyspell-prog-mode)
(add-hook 'java-mode-hook #'hl-todo-mode)
(add-hook 'java-mode-hook #'hungry-delete-mode)
(add-hook 'java-mode-hook #'lsp-deferred)
(add-hook 'java-mode-hook #'rainbow-delimiters-mode)
(add-hook 'java-mode-hook #'smartparens-mode)
(add-hook 'java-mode-hook #'user-auto-fill-only-comments)
(add-hook 'java-mode-hook #'ws-butler-mode)
(add-hook 'java-mode-hook #'yas-minor-mode)

;; Language server protocol support
(use-package lsp-java
  :when
  (and user-setting-java-path
       (not (seq-empty-p user-setting-java-runtimes)))
  :after lsp
  :custom
  (lsp-java-java-path user-setting-java-path)
  (lsp-java-import-gradle-java-home user-setting-java-path)
  (lsp-java-configuration-runtimes user-setting-java-runtimes)
  (lsp-java-code-generation-use-blocks t)
  (lsp-java-completion-enabled t))

;; Debug adapter protocol support
(with-eval-after-load 'dap-mode
  (require 'dap-java))

;; Gradle language and build tooling support
(use-package gradle-mode
  :mode "\\.gradle\\'")

(provide 'user-language-java)
;;; user-language-java.el ends here
