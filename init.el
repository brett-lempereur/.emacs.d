;;; init -- Initialisation script.

;;; Commentary:

;; All initialisation is contained within the `user-lisp` path in a set
;; of modules prefixed with `user`.  This script ensures that they are
;; imported in something resembling a correct order.

;;; Code:

;; Backwards compatibility for systems without `early-init.el` support.
(require 'early-init (concat user-emacs-directory "early-init.el"))

;; Machine-specific customisation.
(require 'user-machine)

;; General customisation.
(require 'user-appearance)
(require 'user-completion)
(require 'user-development)
(require 'user-editing)
(require 'user-navigation)
(require 'user-networking)
(require 'user-operating-system)
(require 'user-project-management)
(require 'user-version-control)

;; Programming languages.
(require 'user-language-c-cpp)
(require 'user-language-clojure)
(require 'user-language-common-lisp)
(require 'user-language-emacs-lisp)
(require 'user-language-go)
(require 'user-language-haskell)
(require 'user-language-markup)
(require 'user-language-ocaml)
(require 'user-language-racket)
(require 'user-language-ruby)
(require 'user-language-rust)
(require 'user-language-web)

;; Local and non-version-controlled customisation
(setq custom-file (no-littering-expand-etc-file-name "custom.el"))
(when (file-exists-p custom-file) (load custom-file))

(provide 'init)
;;; init.el ends here
