;;; user-settings -- Customisation options for the environment.

;;; Commentary:

;; Both the environment configuration modules and the per-machine
;; setting modules need these options to be defined.  So, they are
;; provided in a third module.

;;; Code:

(require 'custom)

;; Define a customisation group for machine-specific settings.
(defgroup user-settings nil
  "Machine specific customisations."
  :version "28.0"
  :group 'local)

;; Appearance customisations
(defcustom user-setting-font "Monospace-12"
  "The font face to use for graphical frames."
  :group 'user-settings
  :type '(string))
(defcustom user-setting-theme-package 'subatomic-theme
  "The package that contains the custom theme."
  :group 'user-settings
  :type '(choice (const :tag "None" nil) symbol))
(defcustom user-setting-theme 'subatomic
  "The name of the custom theme."
  :group 'user-settings
  :type '(symbol))
(defcustom user-setting-menu-bar-mode nil
  "Whether to display the menu bar."
  :group 'user-settings
  :type '(boolean))

;; Terminal customisations
(defcustom user-setting-shell "zsh"
  "The program to run as a terminal shell."
  :group 'user-settings
  :type '(string))
(defcustom user-setting-terminal-buffer-size 8192
  "The number of lines to retain in terminal history."
  :group 'user-settings
  :type '(integer))

;; Project management customisations
(defcustom user-setting-project-indexing-method 'hybrid
  "The method to use when indexing project files."
  :group 'user-settings
  :type '(symbol))
(defcustom user-setting-project-paths nil
  "The list of paths that contain projects."
  :group 'user-settings
  :type '(repeat string))

;; Organisation customisations
(defcustom user-setting-notebook-path "~/Notes/"
  "The path that contains notebooks."
  :group 'user-settings
  :type '(string))
(defcustom user-setting-project-notebook-path "~/Notes/Projects"
  "The path that contains project notebooks."
  :group 'user-settings
  :type '(string))

;; Common LISP programming language customisations
(defcustom user-setting-lisp-program nil
  "The path of the Common LISP executable on the machine."
  :group 'user-settings
  :type '(choice (const :tag "None" nil) string))

;; Racket programming language customisations
(defcustom user-setting-racket-program nil
  "The path of the Racket executable on the machine."
  :group 'user-settings
  :type '(choice (const :tag "None" nil) string))

;; Java programming language customisations
(defcustom user-setting-java-path nil
  "The path to the Java executable on the machine."
  :group 'user-settings
  :type '(choice (const :tag "None" nil) string))
(defcustom user-setting-java-runtimes []
  "The list of Java runtimes on the machine."
  :group 'user-settings
  :type '(vector (alist :key-type symbol :value-type (choice string boolean))))

(provide 'user-settings)
;;; user-settings.el ends here
