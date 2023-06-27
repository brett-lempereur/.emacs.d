# .emacs.d

Portable editor configuration for development tasks.

## Installation

Ensure that you have a minimal compiler toolchain, clone the repository
into `~/.emacs.d`, and launch Emacs.

## Machine-specific configuration

Machine specific configuration is in the `machine-lisp` path.  These
scripts are required to set a minimal number of constants that often
differ between machine.

The names of these files are determined using the following function:

```emacs-lisp
(defun user-machine-init ()
  "Return an interned symbol of the machine-specific initialisation module."
  (intern (downcase (format "user-machine-%s"
                            (car (split-string (system-name) "\\."))))))
```

So, `user-machine-rocinante.el` is the configuration file for the
machine `Rocinante.local`.  The fallback `user-machine-unknown.el` is
used when no machine specific configuration exists.

