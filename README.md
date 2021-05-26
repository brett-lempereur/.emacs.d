# .emacs.d

Portable editor configuration for development tasks.

## Installation

Ensure that you have a minimal compiler toolchain, clone the repository
into `~/.emacs.d`, and launch Emacs.

## Usage

In addition to mode-specific bindings, the following useful operations
are bound to the function keys:

* `f1` - switch projects.
* `f2` - open a file in the current project.
* `f3` - search the current project.
* `f4` - search for the symbol under point in the current project.
* `f5` - browse files in the current project.
* `f6` - show revision control status.
* `f7` - show the difference between unstaged commits and head.
* `f8` - annotate the current buffer with revision control history.

For languages that support interactive development:

* `f9` - start a new interactive session.
* `f10` - switch to the interactive session.
* `f11` - evaluate the last expression.
* `f12` - run the project test suite.

For compiled languages:

* `f9` - build the project.
* `f10` - format and lint the project.
* `f11` - clean the project.
* `f12` - run the project test suite.

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

### Appearance

To account for contrast ratio, brightness, and placement differences the
following constants can be defined to control appearance on a
per-machine basis:

``` emacs-lisp
(defconst user-setting-theme-package 'package-name-sym)
(defconst user-setting-theme 'theme-name-sym)
(defconst user-setting-font "Font Name-Size")
```

### Projectile

The locations of projects, and the method used to index them, can also
be defined on a per-machine basis with the following constants:

``` emacs-lisp
(defconst user-setting-project-indexing-method 'method)
(defconst user-setting-project-search-path '(list-of-path-depth-pairs))
```

### Revision Control

We use [Magit](https://github.com/magit/magit) as a revision control interface,

``` emacs-lisp
(defconst user-setting-repository-path '(list-of-repositories...))
```

To interact with Mercurial repositories, install the official remote
helper [git-remote-hg](https://github.com/felipec/git-remote-hg) and use the `hg::` protocol prefix when
cloning.  Remember to run `git gc --aggressive` after cloning too.
