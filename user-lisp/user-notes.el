;;; user-notes -- Notetaking and task management environment.

;;; Commentary:

;; This module provides a relatively opinionated setup that defines a
;; general-purpose notebook file, with a set of capture templates.

;;; Code:

(require 'org)
(require 'org-capture)
(require 'org-indent)

(require 'user-development)
(require 'user-editing)
(require 'user-settings)
(require 'user-utilities)

(defun user-notes-find-notebook ()
  "Visit the general-purpose notebook file in a new or existing buffer."
  (interactive)
  (find-file org-default-notes-file))

(defun user-notes-capture-template (name)
  "Return the path of capture of template NAME."
  (locate-user-emacs-file (format "user-capture-templates/%s.org" name)))

(defun user-notes-current-archive-file ()
  "Return the path of the current archive file."
  (concat org-directory "Archive/" (format-time-string "%Y") ".org"))

;; Hooks for notebook buffers
(add-hook 'org-mode-hook #'auto-fill-mode)
(add-hook 'org-mode-hook #'flyspell-mode)
(add-hook 'org-mode-hook #'org-indent-mode)
(add-hook 'org-mode-hook #'rainbow-delimiters-mode)
(add-hook 'org-mode-hook #'selected-minor-mode)
(add-hook 'org-mode-hook #'smartparens-mode)
(add-hook 'org-mode-hook #'ws-butler-mode)
(add-hook 'org-mode-hook #'yas-minor-mode)

;; Additional modules
(add-to-list 'org-modules 'org-habit)
(add-to-list 'org-modules 'org-tempo)

;; Global keybindings
(global-set-key (kbd "C-c n c") #'org-capture)
(global-set-key (kbd "C-c n a") #'org-agenda)
(global-set-key (kbd "C-c n t") #'org-todo-list)
(global-set-key (kbd "C-c n l") #'org-store-link)
(global-set-key (kbd "C-c n g") #'consult-org-agenda)
(global-set-key (kbd "C-c n f") #'user-notes-find-notebook)

;; Local keybindings
(define-key org-mode-map (kbd "M-g h") #'consult-org-heading)

;; File discovery customisations
(setq org-directory (file-name-as-directory user-setting-notebook-path))
(setq org-default-notes-file (concat org-directory "Notebook.org"))
(setq org-agenda-files (list org-directory))

;; Property customisations
(setq org-columns-default-format "%20ITEM %TODO %TYPE %3PRIORITY %SIZE %TAGS")
(setq org-property-format "%-12s %s")

;; Editing customisations
(setq org-blank-before-new-entry '((heading . t) (plain-list-item . auto)))
(setq org-completion-use-ido t)
(setq org-hide-leading-stars t)
(setq org-return-follows-link t)
(setq org-special-ctrl-a/e t)
(setq org-special-ctrl-k t)
(setq org-time-stamp-rounding-minutes '(5 5))

;; Task management customisations
(setq org-log-done t)

;; Performance customisations
(setq org-fold-core-style 'text-properties)

;; Agenda customisations
(setq org-agenda-menu-two-columns t)
(setq org-agenda-ndays 5)
(setq org-agenda-restore-windows-after-quit t)
(setq org-agenda-show-outline-path 'title)
(setq org-agenda-skip-scheduled-if-done t)
(setq org-agenda-skip-deadline-if-done t)
(setq org-agenda-start-on-weekday nil)
(setq org-agenda-todo-list-sublevels nil)

;; Capture customisations
(setq
 org-capture-templates
 `(("t" "Task" entry (file+olp "" "Tasks")
    (file ,(user-notes-capture-template "task.org")))
   ("r" "Recurring Task" entry (file+olp "" "Recurring Tasks")
    (file ,(user-notes-capture-template "habit.org")))
   ("n" "Note" entry (file+olp "" "Notes")
    (file ,(user-notes-capture-template "note.org")))
   ("j" "Journal" entry (file+olp "" "Journal")
    (file ,(user-notes-capture-template "journal.org")))
   ("m" "Meeting" entry (file+olp "" "Journal")
    (file ,(user-notes-capture-template "meeting.org")))))

;; Archive customisations
(setq org-archive-location (user-notes-current-archive-file))

;; Tag customisations
(setq org-tag-persistent-alist (mapcar #'list user-setting-notebook-projects))

(provide 'user-notes)
;;; user-notes.el ends here
