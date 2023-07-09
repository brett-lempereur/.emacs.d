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

(defun user-notes-find-project-notebook (name)
  "Visit the notebook for project NAME."
  (interactive
   (list
    (completing-read
     "Project: "
     (mapcar #'file-name-base (user-glob
                               user-setting-project-notebook-path
                               "*.org")))))
  (let ((file (concat user-setting-project-notebook-path "/" name ".org")))
    (find-file file)))

(defun user-notes-capture-templates ()
  "Return capture templates for all active notebooks."
  (append
   (user-notes-project-capture-templates "" "")
   (list (list "p" "Projects"))
   (cl-mapcan
    (lambda (prefix name)
      (cons (list (concat "p" prefix) (file-name-base name))
            (user-notes-project-capture-templates (concat "p" prefix) name)))
    (user-lowercase-letters)
    (user-glob user-setting-project-notebook-path "*.org"))))

(defun user-notes-project-capture-templates (prefix name)
  "Return capture templates for project NAME with key PREFIX."
  (list
   (list
    (concat prefix "t") "Task" 'entry
    (list 'file+olp name "Tasks")
    (list 'file (locate-user-emacs-file "user-capture-templates/task.org"))
    :empty-lines 1)
   (list
    (concat prefix "r") "Recurring Task" 'entry
    (list 'file+olp name "Recurring Tasks")
    (list 'file (locate-user-emacs-file "user-capture-templates/habit.org"))
    :empty-lines 1)
   (list
    (concat prefix "n") "Note" 'entry
    (list 'file+olp name "Notes")
    (list 'file (locate-user-emacs-file "user-capture-templates/note.org"))
    :empty-lines 1)
   (list
    (concat prefix "j") "Journal" 'entry
    (list 'file+olp+datetree name "Journal")
    (list 'file (locate-user-emacs-file "user-capture-templates/journal.org"))
    :empty-lines 1)
   (list
    (concat prefix "m") "Meeting" 'entry
    (list 'file+olp+datetree name "Journal")
    (list 'file (locate-user-emacs-file "user-capture-templates/meeting.org"))
    :empty-lines 1)))

(defun user-notes-watch-projects (event)
  "Handle notification EVENT in the project notebook path."
  (when (member (cadr event) '(created deleted renamed))
    (setq org-capture-templates (user-notes-capture-templates))))

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
(global-set-key (kbd "C-c n b") #'org-switchb)
(global-set-key (kbd "C-c n g") #'consult-org-agenda)
(global-set-key (kbd "C-c n f") #'user-notes-find-notebook)
(global-set-key (kbd "C-c n p") #'user-notes-find-project-notebook)

;; Local keybindings
(define-key org-mode-map (kbd "M-g h") #'consult-org-heading)

;; File discovery customisations
(setq org-directory (file-name-as-directory user-setting-notebook-path))
(setq org-default-notes-file (concat org-directory "Notebook.org"))
(setq org-agenda-files
      (list org-directory
            (file-name-as-directory user-setting-project-notebook-path)))

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

;; Watch for changes in the project directory
(file-notify-add-watch user-setting-project-notebook-path
                       '(change)
                       #'user-notes-watch-projects)

;; Set the initial list of capture templates.
(setq org-capture-templates (user-notes-capture-templates))

;; Archiving
(setq
 org-archive-location
 (concat org-directory "Archive/" (format-time-string "%Y") ".org"))

(provide 'user-notes)
;;; user-notes.el ends here
