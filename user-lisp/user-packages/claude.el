;;; claude.el -- Large language model completion and generation.

;;; Commentary:

;; Visual studio code shouldn't have a monopoly on integration with large
;; language models.

;;; Code:

;; -*- lexical-binding: t -*-

(require 'plz)

(defgroup claude nil
  "Large language model text completion and generation."
  :version "28.0"
  :group 'local)

(defcustom claude-api-key nil
  "The API key for the service."
  :group 'claude
  :type '(string))

(defcustom claude-maximum-tokens 4096
  "The default maximum number of tokens in a response."
  :group 'claude
  :type '(integer))

(defcustom claude-model "claude-3-5-sonnet-20240620"
  "The default model to use."
  :group 'claude
  :type '(string))

(defvar claude-current-file nil
  "The file currently being queried.")

(defvar claude-current-question nil
  "The current question.")

(defun claude-explain-buffer (question)
  "Interactively queries QUESTION with the current buffer as context."
  (interactive "sQuestion: ")
  (claude-explain question (point-min) (point-max)))

(defun claude-explain-function (question)
  "Interactively queries QUESTION with the current function as context."
  (interactive "sQuestion: ")
  (save-excursion
    (mark-defun)
    (claude-explain question (mark) (point))))

(defun claude-explain (question start end)
  "Queries QUESTION with context between START and END."
  (setq claude-current-file (buffer-name))
  (setq claude-current-question question)
  (let ((query (format
                claude--explain-prompt
                question
                (buffer-substring-no-properties start end))))
    (plz 'post "https://api.anthropic.com/v1/messages"
      :headers (list (cons "x-api-key" claude-api-key)
                     (cons "anthropic-version" "2023-06-01"))
      :body (json-encode
             (list (cons "model" claude-model)
                   (cons "max_tokens" claude-maximum-tokens)
                   (cons "messages" (claude--build-content query))))
      :as #'json-read
      :then
      (lambda (response)
        (let* ((content (claude--get-text response))
               (title (format (if (> (length claude-current-question) 50)
                                  "%.50s..." "%s")
                              claude-current-question))
               (buffer (get-buffer-create "*Claude*")))
          (with-current-buffer buffer
            (gfm-mode)
            (goto-char (point-max))
            (let ((initial-point (point)))
              (when (not (equal initial-point (point-min)))
                (insert "\n\n"))
              (insert (format "# <%s> %s\n" claude-current-file title))
              (insert (format "_%s_\n\n"
                              (format-time-string "%Y-%m-%d %H:%M:%S" (current-time))))
              (insert content)
              (fill-region initial-point (point)))
            (gfm-view-mode))
          (switch-to-buffer-other-window buffer))))))

;;; Request and response:

(defun claude--build-content (message)
  "Build the content of a request from MESSAGE."
  (let ((message (list (list (cons "role" "user") (cons "content" message)))))
    message))

(defun claude--get-text (response)
  "Return the list of text elements from RESPONSE."
  (mapconcat
   (lambda (c) (alist-get 'text c))
   (seq-filter (lambda (c) (equal (alist-get 'type c) "text"))
               (alist-get 'content response))
   "\n\n"))

;;; Prompts:

(defconst claude--explain-prompt
  "You are a software engineer.  You will be presented with a source code
listing and asked to answer a question about it.  Please provide your response
in the Markdown format, wrapping lines at 74 characters.  The question is: %s

The source code listing follows:
%s"
  "The prompt to use for explaining source code.")

;;; Bindings:

;; Coding.
(global-set-key (kbd "C-c q b") #'claude-explain-buffer)
(global-set-key (kbd "C-c q f") #'claude-explain-function)

(provide 'claude)
;;; claude.el ends here
