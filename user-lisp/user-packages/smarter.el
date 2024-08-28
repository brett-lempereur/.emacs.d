;;; smarter -- Large language model integrations. -*- lexical-binding: t -*-

;;; Commentary:

;; A large language model integration providing interactive explanations of
;; code and line and region completion.

;;; Code:

(require 'request)

(defgroup smarter nil
  "Interactive large language model explanations and completion."
  :version "28.0"
  :group 'local)

(defcustom smarter-api-key nil
  "The API key for the service."
  :group 'smarter
  :type '(string))

(defcustom smarter-maximum-tokens 2048
  "The maximum number of tokens in a response."
  :group 'smarter
  :type '(integer))

(defcustom smarter-terse-maximum-tokens 512
  "The maximum number of tokens in a terse response."
  :group 'smarter
  :type '(integer))

(defcustom smarter-model "claude-3-5-sonnet-20240620"
  "The large language model to use."
  :group 'smarter
  :type '(string))

(defcustom smarter-system-prompt
  "You will be presented with a source code listing and asked to answer a
question about it.  Assume that the person reading your explanation is a
competent software engineer that is familiar with Emacs.  Provide your
response in the Markdown format.  Your reponse must be factually correct,
and you must not hallucinate."
  "The system prompt for explanations."
  :group 'smarter
  :type '(string))

(defcustom smarter-complete-system-prompt
  "You will be presented with a source code listing.  Your task is to
complete the code by replacing the string -$COMPLETION$- with your own
suggestion.  Your suggestion should be:

1. In the same language corresponding to Emacs major mode: %s
2. Correct
3. Simple

Your response should consist only of the suggested code, with no additional
context or explanation."
  "The system prompt for completions."
  :group 'smarter
  :type '(string))

(defvar-local smarter--session 0
  "The current session identifier.")

(defvar-local smarter--explain-history nil
  "The current explanation history.")

(defconst smarter--explain-prompt
  "
The question is \"%s\".  The remainder of this text contains the source code
listing.

%s")

(define-minor-mode smarter-mode
  "Toggle Smarter mode.
Interactively with no argument, this toggles the mode.

A positive prefix argument enables the mode, any other prefix argument
disables it.  From Lisp, argument omitted or nil enables the mode, `toggle'
toggles the state."
  :init-value nil
  :lighter " Smarter"
  :keymap
  `((,(kbd "C-c C-s b") . smarter-explain-buffer)
    (,(kbd "C-c C-s f") . smarter-explain-function)
    (,(kbd "C-c C-s c") . smarter-complete)))

(defun smarter-explain-buffer (question)
  "Interactively queries QUESTION with the current buffer as context."
  (interactive "sExplain: ")
  (message "Sending explain request for buffer...")
  (smarter--explain question (point-min) (point-max)))

(defun smarter-explain-function (question)
  "Interactively queries QUESTION with the current function as context."
  (interactive "sExplain: ")
  (message "Sending explain request for function...")
  (save-excursion
    (mark-defun)
    (smarter--explain question (mark) (point))))

(defun smarter-complete ()
  "Complete the code at point with the current buffer as context."
  (interactive)
  (let* ((context-before (buffer-substring-no-properties (point-min) (point)))
         (context-after (buffer-substring-no-properties (point) (point-max)))
         (context (concat context-before "-$COMPLETION$-" context-after)))
    (request "https://api.anthropic.com/v1/messages"
      :type "POST"
      :data (json-encode
             (smarter--request-body
              (format smarter-complete-system-prompt major-mode)
              context))
      :headers (smarter--request-headers)
      :parser 'json-read
      :success (cl-function
                (lambda (&key data &allow-other-keys)
                  (smarter--on-complete-success
                   (current-buffer) (point) data)))
      :error (cl-function
              (lambda (&key symbol-status &allow-other-keys)
                (smarter--on-complete-error symbol-status))))))

(defun smarter--explain (question start end)
  "Answer QUESTION with the current buffer between START and END as context."
  (let* ((context (buffer-substring-no-properties start end))
         (prompt (format smarter--explain-prompt question context))
         (id (smarter--new-session (current-buffer)))
         (buffer (smarter--explain-buffer (current-buffer))))
    (request "https://api.anthropic.com/v1/messages"
      :type "POST"
      :data (json-encode
             (smarter--request-body smarter-system-prompt prompt nil))
      :headers (smarter--request-headers)
      :parser 'json-read
      :success (cl-function
                (lambda (&key data &allow-other-keys)
                  (smarter--on-explain-success buffer id question data)))
      :error (cl-function
              (lambda (&key data &allow-other-keys)
                (smarter--on-explain-error buffer id question data))))))

(defun smarter--on-explain-success (buffer id question data)
  "Handle success DATA from QUESTION of conversation ID in BUFFER."
  (let ((content (smarter--response-content data))
        (title (smarter--question-to-title id question)))
    (with-current-buffer buffer
      (gfm-mode)
      (goto-char (point-max))
      (let ((initial-point (point)))
        (when (not (equal initial-point (point-min)))
          (insert "\n\n"))
        (insert (format "# %s\n" title))
        (insert (format-time-string "_%Y-%m-%d %H:%M:%S_\n\n" (current-time)))
        (insert content)
        (fill-region initial-point (point)))
      (gfm-view-mode))
    (switch-to-buffer-other-window buffer)))

(defun smarter--on-explain-error (buffer id question data)
  "Handle error DATA from QUESTION of conversation ID in BUFFER."
  (let ((title (smarter--question-to-title id question)))
    (with-current-buffer buffer
      (gfm-mode)
      (goto-char (point-max))
      (when (not (equal (point) (point-min)))
        (insert "\n\n"))
      (insert (format "# %s\n" title))
      (insert (format-time-string "_%Y-%m-%d %H:%M:%S_\n\n" (current-time)))
      (insert "Smarter encountered an error:\n\n```")
      (let ((initial-point (point)))
        (insert (json-encode data))
        (json-pretty-print initial-point (point)))
      (insert "```"))))

(defun smarter--on-complete-success (buffer insertion-point data)
  "Handle success DATA from completion in BUFFER at INSERTION-POINT."
  (with-current-buffer buffer
    (save-excursion
      (goto-char insertion-point)
      (insert (smarter--response-content data))))
  (message "Inserted completion"))

(defun smarter--on-complete-error (status-code)
  "Handle error STATUS-CODE from completion."
  (warn "Failed to generate completion: %S" status-code))

(defun smarter--new-session (source-buffer)
  "Return the next session identifier for SOURCE-BUFFER."
  (let ((buffer (smarter--explain-buffer source-buffer)))
    (with-current-buffer buffer
      (let ((id smarter--session))
        (setq-local smarter--session (+ id 1))
        (setq-local smarter--explain-history nil)
        id))))

(defun smarter--explain-buffer (source-buffer)
  "Get the explain buffer for SOURCE-BUFFER."
  (let* ((source-name (buffer-name source-buffer))
         (buffer-name (format "*Smarter %s*" source-name)))
    (get-buffer-create buffer-name)))

(defun smarter--request-headers ()
  "Return the headers of a request."
  (list (cons "Content-Type" "application/json")
        (cons "Accept" "application/json")
        (cons "x-api-key" smarter-api-key)
        (cons "anthropic-version" "2023-06-01")))

(defun smarter--request-body (system-prompt prompt &optional terse)
  "Return the body of a request for SYSTEM-PROMPT and PROMPT.
Optionally return TERSE output."
  (list (cons "model" smarter-model)
        (cons "max_tokens"
              (if terse smarter-terse-maximum-tokens smarter-maximum-tokens))
        (cons "system" system-prompt)
        (cons "messages"
              (list (list (cons "role" "user") (cons "content" prompt))))))

(defun smarter--question-to-title (id question)
  "Return the title for QUESTION with ID."
  (if (> (length question) 50)
      (format "<%d> %.50s..." id question)
    (format "<%d> %s" id question)))

(defun smarter--response-content (data)
  "Return the list of text elements from DATA."
  (mapconcat
   (lambda (c) (alist-get 'text c))
   (seq-filter (lambda (c) (equal (alist-get 'type c) "text"))
               (alist-get 'content data))
   "\n\n"))

(provide 'smarter)
;;; smarter.el ends here
