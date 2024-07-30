;;; aoc -- An interactive client for Advent of Code.

;;; Commentary:

;; This module provides an interactive client for Advent of Code.  It's hard to
;; argue that this is necessary, but it was fun to work on and it'll reduce my
;; context switching a little.

;;; Code:

(require 'calendar)
(require 'dom)
(require 'shr)

(require 'request)

;;; Customization:

(defgroup aoc nil
  "Advent of Code client."
  :version "28.0"
  :group 'local)

(defcustom aoc-fill-width 74
  "The fill-width for rendered server responses."
  :group 'aoc
  :type '(integer))

;;; State:

(defvar aoc--year nil
  "Which year to retrieve puzzles and submit results for.")

(defvar aoc--day nil
  "Which day to retrieve puzzles and submit results for.")

;;; Macros:

(defmacro aoc--show-article (buffer-name)
  "Return a function to show a puzzle in BUFFER-NAME."
  `(cl-function
    (lambda (&key symbol-status response &allow-other-keys)
      (if (not (eq symbol-status 'success))
          (error "Unexpected status: %s" symbol-status)
        (let ((dom (with-temp-buffer
                     (insert (request-response-data response))
                     (libxml-parse-html-region (point-min) (point-max)))))
          (switch-to-buffer (get-buffer-create ,buffer-name))
          (erase-buffer)
          (dom-print (aoc--get-article dom))
          (let ((shr-width aoc-fill-width)
                (shr-max-width aoc-fill-width)
                (shr-cookie-policy nil))
            (shr-render-region (point-min) (point-max)))
          (goto-char (point-min))
          (view-mode))))))

(defmacro aoc--show-input (buffer-name)
  "Return a function to show puzzle input in BUFFER-NAME."
  `(cl-function
    (lambda (&key symbol-status response &allow-other-keys)
      (if (not (eq symbol-status 'success))
          (error "Unexpected status: %s" symbol-status)
        (switch-to-buffer (get-buffer-create ,buffer-name))
        (erase-buffer)
        (insert (request-response-data response))))))

(defmacro aoc--show-failure (data-type)
  "Return a function to show response failure for DATA-TYPE."
  `(cl-function
    (lambda (&key symbol-status &allow-other-keys)
      (error "Failed to fetch %s from Advent of Code: %s"
             ,data-type
             symbol-status))))

;;; User interface:

(defun aoc-set-puzzle (year day)
  "Set the current YEAR and DAY for puzzles."
  (interactive (list (aoc--validate-year (read-number "Year: "))
                     (aoc--validate-day (read-number "Day: "))))
  (setq aoc--year year)
  (setq aoc--day day))

(defun aoc-puzzle (year day)
  "Display the puzzle for YEAR and DAY."
  (interactive
   (list
    (if (or (not aoc--year) current-prefix-arg)
        (aoc--validate-year (read-number "Year: "))
      aoc--year)
    (if (or (not aoc--day) current-prefix-arg)
        (aoc--validate-day (read-number "Day: "))
      aoc--day)))
  (aoc-set-puzzle year day)
  (let ((url (format "https://adventofcode.com/%d/day/%d" year day))
        (cookies (concat "session=" (aoc--cookie))))
    (request url
      :method "GET"
      :headers (list (cons "Cookie" cookies))
      :success (aoc--show-article "Advent of Code")
      :error (aoc--show-failure "puzzle"))))

(defun aoc-input (year day)
  "Display the input for YEAR and DAY."
  (interactive
   (list
    (if (or (not aoc--year) current-prefix-arg)
        (aoc--validate-year (read-number "Year: "))
      aoc--year)
    (if (or (not aoc--day) current-prefix-arg)
        (aoc--validate-day (read-number "Day: "))
      aoc--day)))
  (aoc-set-puzzle year day)
  (let ((url (format "https://adventofcode.com/%d/day/%d/input" year day))
        (cookies (concat "session=" (aoc--cookie))))
    (request url
      :method "GET"
      :headers (list (cons "Cookie" cookies))
      :success (aoc--show-input "Advent of Code Input")
      :error (aoc--show-failure "input"))))

(defun aoc-submit (year day part answer)
  "Submit ANSWER for PART of the puzzle on YEAR and DAY."
  (interactive
   (list
    (if (or (not aoc--year) current-prefix-arg)
        (aoc--validate-year (read-number "Year: "))
      aoc--year)
    (if (or (not aoc--day) current-prefix-arg)
        (aoc--validate-day (read-number "Day: "))
      aoc--day)
    (aoc--validate-part (read-number "Part: "))
    (read-string "Answer: ")))
  (aoc-set-puzzle year day)
  (let ((url (format "https://adventofcode.com/%d/day/%d/answer" year day))
        (cookies (concat "session=" (aoc--cookie))))
    (request url
      :method "POST"
      :headers (list (cons "Cookie" cookies))
      :data (list (cons "level" part)
                  (cons "answer" answer))
      :success (aoc--show-input "Advent of Code Submission")
      :error (aoc--show-failure "submission"))))

;;; Networking:

(defun aoc--cookie ()
  "Return the cookie for the current user.

This is the same location that my Racket client expects the session cookie
to be stored at."
  (with-temp-buffer
    (insert-file-contents "~/.advent-of-code")
    (string-trim (buffer-string))))

;;; Parsing:

(defun aoc--get-article (dom)
  "Return the article child of DOM."
  (dom-child-by-tag (dom-child-by-tag dom 'body) 'main))

;;; Input validation:

(defun aoc--validate-year (year)
  "Validate that YEAR is within range and return it."
  (let* ((current-date (calendar-current-date))
         (current-year (calendar-extract-year current-date))
         (maximum-year (if (>= (calendar-extract-month current-date) 12)
                           current-year (- current-year 1))))
    (if (or (> year maximum-year)
            (< year 2015))
        (progn
          (error "Year %d is not within range 2015-%d" year maximum-year)
          (sit-for 1)
          t)
      year)))

(defun aoc--validate-day (day)
  "Validate that DAY is within range and return it."
  (if (or (< day 1)
          (> day 25))
      (progn
        (error "Day %d is not within range 1-25" day)
        (sit-for 1)
        t)
    day))

(defun aoc--validate-part (part)
  "Validate that PART is within range and return it."
  (if (not (or (= part 1) (= part 2)))
      (progn
        (error "Part %d is not within range 1-2" part)
        (sit-for 1)
        t)
    part))

(provide 'aoc)
;;; aoc.el ends here
