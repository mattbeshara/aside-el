;;; aside.el --- Toggle-able side windows -*- lexical-binding: t -*-

;; Copyright 2021 Matt Beshara

;; Author: Matt Beshara <m@mfa.pw>
;; URL: https://github.com/mattbeshara/aside-el
;; Version: 1.0.0

;; This file is NOT part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by the Free
;; Software Foundation, either version 3 of the License, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
;; FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
;; more details.
;;
;; You should have received a copy of the GNU General Public License along
;; with this program.  If not, see <https://www.gnu.org/licenses/>.


;;; Commentary:
;;
;; The code in this package makes it straightforward to configure side windows
;; which display specific buffers, and commands which can toggle the
;; visibility of those windows.  The function of most interest here is
;; probably ASIDE-WINDOW-TOGGLE-DWIM.  This function returns a closure which
;; can be used to toggle the visibility of specific buffers in a specific
;; window, as well as to jump directly to that window if it is already
;; visible.  There are two uses for this code in particular which I find
;; indispensable, and they are included as the ASSISTANT and DIRED
;; configurations.  The DIRED configuration and its supporting code are
;; defined and documented in aside-dired.el.

;; The ASSISTANT configuration causes a side window on the right of the frame
;; to be used for buffers which provide contextual information that I only
;; want visible for a short amount of time, such as Help, Apropos, Occur, etc.
;; Having one of these buffers appear or disappear should not cause me to lose
;; my place in any other buffer, but the window they appear in should be big
;; enough to let me see the information I need with a minimum of scrolling.

;; To activate the ASSISTANT configuration, do something like the following:
;; (require 'aside)
;; (push (aside-configuration-display-buffer-alist 'assistant)
;;       display-buffer-alist)
;; (defalias 'my-aside-assistant-dwim
;;   (aside-window-toggle-dwim
;;    (aside-configuration-buffer-matcher 'assistant))
;;   "DWIM command for the assistant window.")
;; (define-key global-map (kbd "C-S-h") #'my-aside-assistant-dwim)


;;; Code:

(defvar aside-configuration-assistant
  `(assistant
    ,(concat "\\*Help\\*"           "\\|"
             "\\*Apropos\\*"        "\\|"
             "\\*rg\\*"             "\\|"
             "\\*grep\\*"           "\\|"
             "\\*Occur\\*"          "\\|"
             "\\*Embark Export"     "\\|"
             "\\*Embark Collect\\*" "\\|"
             "\\*xref\\*")
    ((side . right)
     (window-width . 60)))
  "Configuration for the ASSISTANT window, which displays Help, Occur, and
other similar buffers which do not display primary content.")

(defcustom aside-configurations (list aside-configuration-assistant)
  "A list of lists.  The CAR of each sublist should be a symbol identifying
the configuration, and the CDR should be of the form (CONDITION . ACTION), as
specified for DISPLAY-BUFFER-ALIST.")

(defun aside-buffer-name-matcher (regexp)
  "Returns a closure that accepts a buffer and returns a non-NIL value if the
name of that buffer matches REGEXP."
  (lambda (buffer)
    (string-match regexp (buffer-name buffer))))

(defun aside--find-buffer-matching (buffer-matcher)
  "Returns a buffer for which BUFFER-MATCHER returns non-NIL, if any match."
  (seq-some (lambda (buffer)
              (and (funcall buffer-matcher buffer)
                   buffer))
            (buffer-list)))

(defun aside--find-window-with-buffer-matching (buffer-matcher)
  "Returns a window displaying a buffer for which BUFFER-MATCHER returns a
non-NIL value, if any match."
  (seq-some (lambda (win)
              (and (funcall buffer-matcher (window-buffer win))
                   win))
            (window-list)))

(defun aside-configuration-display-buffer-alist (id)
  "Returns a list of display options suitable for adding to
DISPLAY-BUFFER-ALIST for a given configuration ID."
  (let ((info (cdr (assoc id aside-configurations))))
    (when info
      `(,(car info) (display-buffer-in-side-window) ,@(cadr info)))))

(defun aside-configuration-buffer-matcher (id)
  "Returns a matcher for a given configuration ID."
  (let ((info (assoc id aside-configurations)))
    (when info
      (cadr info))))

(defun aside-window-toggle-dwim (buffer-matcher &optional buffer-creator)
  "Returns a closure which can be used to create a window toggling command.

BUFFER-MATCHER may be either a function which accepts a buffer and returns
non-NIL if the buffer it is called with should be displayed in the side
window, or a regexp which will be matched against buffer names.

When the closure returned by this function is called, BUFFER-MATCHER will be
called on successive elements of (BUFFER-LIST) until one is found that
matches.  The closure will also try to find a window in (WINDOW-LIST) which is
displaying a buffer which BUFFER-MATCHER matches.  If a matching window is not
found (which means that there must also be no matching buffer) then
BUFFER-CREATOR is called, if a value for that argument was supplied.  If a
matching buffer was found, but window was found displaying it, then that
buffer will be displayed, and its window selected.  If the matching buffer is
the current buffer (i.e. the buffer visible in the selected window) then the
window is deleted.  Otherwise, the matching window (which is already
displaying the buffer) is selected.

Note that even if both a matching buffer and matching window are found, the
buffer displayed in the window might not be the same as the buffer found from
searching (BUFFER-LIST).  That these values are allowed to be different is a
good thing for consistent behaviour.  Specifically, this means that if the
function is called while the side window is selected, it will always cause the
side window to close, even if the side window could have any one of multiple
different buffers visible.  If the logic was modified such that the buffer
displayed in the side window had to be the same buffer as the buffer found
when searching (BUFFER-LIST), then whether the side window was hidden or not
when this function was called would depend not only on if the side window was
selected, but also which buffer the side window was displaying. In my
experience, this was even more confusing than this attempt at an explanation
makes it sound.

The overall effect of this function is to allow creating a single ‘do what I
mean’ key binding which can: create a new buffer and display it in a specific
window; switch directly to that window from any other window; hide the buffer
by deleting the window when it already has focus; and re-open the buffer if no
visible window is currently displaying it."
  (lambda ()
    (interactive)
    (let* ((b-m-lambda (if (functionp buffer-matcher)
                           buffer-matcher
                           (aside-buffer-name-matcher buffer-matcher)))
           (buffer (aside--find-buffer-matching b-m-lambda))
           (window (aside--find-window-with-buffer-matching b-m-lambda)))
      (cond ((and (null window) (null buffer))
             (when buffer-creator
               (funcall buffer-creator)))
            ((and (null window) buffer)
             (select-window (display-buffer buffer)))
            ((funcall b-m-lambda (current-buffer))
             (delete-window window))
            (t
             (select-window window))))))

(provide 'aside)
