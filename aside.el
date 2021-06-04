;;; aside.el --- Toggle-able side windows -*- lexical-binding: t -*-

;; Copyright 2021 Matt Beshara

;; Author: Matt Beshara <m@mfa.pw>
;; URL: https://github.com/mattbeshara/aside-el
;; Version: 1.1.0

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
;; probably ‘aside-window-toggle-dwim’.  This function returns a closure which
;; can be used to toggle the visibility of specific buffers in a specific
;; window, as well as to jump directly to that window if it is already
;; visible.  There are two uses for this code in particular which I find
;; indispensable, and they are included as the Context and Dired
;; configurations.  These configurations and their supporting code are defined
;; and documented in ‘aside-context.el’ and ‘aside-dired.el’.

;; Here is some more information on how ‘aside-window-toggle-dwim’ works which
;; was too large to put into its docstring:
;;
;; After a matcher closure has been derived from BUFFER-MATCHER, the matcher
;; is used to try to find a matching buffer from ‘buffer-list’, and a window
;; from ‘window-list’ which is currently displaying a matching buffer.
;;
;; When neither a matching buffer or window are found, BUFFER-CREATOR is
;; called, if a value for that argument was supplied.  If a matching buffer
;; was found, but there is no window displaying it, then that buffer will be
;; displayed, and its window selected.  If the matching buffer is the current
;; buffer (i.e. the buffer visible in the selected window) then the window is
;; deleted.  Otherwise, the matching window (which must already be displaying
;; the buffer) is selected.
;;
;; Note that even if both a matching buffer and matching window are found, the
;; buffer displayed in the window might not be the same as the buffer found
;; from searching ‘buffer-list’.  That these values are allowed to be
;; different is a good thing for consistent behaviour.  Specifically, this
;; means that if the function is called while the side window is selected, it
;; will always cause the side window to close, even if the side window could
;; have any one of multiple different buffers visible.  If the logic was
;; modified such that the buffer displayed in the side window had to be the
;; same buffer as the buffer found when searching ‘buffer-list’, then whether
;; the side window was hidden or not when this function was called would
;; depend not only on if the side window was selected, but also which buffer
;; the side window was displaying.  In my experience, this was even more
;; confusing than this attempt at an explanation makes it sound.


;;; Code:

;; Symbol builders

(defun aside--condition-var-name (symbol)
  "Returns the name of the variable expected to hold a CONDITION as used in
‘display-buffer-alist’."
  (intern (format "aside-%s-condition" symbol)))

(defun aside--action-alist-var-name (symbol)
  "Returns the name of the variable expected to hold an ACTION-ALIST as used
in ‘display-buffer-alist’."
  (intern (format "aside-%s-action-alist" symbol)))

(defun aside--display-buffer-function-name (symbol)
  "Returns the name of a function expected to be usable in place of
‘display-buffer-in-side-window’ in ‘display-buffer-alist’."
  (intern (format "aside--%s-display-buffer" symbol)))

(defun aside--hook-var-name (symbol)
  "Returns the name of a variable used as a hook for a specific
configuration."
  (intern (format "aside-%s-hook" symbol)))

(defun aside--hook-run-var-name (symbol)
  "Returns the name of a variable used to keep track of whether of not a hook
has been run for a specific configuration."
  (intern (format "aside--%s-hook-run" symbol)))

(defun aside--configuration-id-from-var-name (symbol)
  "Returns the first word in SYMBOL appearing after a hyphen, ‘intern’ed as a
symbol."
  (intern (elt (split-string (format "%s" symbol) "-") 1)))


;; Buffer and window matching

(defun aside--buffer-name-matcher (regexp)
  "Returns a closure that accepts a buffer and returns a non-nil value if the
name of that buffer matches REGEXP."
  (lambda (buffer)
    (string-match regexp (buffer-name buffer))))

(defun aside--matcher (thing)
  "Returns a closure for matching buffers derived from THING.
If THING is a function, that function is returned.  If THING is a string, it
is assumed to be a regexp, and a closure which uses that regexp to match the
names of buffers is returned.  If THING is a symbol, an attempt is made to
generate a variable name from it and its value is used in a recursive call."
  (cond ((symbolp thing)
         ;; Check if it’s a symbol first, before checking it’s a function,
         ;; because ‘functionp’ will return t when passed a symbol a function
         ;; is bound to.
         (if-let ((var-name (aside--condition-var-name thing))
                  (value (ignore-errors (symbol-value var-name))))
             (aside--matcher value)
           (error "No value for symbol" var-name)))
        ((functionp thing)
         thing)
        ((stringp thing)
         (aside--buffer-name-matcher thing))
        (t
         (error "Unhandled THING type" thing))))

(defun aside--find-buffer-matching (buffer-matcher)
  "Returns a buffer for which BUFFER-MATCHER returns non-nil, if any match."
  (seq-some (lambda (buffer)
              (and (funcall buffer-matcher buffer)
                   buffer))
            (buffer-list)))

(defun aside--find-window-with-buffer-matching (buffer-matcher)
  "Returns a window displaying a buffer for which BUFFER-MATCHER returns a
non-nil value, if any match."
  (seq-some (lambda (win)
              (and (funcall buffer-matcher (window-buffer win))
                   win))
            (window-list)))


;;; Configuration definition

(defun aside--configuration-display-buffer-alist (id)
  "Returns a list of display options suitable for adding to
‘display-buffer-alist’."
  (when-let ((condition-var (aside--condition-var-name id))
             (condition (ignore-errors (symbol-value condition-var)))
             (action-alist-var (aside--action-alist-var-name id))
             (action-alist (ignore-errors (symbol-value action-alist-var)))
             (display-buffer (aside--display-buffer-function-name id)))
    `(,condition (,display-buffer) ,@action-alist)))

(defun aside--define-buffer-display-function (id)
  "Defines a function which wraps around ‘display-buffer-in-side-window’, but
which also runs a hook specific to each configuration."
  (let ((hook-var-name (aside--hook-var-name id))
        (hook-run-var-name (aside--hook-run-var-name id))
        (display-buffer-func-name (aside--display-buffer-function-name id)))
    (set hook-run-var-name nil)
    (make-local-variable hook-run-var-name)
    (fset display-buffer-func-name
          (lambda (buffer alist)
            (display-buffer-in-side-window buffer alist)
            (with-current-buffer buffer
              (unless (symbol-value hook-run-var-name)
                (run-hooks hook-var-name)))))))

(defun aside-enable-configuration (id)
  "If an ‘eql’ list is not already present in ‘display-buffer-alist’, pushes a
list composed of ‘aside-<id>-condition’ and ‘aside-<id>-action-alist’ to it.
Also calls ‘aside--define-buffer-display-function’ to define a buffer display
function which runs a hook specific to the configuration specified by ID."
  (aside--define-buffer-display-function id)
  (add-to-list 'display-buffer-alist
               (aside--configuration-display-buffer-alist id)))

(defun aside-disable-configuration (id)
  "Removes a list composed of ‘aside-<id>-condition’ and
‘aside-<id>-action-alist’ from ‘display-buffer-alist’, if one is present."
  (setq display-buffer-alist
        (delete (aside--configuration-display-buffer-alist id)
                display-buffer-alist)))

(defun aside-configuration-setter-function (name value)
  "A setter function for customizable options which disables the old
configuration before enabling a new one, using the newly set value."
  (let ((configuration-id (aside--configuration-id-from-var-name name)))
    (aside-disable-configuration configuration-id)
    (set name value)
    (aside-enable-configuration configuration-id)))


;; Public interface

(defun aside-window-toggle-dwim (buffer-matcher &optional buffer-creator)
  "Returns a closure which can be used to create a window toggling command.
BUFFER-MATCHER is passed to ‘aside--matcher’, and may be a symbol, string, or
function.  Refer to that function’s docstring for more information.

For more detailed information about this function, see the Commentary section
of ‘aside.el’.  The overall effect of this function is to allow creating a
single ‘do what I mean’ key binding which can: create a new buffer and display
it in a specific window; switch directly to that window from any other window;
hide the buffer by deleting the window when it already has focus; and re-open
the buffer if no visible window is currently displaying it."
  (lambda ()
    (interactive)
    (let* ((matcher (aside--matcher buffer-matcher))
           (buffer (aside--find-buffer-matching matcher))
           (window (aside--find-window-with-buffer-matching matcher)))
      (cond ((and (null window) (null buffer))
             (when buffer-creator
               (funcall buffer-creator)))
            ((and (null window) buffer)
             (select-window (display-buffer buffer)))
            ((funcall matcher (current-buffer))
             (delete-window window))
            (t
             (select-window window))))))

(provide 'aside)
