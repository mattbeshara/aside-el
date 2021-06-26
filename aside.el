;;; aside.el --- Toggle-able side windows -*- lexical-binding: t -*-

;; Copyright 2021 Matt Beshara

;; Author: Matt Beshara <m@mfa.pw>
;; URL: https://github.com/mattbeshara/aside-el
;; Version: 1.4.1

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

(defun aside--condition-var-name (sym)
  "Use SYM to generate a var name ending with CONDITION."
  (intern (format "aside-%s-condition" sym)))

(defun aside--action-alist-var-name (sym)
  "Use SYM to generate a var name ending with ACTION-ALIST."
  (intern (format "aside-%s-action-alist" sym)))

(defun aside--display-buffer-function-name (sym)
  "Use SYM to generate a var name ending with DISPLAY-BUFFER."
  (intern (format "aside--%s-display-buffer" sym)))

(defun aside--hook-var-name (sym)
  "Use SYM to generate a var name ending with HOOK."
  (intern (format "aside-%s-hook" sym)))

(defun aside--hook-run-var-name (sym)
  "Use SYM to generate a var name ending with HOOK-RUN."
  (intern (format "aside--%s-hook-run" sym)))

(defun aside--configuration-id-from-var-name (sym)
  "Use SYM to generate a symbol from first word after hyphen."
  (intern (elt (split-string (format "%s" sym) "-") 1)))


;; Buffer and window matching

(defun aside--buffer-name-matcher (regexp)
  "Return closure that matches REGEXP against BUFFER name."
  (lambda (buffer)
    (string-match regexp (buffer-name buffer))))

(defun aside--matcher (thing)
  "Return closure for matching buffers, derived from THING.
If THING is a function, that function is returned.  If THING
is a string, it is assumed to be a regexp, and a closure
which uses that regexp to match the names of buffers is
returned.  If THING is a symbol, an attempt is made to
generate a variable name from it and its value is used in a
recursive call."
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

(defun aside--find-buffer-matching (matcher)
  "Return some buffer MATCHER returns non-nil for."
  (seq-some (lambda (buffer)
              (and (funcall matcher buffer)
                   buffer))
            (buffer-list)))

(defun aside--find-window-with-buffer-matching (matcher)
  "Return window displaying buffer MATCHER returns non-nil for."
  (seq-some (lambda (win)
              (and (funcall matcher (window-buffer win))
                   win))
            (window-list)))


;;; Configuration definition

(defun aside--configuration-display-buffer-alist (id)
  "Return ‘display-buffer-alist’ display options for ID."
  (when-let ((condition-var (aside--condition-var-name id))
             (condition (ignore-errors (symbol-value condition-var)))
             (action-alist-var (aside--action-alist-var-name id))
             (action-alist (ignore-errors (symbol-value action-alist-var)))
             (display-buffer (aside--display-buffer-function-name id)))
    `(,condition (,display-buffer) ,@action-alist)))

(defun aside--define-buffer-display-function (id)
  "Defines a buffer display function for ID.

The function first defines a couple of variables, one to act
as a hook for a specific configuration, the other to track
which buffers the hook has been run in.  Then, it defines a
function which calls ‘display-buffer-in-side-window’ and
then runs the appropriate configuration’s hook if necessary."
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
  "Enable a configuration specified by ID.
If an ‘equal’ list is not already present in
‘display-buffer-alist’, this function pushes a list composed
of ‘aside-<id>-condition’ and ‘aside-<id>-action-alist’ to
it.  It also calls ‘aside--define-buffer-display-function’
to define a buffer display function which runs a hook
specific to the configuration specified by ID when the
buffer is displayed."
  (aside--define-buffer-display-function id)
  (add-to-list 'display-buffer-alist
               (aside--configuration-display-buffer-alist id)))

(defun aside-disable-configuration (id)
  "Disable a configuration specified by ID if one is enabled.
Removes a list composed of ‘aside-<id>-condition’ and
‘aside-<id>-action-alist’ from ‘display-buffer-alist’, if
one is present.  Otherwise, returns nil."
  (when (member (aside--configuration-display-buffer-alist id)
                display-buffer-alist)
    (setq display-buffer-alist
          (delete (aside--configuration-display-buffer-alist id)
                  display-buffer-alist))))

(defun aside-configuration-setter-function (name value)
  "Set customizable option NAME to VALUE.
This function first checks if there is already an enabled
configuration corresponding to NAME, and if there is, it
will disable the old configuration before setting the new
value, and then enabling a new configuration which uses the
new value.  If a matching configuration is not already
enabled, all that happens is that the variable NAME is ‘set’
to VALUE."
  (let ((configuration-id (aside--configuration-id-from-var-name name)))
    (if (aside-disable-configuration configuration-id)
        (progn
          (set name value)
          (aside-enable-configuration configuration-id))
        (set name value))))


;; Public interface

(defun aside-window-toggle-dwim (buffer-matcher &optional buffer-creator)
  "Return a window toggling closure.
BUFFER-MATCHER is passed to ‘aside--matcher’, and may be a
symbol, string, or function.  Refer to that function’s
docstring for more information.  BUFFER-CREATOR is called if
no matching buffer exists.

For more detailed information about this function, see the
Commentary section of ‘aside.el’.  The overall effect of
this function is to allow creating a single ‘do what I mean’
key binding which can: create a new buffer and display it in
a specific window; switch directly to that window from any
other window; hide the buffer by deleting the window when it
already has focus; and re-open the buffer if no visible
window is currently displaying it."
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

;;; aside.el ends here
