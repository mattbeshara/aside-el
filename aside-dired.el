;;; aside-dired.el --- Light, powerful file browser -*- lexical-binding: t -*-

;; Copyright 2021 Matt Beshara

;; Author: Matt Beshara <m@mfa.pw>
;; URL: https://github.com/mattbeshara/aside-el
;; Version: 1.2.0

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
;; The Dired configuration for Aside creates a side window displaying a Dired
;; buffer at the root of the current project when first used, but then toggles
;; the visibility of that side window, and allows you to jump directly to that
;; side window from any other window if the window is already visible.  This,
;; coupled with a small number of helper functions which ensure that
;; subdirectories are also opened in the side window and regular files are
;; not, makes for a very powerful but lightweight and convenient means of
;; navigating a project’s directory structure.
;;
;; Many of the variables in this file defined with ‘defcustom’ use a custom
;; setter.  If you change the value of those variable outside of Customize and
;; do not use ‘customize-set-variable’ to do so, you may want to call
;; ‘aside-disable-configuration’ before changing the value, and
;; ‘aside-enable-configuration’ after the new value has been set.
;;
;; Loading this file will modify ‘display-buffer-alist’.
;; To activate the Dired configuration, do something like this:
;; (require 'aside-dired)
;; (define-key global-map (kbd "C-S-d") #'aside-dired-dwim)


;;; Code:

(require 'aside)
(require 'aside-hook-functions)
(require 'dired)
(require 'rx)

(defgroup aside-dired ()
  "Options for the Aside-Dired window.")

(defcustom aside-dired-buffer-suffix "aside-dired"
  "Suffix added to names of Aside-Dired buffers.
The default value of ‘aside-dired-condition’ uses this value
to prevent matching regular Dired buffers."
  :group 'aside-dired
  :type 'string
  :set #'aside-configuration-setter-function)

(defcustom aside-dired-condition
  (rx (seq "*" (one-or-more any) (literal aside-dired-buffer-suffix) "*"))
  "Used as a CONDITION in ‘display-buffer-alist’.
Matches the names of buffers that should be displayed in the
Aside-Dired window."
  :group 'aside-dired
  :type '(choice regexp function)
  :set #'aside-configuration-setter-function)

(defcustom aside-dired-action-alist
  '((side . left)
    (window-width . 24))
  "Alist used as the ACTION argument to ‘display-buffer’.
Applies to windows containing buffers matched by
‘aside-dired-condition’."
  :group 'aside-dired
  :type 'sexp
  :set #'aside-configuration-setter-function)

(defun aside--dired-root-dir-project ()
  "Get project root dir from ‘project-current’ if available."
  (when-let ((project-info (project-current))
             (dir (cdr project-info)))
    (expand-file-name dir)))

(defun aside--dired-root-dir ()
  "Get project root dir of file current buffer is visiting."
  (or (aside--dired-root-dir-project)
      default-directory))

(defcustom aside-dired-root-dir #'aside--dired-root-dir
  "A function returning root dir of current buffer’s project."
  :group 'aside-dired
  :type 'function)

(defun aside-dired-find-file ()
  "Use instead of ‘dired-find-file’ in Dired-Aside windows.
Ensures that subdirs of the project root dir are opened in
the side window, but that files are not."
  (interactive)
  (let ((file (dired-get-file-for-visit)))
    (if (file-directory-p file)
        (aside--dired file)
        (dired--find-file #'find-file-other-window file))
    (dired--find-file #'find-file file)))

(defun aside-dired-up-directory ()
  "Use instead of ‘dired-up-directory’ in Dired-Aside windows.
Calls ‘aside--dired’ to open parent dirs in the side window."
  (interactive)
  (let* ((dir (dired-current-directory))
         (up (file-name-directory (directory-file-name dir))))
    (aside--dired up)))

(defcustom aside-dired-mode-hook
  ;; A hook named ‘aside-dired-hook’ would also be run by the function defined
  ;; by ‘aside--define-buffer-display-function’ for Aside-Dired, and hooks
  ;; named in that way are used by the other configurations.  However,
  ;; because a minor mode is defined to add key bindings, and defining a minor
  ;; mode also implicitly defines a hook, to reduce confusion I have named
  ;; this hook ‘aside-dired-mode-hook’ so only one hook variable is in use.
  '(dired-hide-details-mode
    aside-hook-change-default-face-height
    aside-hook-enable-truncate-lines
    aside-hook-disable-display-line-numbers-mode)
  "Normal hook run for buffers in the Aside-Dired window."
  :group 'aside-dired
  :type 'hook
  :options '(dired-hide-details-mode
             aside-hook-change-default-face-height
             aside-hook-enable-truncate-lines
             aside-hook-disable-display-line-numbers-mode))

(defun aside--dired-subdir-matcher ()
  "Match Aside-Dired buffers for dirs inside project root."
  (lambda (buffer)
    (when-let ((root (funcall aside-dired-root-dir)))
      (string-match
       (format "\\\*%s.* %s\\\*"
               (regexp-quote (expand-file-name root))
               aside-dired-buffer-suffix)
       (buffer-name buffer)))))

(defvar aside-dired-mode-map
  (let ((map (make-composed-keymap nil dired-mode-map)))
    (define-key map [remap dired-find-file] #'aside-dired-find-file)
    (define-key map [remap dired-up-directory] #'aside-dired-up-directory)
    map)
  "Keymap with Aside-Dired variants of dired functions.")

(define-minor-mode aside-dired-mode
  "Enables Aside-specific behavior in Dired buffers."
  :lighter " D-A"
  :keymap aside-dired-mode-map)

(defun aside--dired (&optional dir)
  "Open Dired buffer at DIR in the Aside-Dired side window.
If DIR is nil, use value of ‘aside-dired-root-dir’ instead."
  (let* ((dir (or dir (funcall aside-dired-root-dir)))
         (buffer (dired-noselect (expand-file-name dir))))
    (with-current-buffer buffer
      (rename-buffer
       (format "*%s %s*"
               (expand-file-name dired-directory)
               aside-dired-buffer-suffix)
       t)
      (aside-dired-mode))
    (select-window (display-buffer buffer))))

(defalias 'aside-dired-dwim
  (aside-window-toggle-dwim (aside--dired-subdir-matcher) #'aside--dired)
  "DWIM command for the Aside-Dired window.")

(provide 'aside-dired)

;;; aside-dired.el ends here
