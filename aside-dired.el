;;; aside.el --- Toggle-able side windows -*- lexical-binding: t -*-

;; Copyright 2021 Matt Beshara

;; Author: Matt Beshara <m@mfa.pw>
;; URL: https://github.com/mattbeshara/aside-el
;; Version: 1.0.1

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
;; The DIRED configuration for Aside creates a side window displaying a Dired
;; buffer at the root of the current project when first used, but then toggles
;; the visibility of that side window, and allows you to jump directly to that
;; side window from any other window if the window is already visible.  This,
;; coupled with a small number of helper functions which ensure that
;; subdirectories are also opened in the side window and regular files are
;; not, makes for a very powerful but lightweight and convenient means of
;; navigating a project’s directory structure.

;; To activate the DIRED configuration, do something like this:
;; (require 'aside-dired)
;; (push aside-configuration-dired aside-configurations)
;; (push (aside-configuration-display-buffer-alist 'dired)
;;       display-buffer-alist)
;; (defalias 'my-aside-dired-dwim
;;   (aside-window-toggle-dwim (aside-dired-subdir-matcher) #'aside-dired)
;;   "DWIM command for the Dired side window.")
;; (define-key global-map (kbd "C-S-s") #'my-aside-dired-dwim)
;; (aside-dired-define-key-overrides)

;; You may also want to hide everything but the file names by default with:
;; (add-hook 'dired-mode-hook #'dired-hide-details-mode)


;;; Code:

(require 'dired)

(defcustom aside-dired-root-dir #'aside--dired-root-dir
  "A function which should return the root directory of the project the
current buffer belongs to.")

(defvar aside-dired-buffer-suffix "aside-dired"
  "Suffix added to Dired buffer names to differentiate buffers shown in the
Aside-Dired window.")

(defvar aside-configuration-dired
  `(dired
    ,(concat "\\*.+ " aside-dired-buffer-suffix "\\*")
    ((side . left)
     (window-width . 24)
     (window-parameters . ((mode-line-format . (" " "%b"))))))
  "Configuration for the DIRED window, which displays directory listings for
the project root directory and its subdirectories.")

(defun aside--dired-root-dir-magit ()
  "Tries to use Magit’s MAGIT-GIT-DIR function to determine the project root
dir, if it happens to be available."
  (when (fboundp 'magit-git-dir)
    (let ((git-dir (magit-git-dir)))
      (and git-dir
           (file-name-directory (directory-file-name git-dir))))))

(defun aside--dired-root-dir-project ()
  "Tries to use project.el’s PROJECT-CURRENT function to determine the project
root dir,if it happens to be available."
  (when (fboundp 'project-current)
    (let ((project-info (project-current)))
      (when project-info
        (cdr project-info)))))

(defun aside--dired-root-dir-vc ()
  "Tries to use VC’s VC-ROOT-DIR function to determine the project root dir,
if it happens to be available."
  (when (fboundp 'vc-root-dir)
    (vc-root-dir)))

(defun aside--dired-root-dir ()
  "Tries to find the root directory of the project the file of the current
buffer belongs to."
  (or (aside--dired-root-dir-magit)
      (aside--dired-root-dir-project)
      (aside--dired-root-dir-vc)
      default-directory))

(defun aside-dired-subdir-matcher ()
  "Matches Dired buffers the names of which follow the format for buffer names
Aside-Dired generates, for the project root dir and any of its subdirs."
  (lambda (buffer)
    (let ((root (funcall aside-dired-root-dir)))
      (when root
        (string-match
         (concat "\\\*"
                 (regexp-quote (expand-file-name root))
                 "?.* "
                 aside-dired-buffer-suffix
                 "\\\*")
         (buffer-name buffer))))))

(defun aside-dired (&optional dir)
  "Open a Dired buffer at DIR in the Aside-Dired side window.  If DIR is not
supplied, call ASIDE-DIRED-ROOT-DIR and use its return value instead."
  (let* ((dir (or dir (funcall aside-dired-root-dir)))
         (buffer (dired-noselect (expand-file-name dir))))
    (with-current-buffer buffer
      (rename-buffer
       (concat "*"
               (expand-file-name dired-directory)
               " "
               aside-dired-buffer-suffix
               "*")
       t))
    (select-window (display-buffer buffer))))

(defun aside-dired-find-file ()
  "A wrapper around the standard DIRED-FIND-FILE functionality which uses
ASIDE-DIRED to open directories and ensure files are not opened in the side
window, if the current buffer is an Aside-Dired buffer."
  (interactive)
  (if (string-match (aside-configuration-buffer-matcher 'dired) (buffer-name))
      (let ((file (dired-get-file-for-visit)))
        (if (file-directory-p file)
            (aside-dired file)
            (dired--find-file #'find-file-other-window file))
        (dired--find-file #'find-file file))
      (dired-find-file)))

(defun aside-dired-up-directory ()
  "A wrapper around the standard DIRED-UP-DIRECTORY functionality which uses
ASIDE-DIRED to open parent directories in the side window, if the current
buffer is an Aside-Dired buffer."
  (interactive)
  (if (string-match (aside-configuration-buffer-matcher 'dired) (buffer-name))
      (let* ((dir (dired-current-directory))
             (up (file-name-directory (directory-file-name dir))))
        (aside-dired up))
      (dired-up-directory)))

(defun aside-dired-define-key-overrides ()
  "Rebinds C-m and ^ in dired buffers to call the Aside-Dired versions of the
DIRED-FIND-FILE and DIRED-UP-DIRECTORY functions. Unless you rebind these keys
to something else in the first place, using these functions in place of the
original Dired functions should not cause problems, as the Aside-Dired
versions of these functions only cause a change in behaviour from Aside-Dired
buffers."
  (define-key dired-mode-map (kbd "C-m") #'aside-dired-find-file)
  (define-key dired-mode-map (kbd "^") #'aside-dired-up-directory))

(provide 'aside-dired)
