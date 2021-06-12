;;; aside-hook-functions.el --- Hook functions -*- lexical-binding: t -*-

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
;; This file contains some functions which I use to improve the presentation
;; of buffers displayed in Aside windows.


;;; Code:

(defun aside-hook-enable-truncate-lines ()
  "Enables line truncation, without displaying a message about it."
  (let ((inhibit-message t))
    (toggle-truncate-lines 1)))

(defun aside-hook-disable-display-line-numbers-mode ()
  "Call command ‘display-line-numbers-mode’ to disable line numbers."
  (when (fboundp 'display-line-numbers-mode)
    (display-line-numbers-mode -1)))

(defun aside-hook-reduce-font-size ()
  "Make the font size of the default face a bit smaller."
  (face-remap-add-relative 'default
                           :height (- (face-attribute 'default :height)
                                      10)))

(provide 'aside-hook-functions)

;;; aside-hook-functions.el ends here
