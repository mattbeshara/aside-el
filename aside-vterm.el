;;; aside-vterm.el --- A shared global VTerm buffer -*- lexical-binding: t -*-

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
;; It’s not a new idea – a terminal you can toggle the visibility of by
;; pressing a simple key binding.  Only a single, shared, global VTerm
;; instance is used.
;;
;; To activate the VTerm configuration, do something like the following:
;; (require 'aside-vterm)
;; (define-key global-map (kbd "C-`") #'aside-vterm-dwim)


;;; Code:

(require 'aside)
(require 'aside-hook-functions)
(require 'rx)
(require 'vterm)

(defgroup aside-vterm ()
  "Options for the Aside-VTerm window.")

(defcustom aside-vterm-buffer-name
  "*Aside-VTerm*"
  "The name for the single, shared, global Aside-Vterm buffer.

This option uses a custom setter.  If you change this option outside of
Customize, you will probably want to call ‘aside-disable-configuration’ before
changing this value, and ‘aside-enable-configuration’ after the new value has
been set."
  :group 'aside-vterm
  :type 'string
  :set #'aside-configuration-setter-function)

(defcustom aside-vterm-condition
  (rx (literal aside-vterm-buffer-name))
  "A regexp or function which will be used in ‘display-buffer-alist’ to match
the names of buffers that should be displayed in the Aside-VTerm window.

This option uses a custom setter.  If you change this option outside of
Customize, you will probably want to call ‘aside-disable-configuration’ before
changing this value, and ‘aside-enable-configuration’ after the new value has
been set."
  :group 'aside-vterm
  :type '(choice regexp function)
  :set #'aside-configuration-setter-function)

(defcustom aside-vterm-action-alist
  '((side . bottom)
    (window-height . 10))
  "An alist suitable for passing as the ACTION argument to ‘display-buffer’
when displaying buffers in the Aside-VTerm window.

This option uses a custom setter.  If you change this option outside of
Customize, you will probably want to call ‘aside-disable-configuration’ before
changing this value, and ‘aside-enable-configuration’ after the new value has
been set."
  :group 'aside-vterm
  :type 'sexp
  :set #'aside-configuration-setter-function)

(defcustom aside-vterm-hook
  '(aside-hook-reduce-font-size
    aside-hook-enable-truncate-lines
    aside-hook-disable-display-line-numbers-mode)
  "Normal hook run when opening a buffer in the Aside-VTerm window."
  :group 'aside-vterm
  :type 'hook
  :options '(aside-hook-reduce-font-size
             aside-hook-enable-truncate-lines
             aside-hook-disable-display-line-numbers-mode))

(defun aside--vterm ()
  "Creates a VTerm buffer which has the correct name and respects the options
specified in ‘display-buffer-alist’."
  (let ((buffer (get-buffer-create aside-vterm-buffer-name)))
    (with-current-buffer buffer
      (unless (derived-mode-p 'vterm-mode)
        (vterm-mode))
      (select-window (display-buffer buffer)))))

(defalias 'aside-vterm-dwim
  (aside-window-toggle-dwim 'vterm #'aside--vterm)
  "DWIM command for the Aside-VTerm window.")

(provide 'aside-vterm)
