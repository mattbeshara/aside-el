;;; aside-context.el --- Contextual information -*- lexical-binding: t -*-

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
;; The Context configuration causes a side window on the right of the frame
;; to be used for buffers which provide contextual information that I only
;; want visible for a short amount of time, such as Help, Apropos, Occur, etc.
;; Having one of these buffers appear or disappear should not cause me to lose
;; my place in any other buffer, but the window they appear in should be big
;; enough to let me see the information I need with a minimum of scrolling.
;;
;; Many of the variables in this file defined with ‘defcustom’ use a custom
;; setter.  If you change the value of those variable outside of Customize and
;; do not use ‘customize-set-variable’ to do so, you may want to call
;; ‘aside-disable-configuration’ before changing the value, and
;; ‘aside-enable-configuration’ after the new value has been set.
;;
;; To activate the Context configuration, do something like the following:
;; (require 'aside-context)
;; (aside-enable-configuration 'context)
;; (define-key global-map (kbd "C-S-h") #'aside-context-dwim)


;;; Code:

(require 'aside)
(require 'aside-hook-functions)
(require 'rx)

(defgroup aside-context ()
  "Options for the Aside-Context window.")

(defcustom aside-context-condition
  (rx (or "*Apropos*"
          "*Embark Collect*"
          "*Embark Export*"
          "*grep*"
          "*Help*"
          "*Occur*"
          "*rg*"
          "*xref*"))
  "Used as a CONDITION in ‘display-buffer-alist’.
Matches the names of buffers that should be displayed in the
Aside-Context window."
  :group 'aside-context
  :type '(choice regexp function)
  :set #'aside-configuration-setter-function)

(defcustom aside-context-action-alist
  '((side . right)
    (window-width . 60))
  "Alist used as the ACTION argument to ‘display-buffer’.
Applied to windows containing buffers matched by
‘aside-context-condition’."
  :group 'aside-context
  :type 'sexp
  :set #'aside-configuration-setter-function)

(defcustom aside-context-hook
  '(aside-hook-change-default-face-height
    aside-hook-enable-truncate-lines
    aside-hook-disable-display-line-numbers-mode)
  "Normal hook run for buffers in the Aside-Context window."
  :group 'aside-context
  :type 'hook
  :options '(aside-hook-change-default-face-height
             aside-hook-enable-truncate-lines
             aside-hook-disable-display-line-numbers-mode))

(defalias 'aside-context-dwim
  (aside-window-toggle-dwim 'context)
  "DWIM command for the Aside-Context window.")

(provide 'aside-context)

;;; aside-context.el ends here
