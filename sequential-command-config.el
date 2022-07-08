;;; seq-command-config.el --- Examples of seq-command.el -*- lexical-binding: t; -*-

;; Copyright (C) 2009  rubikitch

;; Author: rubikitch <rubikitch@ruby-lang.org>
;; Keywords: extensions, convenience
;; URL: https://github.com/rubikitch/seq-command
;; Package-Requires: ((emacs "25.1"))

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; Examples of seq-command.el .

;;; Commands:
;;
;; Below are complete command list:
;;
;; `seq-command-setup-keys'rebinds C-a, C-e, M-u, M-c, and M-l to
;; seq-command-* commands.
;;
;;; Customizable Options:
;;
;; Below are customizable option list:
;;

;;; History:

;; Revision 1.4.0 2022/07/08
;; * Change the name from sequential-command-config.el to
;;   seq-command-config.el.
;;
;; Revision 1.3.1 2022/06/30 ril
;; * Modify some doc stirngs.
;; * Change prefix from seq- to seq-command- to avoid conflict with
;;   the package `seq.el'.
;; * Require Emacs 25.1 or more and `org.el'.
;;
;; Revision 1.3  2009/03/22 09:09:58  rubikitch
;; New command: `seq-command-setup-keys'
;;
;; Revision 1.2  2009/02/17 12:56:26  rubikitch
;; fixed typo
;;
;; Revision 1.1  2009/02/17 03:13:47  rubikitch
;; Initial revision
;;

;;; Code:

(require 'seq-command)
(defconst seq-command-config-version "1.4.0")

(define-seq-command seq-command-home
  beginning-of-line beginning-of-buffer seq-command-return)
(define-seq-command seq-command-end
  end-of-line end-of-buffer seq-command-return)

(defun seq-command-upcase-backward-word ()
  "Upcase the word just before the cursor."
  (interactive)
  (upcase-word (- (1+ (seq-command-count*)))))

(defun seq-command-capitalize-backward-word ()
  "Capitalize the word just before the cursor."
  (interactive)
  (capitalize-word (- (1+ (seq-command-count*)))))

(defun seq-command-downcase-backward-word ()
  "Downcase the word just before the cursor."
  (interactive)
  (downcase-word (- (1+ (seq-command-count*)))))

(defun seq-command-setup-keys ()
  "Rebind `C-a', `C-e', `M-u', `M-c', and `M-l' to seq-command-* commands.
If you use `org-mode', rebind `C-a' and `C-e'."
  (interactive)
  (global-set-key "\C-a" 'seq-command-home)
  (global-set-key "\C-e" 'seq-command-end)
  (global-set-key "\M-u" 'seq-command-upcase-backward-word)
  (global-set-key "\M-c" 'seq-command-capitalize-backward-word)
  (global-set-key "\M-l" 'seq-command-downcase-backward-word)
  (with-eval-after-load 'org
    (define-seq-command org-seq-command-home
      org-beginning-of-line beginning-of-buffer seq-command-return)
    (define-seq-command org-seq-command-end
      org-end-of-line end-of-buffer seq-command-return)
    (define-key org-mode-map "\C-a" 'org-seq-command-home)
    (define-key org-mode-map "\C-e" 'org-seq-command-end)
    )
  )

(provide 'seq-command-config)
;; How to save (DO NOT REMOVE!!)
;; (emacswiki-post "seq-command-config.el")
;;; seq-command-config.el ends here
