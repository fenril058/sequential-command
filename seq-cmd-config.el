;;; seq-cmd-config.el --- Examples of seq-cmd.el -*- lexical-binding: t; -*-

;; Copyright (C) 2009  rubikitch

;; Author: rubikitch <rubikitch@ruby-lang.org>
;; Maintainer: ril <fenril.nh@gmail.com>
;; Keywords: extensions, convenience
;; Version: 1.5.0
;; URL: https://github.com/fenril058/sequential-command
;; Package-Requires: ((emacs "24.4"))

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

;; Examples of seq-cmd.el .

;;; Commands:
;;
;; Below are complete command list:
;;
;; `seq-cmd-setup-keys'rebinds C-a, C-e, M-u, M-c, and M-l to
;; seq-cmd-* commands.
;;
;;; Customizable Options:
;;
;; Below are customizable option list:
;;

;;; History:

;; Revision 1.5.0 2022/07/11
;; * Define smart seq-cmd using `define-seq-cmd-for-cursor'
;;   and change using them in `seq-cmd-home', `seq-cmd-end'.
;; * Rewrite `reqruire' to `with-eval-after-load' `org.el'
;;   to prevent from slowing down of the initial process of Emacs.
;; * Now there seems no need to require Emacs 25.1 or more, but using
;;   `with-eval-after-load' requires Emacs 24.4 or more.
;; * New variable: `seq-cmd-home-prefer-back-to-indentation'
;;   this change the behavior of the `seq-commnad-setup-keys'.
;;   Read the doc strings of it to see the details.
;;
;; Revision 1.4.0 2022/07/08
;; * Change the name from sequential-command-config.el to
;;   seq-cmd-config.el.
;;
;; Revision 1.3.1 2022/06/30 ril
;; * Modify some doc stirngs.
;; * Change prefix from seq- to seq-cmd- to avoid conflict with
;;   the package `seq.el'.
;; * Require Emacs 25.1 or more and `org.el'.
;;
;; Revision 1.3  2009/03/22 09:09:58  rubikitch
;; New command: `sequential-command-setup-keys'
;;
;; Revision 1.2  2009/02/17 12:56:26  rubikitch
;; fixed typo
;;
;; Revision 1.1  2009/02/17 03:13:47  rubikitch
;; Initial revision
;;

;;; Code:

(require 'seq-cmd)
(defconst seq-cmd-config-version "1.5.0")

(defcustom seq-cmd-home-prefer-back-to-indentation nil
  "If non-nil `seq-cmd-setup-keys' bind `C-a' to `seq-cmd-home-another'.
It calls `back-to-indentation' firt rather than
`beginning-of-line', which is originaly bind to `C-a'."
  :type 'boolean
  :group 'seq-cmd
  )

(define-seq-cmd-for-cursor back-to-indentation
  (<= seq-cmd-old-point seq-cmd-new-point))
(define-seq-cmd-for-cursor beginning-of-line)
(define-seq-cmd-for-cursor beginning-of-buffer)
(define-seq-cmd-for-cursor end-of-line)
(define-seq-cmd-for-cursor end-of-buffer)

(define-seq-cmd seq-cmd-home
  seq-cmd-beginning-of-line
  beginning-of-buffer
  seq-cmd-return)

(define-seq-cmd seq-cmd-home-another
  seq-cmd-back-to-indentation
  seq-cmd-beginning-of-line
  beginning-of-buffer
  seq-cmd-return)

(define-seq-cmd seq-cmd-end
  seq-cmd-end-of-line
  end-of-buffer
  seq-cmd-return)

(defun seq-cmd-upcase-backward-word ()
  "Upcase the word just before the cursor."
  (interactive)
  (upcase-word (- (1+ (seq-cmd-count)))))

(defun seq-cmd-capitalize-backward-word ()
  "Capitalize the word just before the cursor."
  (interactive)
  (capitalize-word (- (1+ (seq-cmd-count)))))

(defun seq-cmd-downcase-backward-word ()
  "Downcase the word just before the cursor."
  (interactive)
  (downcase-word (- (1+ (seq-cmd-count)))))

(defun seq-cmd-setup-keys ()
  "Rebind `C-a', `C-e', `M-u', `M-c', and `M-l' to seq-cmd-* commands.
If you use `org-mode', rebind `C-a' and `C-e'."
  (interactive)
  (if seq-cmd-home-prefer-back-to-indentation
      (global-set-key "\C-a" 'seq-cmd-home-another)
    (global-set-key "\C-a" 'seq-cmd-home))
  (global-set-key "\C-e" 'seq-cmd-end)
  (global-set-key "\M-u" 'seq-cmd-upcase-backward-word)
  (global-set-key "\M-c" 'seq-cmd-capitalize-backward-word)
  (global-set-key "\M-l" 'seq-cmd-downcase-backward-word)
  (with-eval-after-load 'org
    (define-seq-cmd org-seq-cmd-home
      org-beginning-of-line beginning-of-buffer seq-cmd-return)
    (define-seq-cmd org-seq-cmd-end
      org-end-of-line end-of-buffer seq-cmd-return)
    (define-key org-mode-map "\C-a" 'org-seq-cmd-home)
    (define-key org-mode-map "\C-e" 'org-seq-cmd-end)
    ))

(provide 'seq-cmd-config)
;;; seq-cmd-config.el ends here
