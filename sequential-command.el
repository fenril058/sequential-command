;;; seq-command.el --- Many commands into one command -*- lexical-binding: t; -*-

;; Copyright (C) 2009  rubikitch

;; Author: rubikitch <rubikitch@ruby-lang.org>
;; Maintainer: ril <fenril.nh@gmail.com>
;; Keywords: convenience, lisp
;; Package-Version: 20220707.00
;; Version: 1.4.0
;; URL: https://github.com/fenril058/seq-command
;; Package-Requires: ((emacs "24.3") (cl-lib "0.5"))

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

;; Integrating multiple commands into one command is sometimes
;; useful.  Pressing C-e at the end of line is useless and adding the
;; other behavior in this situation is safe.
;;
;; For example, defining `my-end': if point is at the end of line, go
;; to the end of buffer, otherwise go to the end of line.  Just
;; evaluate it!
;;
;; (define-seq-command my-end end-of-line end-of-buffer)
;; (global-set-key (kbd "C-e") 'my-end)
;;
;; Consequently, pressing C-e C-e is `end-of-buffer'!
;;
;; `define-seq-command' is a macro that defines a command whose
;; behavior is changed by sequence of calls of the same command.
;;
;; `seq-command-return' is a command to return to the position when
;; sequence of calls of the same command was started.
;;
;; See `seq-command-config.el' if you want examples.

;;; Commands:
;;
;; Below are complete command list:
;;
;; `seq-command-return' returns to the position when sequence of calls
;; of the same command was started.
;;
;;; Customizable Options:
;;
;; Below are customizable option list:
;;

;;; Bug Report:
;;
;; If you have problem, send a bug report via M-x seq-command-send-bug-report.
;; The step is:
;;  0) Setup mail in Emacs, the easiest way is:
;;       (setq user-mail-address "your@mail.address")
;;       (setq user-full-name "Your Full Name")
;;       (setq smtpmail-smtp-server "your.smtp.server.jp")
;;       (setq mail-user-agent 'message-user-agent)
;;       (setq message-send-mail-function 'message-smtpmail-send-it)
;;  1) Be sure to use the LATEST version of seq-command.el.
;;  2) Enable debugger. M-x toggle-debug-on-error or (setq debug-on-error t)
;;  3) Use Lisp version instead of compiled one: (load "seq-command.el")
;;  4) Do it!
;;  5) If you got an error, please do not close *Backtrace* buffer.
;;  6) M-x seq-command-send-bug-report and M-x insert-buffer *Backtrace*
;;  7) Describe the bug using a precise recipe.
;;  8) Type C-c C-c to send.
;;  # If you are a Japanese, please write in Japanese:-)

;;; History:

;; Revision 1.4.0 2022/07/08
;; * Change the name from sequential-command.el to seq-command.el.
;; * Move demo code to seq-command-demo.el
;;
;; Revision 1.3.1 2022/06/30 ril
;; * Change some doc strings.
;; * Change some comments.
;; * Change prefix from seq- to seq-command- to avoid conflict with
;;   the package `seq.el'.
;; * Require Emacs 24.3 or more.
;;
;; Revision 1.3  2010/05/04 08:55:35  rubikitch
;; Added bug report command
;;
;; Revision 1.2  2009/02/17 03:04:18  rubikitch
;; * Add demo.
;; * Rename file name.
;; * New macro: `define-sequential-command'.
;; * New command: `seq-return'.
;;
;; Revision 1.1  2009/02/17 01:24:04  rubikitch
;; Initial revision
;;

;;; Code:

(eval-when-compile (require 'cl-lib))

(defgroup seq-command nil
  "Many commands into one command."
  :group 'convenience)

(defconst seq-command-version "1.4.0")
(defvar seq-command-store-count 0)
(defvar seq-command-start-position nil
  "Store `point' and `window-start' when sequantial-command was started.
This variable is updated by `seq-command-count'.")

(defun seq-command-count ()
  "Return number of times `this-command' was executed.
It also updates `seq-command-start-position'."
  (if (eq last-command this-command)
      (cl-incf seq-command-store-count)
    (setq seq-command-start-position (cons (point) (window-start))
          seq-command-store-count 0)))

(defmacro define-seq-command (name &rest commands)
  "Define a sequantial-command the name is NAME.
When NAME command called, it executs a command in COMMANDS
in turn by every call."
  (let ((cmdary (apply 'vector commands)))
    `(defun ,name ()
       ,(concat "Sequential command of
"
                (mapconcat
                 (lambda (cmd) (format "`%s'" (symbol-name cmd)))
                 commands "
")
                ".")
       (interactive)
       (call-interactively
        (aref ,cmdary (mod (seq-command-count) ,(length cmdary)))))))
;; (macroexpand '(define-seq-command foo beginning-of-line beginning-of-buffer))

(defun seq-command-return ()
  "Return the position when a seq-command was called."
  (interactive)
  (goto-char (car seq-command-start-position))
  (set-window-start (selected-window) (cdr seq-command-start-position)))

;;;; Bug report
(defvar seq-command-maintainer-mail-address
  (concat "fen" "ril.nh@gm" "ail.com"))
(defvar seq-command-bug-report-salutation
  "Describe bug below, using a precise recipe.

When I executed M-x ...

How to send a bug report:
  1) Be sure to use the LATEST version of seq-command.el.
  2) Enable debugger. M-x toggle-debug-on-error
 or (setq debug-on-error t)
  3) Use Lisp version instead of compiled one:
(load \"seq-command.el\")
  4) If you got an error, please paste *Backtrace* buffer.
  5) Type C-c C-c to send.
# If you are a Japanese, please write in Japanese:-)")
(defun seq-command-send-bug-report ()
  "Bug report function for `seq-command.el'."
  (interactive)
  (reporter-submit-bug-report
   seq-command-maintainer-mail-address
   "seq-command.el"
   (apropos-internal "^seq-command" 'boundp)
   nil nil
   seq-command-bug-report-salutation))

(provide 'seq-command)
;; How to save (DO NOT REMOVE!!)
;; (emacswiki-post "seq-command.el")
;;; seq-command.el ends here
