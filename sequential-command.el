;;; sequential-command.el --- Many commands into one command -*- lexical-binding: t; -*-

;; Copyright (C) 2009  rubikitch

;; Author: rubikitch <rubikitch@ruby-lang.org>
;; Maintainer: ril <fenril.nh@gmail.com>
;; Keywords: convenience, lisp
;; Version: 1.5.0
;; URL: https://github.com/fenril058/sequential-command
;; Package-Requires: ((cl-lib "0.5"))

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
;; useful. Pressing C-e at the end of line is useless and adding the
;; other behavior in this situation is safe.
;;
;; For example, defining `my-end': if point is at the end of line, go
;; to the end of buffer, otherwise go to the end of line. Just evaluate it!
;;
;; (define-sequential-command my-end  end-of-line end-of-buffer)
;; (global-set-key "\C-e" 'my-end)
;;
;; Consequently, pressing C-e C-e is `end-of-buffer'!
;;
;; `define-sequential-command' is a macro that defines a command whose
;; behavior is changed by sequence of calls of the same command.
;;
;; `seq-cmd-return' is a command to return to the position when sequence
;; of calls of the same command was started.
;;

;;; History:


;; Revision 1.5.0  2024/03/31 ril
;; * New macfo: `define-seq-cmd-for-cursor'
;;   The code is mainly from
;;   <https://github.com/HKey/sequential-command>.
;;
;; Revision 1.4.0  2024/03/29 ril
;; * Delelte bug report command
;; * Delelte demo code
;; * Change the prefix seq- to seq-cmd-
;;   To avoid conflict with the package `seq.el'.
;; * Delete comments
;; * Add autoload keyword
;; * Add group
;; * Set indent of define-sequential-command
;;
;; Revision 1.3  2010/05/04 08:55:35  rubikitch
;; * Add bug report command
;;
;; Revision 1.2  2009/02/17 03:04:18  rubikitch
;; * Add demo.
;; * Rename file name.
;; * New macro: `define-sequential-command'.
;; * New command: `seq-return'.
;;
;; Revision 1.1  2009/02/17 01:24:04  rubikitch
;; * Initial revision
;;

;;; Code:

(eval-when-compile (require 'cl-lib))

(defconst seq-cmd-version "1.5.0")

(defgroup sequential-command nil
  "Many commands into one command."
  :group 'convenience
  :prefix 'seq-cmd)

(defvar seq-cmd-store-count 0)

(defvar seq-cmd-skip-count 0)

(defvar seq-cmd-start-position nil
  "Stores `point' and `window-start' when sequence of calls of the same
 command was started. This variable is updated by `seq-count'")

(defun seq-cmd-count ()
  "Returns number of times `this-command' was executed.
It also updates `seq-cmd-start-position'."
  (if (eq last-command this-command)
      (cl-incf seq-cmd-store-count)
    (setq seq-cmd-start-position  (cons (point) (window-start))
          seq-cmd-store-count 0)))

;;;###autoload
(defmacro define-sequential-command (name &rest commands)
  "Define a sequantial-command the name is NAME.
When NAME command called, it executs a command in COMMANDS
in turn by every call."
  (declare (indent defun))
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
        (aref ,cmdary (mod (seq-cmd-count) ,(length cmdary)))))))

;;;###autoload
(defun seq-cmd-return ()
  "Return to the position when sequence of calls of the same
command was started."
  (interactive)
  (goto-char (car seq-cmd-start-position))
  (set-window-start (selected-window) (cdr seq-cmd-start-position)))

(defun seq-cmd-next ()
  "Skip a command in a sequence and execut next command."
  (setq last-command this-command)
  (cl-incf seq-cmd-skip-count)
  (call-interactively this-command))

;;;###autoload
(defmacro define-seq-cmd-for-cursor (source-command &optional comp-form)
  "Define moving corsor command for seq-cmd.
This macro define the function of whith the name is
seq-cmd-SOURCE-COMMAND-.  It executs SOURCE-COMMAND when
called, and evaluate COMP-FORM.  If COMP-FORM retunrs non-nil value,
`seq-cmd-next' is called after that.  By default COMP-FORM is
\(= seq-cmd-old-point seq-cmd-new-point\), which retunrs t
if the cursor position does not move after executing
SOURCE-COMMAND.  In COMP-FORM, `seq-cmd-old-point'
interpreted as the cursor position before SOURCE-COMMAND executed
and `seq-cmd-new-point' interpreted as the cursorposition
after SOURCE-COMMAND executed."
  (declare (indent function))
  (setq comp-form (or comp-form
                      '(= seq-cmd-old-point seq-cmd-new-point)))
  `(defun ,(intern (concat "seq-cmd-" (symbol-name source-command))) ()
     (interactive)
     (let ((seq-cmd-old-point (point)))
       (call-interactively ',source-command)
       (let ((seq-cmd-new-point (point)))
         (when ,comp-form
           (goto-char seq-cmd-old-point)
           (seq-cmd-next))))))

(provide 'sequential-command)

;;; sequential-command.el ends here
