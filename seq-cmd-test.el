;;; seq-cmd-test.el --- test code of seq-cmd.el  -*- lexical-binding: t; -*-

;; Copyright (C) 2022  ril

;; Author: ril <fenril.nh@gmail.com>
;; Keywords: convenience, lisp
;; Version: 0.0.1
;; URL: https://github.com/fenril058/sequential-command

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Test code for `seq-cmd.el'

;;; Code:

(require 'seq-cmd)
(require 'seq-cmd-config)
(require 'seq-cmd-demo)
(require 'cort)

(defun seq-cmd-test-insert (string)
  (interactive)
  (insert string))


;; The macro `with-temp-buffer' does not work with key input,
;; because after key input the current buffer becomes *scratch*.
;; Therefore, *scratch* buffer is now used.
(defun seq-cmd-test-string-operations (string command)
  "Test function for `seq-cmd.el'.
Delete the current buffer, insert STRING and execute COMMAND,
and the return the whole string of the buffer."
  (delete-region (point-min) (point-max))
  (insert string)
  (command-execute command)
  (buffer-string))

(defun seq-cmd-test-cursor-operations (string command)
  "Test function for `seq-cmd.el'.
Delete the current buffer, insert STRING and execute COMMAND,
and the return the cursor position."
  (delete-region (point-min) (point-max))
  (insert string)
  (command-execute command)
  (point))

;; (message (format "%s" seq-cmd-start-position))
;; (message (format "%s" seq-cmd-store-count))
;; (message (format "%s" seq-cmd-skip-count))

;;; Creat the command of pushing keys.
;; `seq-demo.el'
(seq-cmd-demo)
(fset 'test-CxCz "\^x\^z")
(fset 'test-CxCzCxCz "\^x\^z\^x\^z")
(fset 'test-CxCzCxCzCxCz "\^x\^z\^x\^z\^x\^z")
;; `seq-cmd-config.el'
(setq seq-cmd-home-prefer-back-to-indentation nil) ; set key bindings of original `sequentail-command-config.el'
(seq-cmd-setup-keys)
(fset 'test-Ca "\^a")
(fset 'test-CaCa "\^a\^a")
(fset 'test-CaCaCa "\^a\^a\^a")
(fset 'test-Ce "\^e")
(fset 'test-CeCe "\^e\^e")
(fset 'test-CeCe "\^e\^e\^e")
(fset 'test-Mc "\^[c")
(fset 'test-McMc "\^[c\^[c")
(fset 'test-McMcMc "\^[c\^[c\^[c")
(fset 'test-Mu "\^[u")
(fset 'test-MuMu "\^[u\^[u")
(fset 'test-MuMuMu "\^[u\^[u\^[u")

;;; test
(cort-deftest-generate seq-cmd-count-test :=
  '(((with-temp-buffer
       (command-execute 'test-CxCz)
       seq-cmd-store-count)
     0)
    ((with-temp-buffer
       (command-execute 'test-CxCzCxCz)
       seq-cmd-store-count)
     1)
    ((with-temp-buffer
       (command-execute 'test-CxCzCxCzCxCz)
       seq-cmd-store-count)
     2)
    ))

(cort-deftest-generate seq-cmd-config-test/string :string=
  '(
    ((seq-cmd-test-string-operations "aaa-bbb-ccc-ddd" 'test-Mc)
     "aaa-bbb-ccc-Ddd")
    ((seq-cmd-test-string-operations "aaa-bbb-ccc-ddd" 'test-McMc)
     "aaa-bbb-Ccc-Ddd")
    ((seq-cmd-test-string-operations "aaa-bbb-ccc-ddd" 'test-McMcMc)
     "aaa-Bbb-Ccc-Ddd")
    ((seq-cmd-test-string-operations "aaa-bbb-ccc-ddd" 'test-Mu)
     "aaa-bbb-ccc-DDD")
    ((seq-cmd-test-string-operations "aaa-bbb-ccc-ddd" 'test-MuMu)
     "aaa-bbb-CCC-DDD")
    ((seq-cmd-test-string-operations "aa-bb" 'test-MuMuMu)
     "AA-BB")
    ))


(cort-deftest-generate seq-cmd-test/position :=
  '(((progn
       (delete-region (point-min) (point-max))
       (insert "aaa
bbb")
       (point)
       )
     8)
    ((seq-cmd-test-cursor-operations "aaa
bbb" 'test-Ca)
     5)
    ((seq-cmd-test-cursor-operations "aaa
bbb" 'test-CaCa)
     1)
    ((seq-cmd-test-cursor-operations "aaa
bbb" 'test-CaCaCa)
     8)
    ((progn
       (delete-region (point-min) (point-max))
       (insert "aaa
bbb")
       (backward-char 3)
       (command-execute 'test-Ca)
       (point)
       )
     1)
    )
  )

;; (macroexpand '(define-seq-cmd foo beginning-of-line beginning-of-buffer))

;; (macroexpand '(define-seq-cmd-for-cursor beginning-of-line))

(provide 'seq-cmd-test)
;;; seq-cmd-test.el ends here
