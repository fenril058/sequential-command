;;; seq-command-test.el --- test code of seq-command.el  -*- lexical-binding: t; -*-

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

;;

;;; Code:

(require 'seq-command)
(require 'seq-command-config)
(require 'cort)

(defun seq-command-test (string command)
  "Test function for `seq-command.el'."
  (with-temp-buffer
    (insert string)
    (funcall command)
    (buffer-string)))

;; (message (format "%s" seq-command-store-count))

;; (seq-command-setup-keys)

(fset 'test-M-c "\^[c")
(fset 'test-M-l "\^[l")
(fset 'test-M-u "\^[u")

(cort-deftest-generate seq-command-config-test/string :string=
  '(((with-temp-buffer
       (insert "aaa-bbb-ccc-ddd")
       (message (format "%s" seq-command-start-position))
       (message (format "%s" seq-command-store-count))
       (command-execute 'seq-command-capitalize-backward-word)
       (message (format "%s" seq-command-start-position))
       (message (format "%s" seq-command-store-count))
       (message (format "%s" seq-command-skip-count))
       (buffer-string)
       )
     "aaa-bbb-ccc-Ddd")
    ;; ((with-temp-buffer
    ;;    (insert "aaa-bbb-ccc-ddd")
    ;;    (seq-command-capitalize-backward-word)
    ;;    (seq-command-capitalize-backward-word)
    ;;    (buffer-string)
    ;;    )
    ;;  "aaa-bbb-Ccc-Ddd")
    ;; ((with-temp-buffer
    ;;    (insert "aaa-bbb-ccc-ddd")
    ;;    (seq-command-capitalize-backward-word)
    ;;    (seq-command-capitalize-backward-word)
    ;;    (seq-command-capitalize-backward-word)
    ;;    (buffer-string)
    ;;    )
    ;;  "aaa-Bbb-Ccc-Ddd")
    )
  )

;; (cort-deftest-generate seq-command-config-test/upcase :string=
;;   '(((with-temp-buffer
;;        (insert "aaa-bbb-ccc")
;;        (buffer-string)
;;        )
;;      "aaa-bbb-ccc")
;;     ((with-temp-buffer
;;        (insert "aaa-bbb-ccc")
;;        (seq-command-upcase-backward-word)
;;        (buffer-string)
;;        )
;;      "aaa-bbb-CCC")
;;     ((with-temp-buffer
;;        (insert "aaa-bbb-ccc")
;;        (seq-command-upcase-backward-word)
;;        (seq-command-upcase-backward-word)
;;        (buffer-string)
;;        )
;;      "aaa-BBB-CCC")
;;     ((with-temp-buffer
;;        (insert "aaa-bbb-ccc")
;;        (seq-command-upcase-backward-word)
;;        (seq-command-upcase-backward-word)
;;        (seq-command-upcase-backward-word)
;;        (buffer-string)
;;        )
;;      "AAA-BBB-CCC")
;;     )
;;   )

;; (cort-deftest-generate seq-command-test :=
;;   '(((seq-command-test 'seq-command-home) t))
;;   )

;; (macroexpand '(seq-command-define-command foo beginning-of-line beginning-of-buffer))

;; (macroexpand '(seq-command-define-cursor-command beginning-of-line))

(provide 'seq-command-test)
;;; seq-command-test.el ends here
