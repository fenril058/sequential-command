;;; seq-command-demo.el --- demo code of seq-command.el  -*- lexical-binding: t; -*-

;; Copyright (C) 2022  ril

;; Author: ril <fenril.nh@gmail.com>
;; Keywords: convenience, lisp
;; Version: 0.0.1

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

;; This is the demonstration of `seq-command.el'
;; Execute M-x seq-command-demo. And press C-x C-z many times.

;;; Code:

(require 'seq-command)
(defconst seq-command-demo-version "0.0.1")

(defun seq-command-demo ()
  "Demo function of seq-command."
  (interactive)
  (global-set-key "\C-x\C-z" 'seq-command-count-test)
  (message "Press C-x C-z repeatedly"))

(defun seq-command-count-test ()
  "Test function of seq-command-count."
  (interactive)
  (message "seq-command-count: %d" (seq-command-count)))

(provide 'seq-command-demo)
;;; seq-command-demo.el ends here
