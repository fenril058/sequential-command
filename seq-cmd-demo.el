;;; seq-cmd-demo.el --- demo code of seq-cmd.el  -*- lexical-binding: t; -*-

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

;; This is the demonstration of `seq-cmd.el'
;; Execute M-x seq-cmd-demo. And press C-x C-z many times.

;;; Code:

(require 'seq-cmd)
(defconst seq-cmd-demo-version "0.0.1")

(defun seq-cmd-demo ()
  "Demo function of seq-cmd."
  (interactive)
  (global-set-key "\C-x\C-z" 'seq-cmd-count-test)
  (message "Press C-x C-z repeatedly"))

(defun seq-cmd-count-test ()
  "Test function of seq-cmd-count."
  (interactive)
  (message "seq-cmd-count: %d" (seq-cmd-count)))

(provide 'seq-cmd-demo)
;;; seq-cmd-demo.el ends here
