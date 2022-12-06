;;; solution.el --- Advent of Code day 1 puzzle 1 -*- lexical-binding: t -*-

;; Author: Lens
;; Maintainer: Lens
;; Version: 0.1.0
;; Package-Requires: (dependencies)


;; This file is not part of GNU Emacs

;; This program is free software: you can redistribute it and/or modify
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

;; A bunch of elves (lists of numbers) have foods and we need to know
;; just how much each one has. A list of integers, empty line
;; separating each, we need to find the largest sum(s).

;;; Code:

(message "%s" "Lens_r Advent of Code 2022 day 1 puzzle 1 in Emacs Lisp")

(defvar solution 0)

(defun add-consecutive-number-list ()
  "Add a list of newline separated numbers."
  (interactive "i")
  (let ((possible-solution 0)
        (tmp))
    (while (setq tmp (thing-at-point 'number t))
      (setq possible-solution (+ possible-solution tmp))
      (next-line))
    (message "possible solution: %i" possible-solution)
    possible-solution))

(defun solve ()
  "Read input.txt, keeping track of the largest list as we go."
  (with-temp-buffer
    (insert-file-contents "input.txt")
    (let ((possible-solution))
      (while (not (eobp))
        (setq possible-solution (add-consecutive-number-list))
        (while (string= "\n" (thing-at-point 'line t))
          (next-line))
        (when (> possible-solution solution)
          (message "%s: %i" "got new solution" possible-solution)
          (setq solution possible-solution)))
      )
    )
  )

(solve)
(message "%i" solution)

;;; solution.el ends here
