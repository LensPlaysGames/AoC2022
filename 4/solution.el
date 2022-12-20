;;; solution.el --- Advent of Code day 4 -*- lexical-binding: t -*-

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



;;; Code:

(message "%s" "Lens_r Advent of Code 2022 day 4 in Emacs Lisp")

(defun decode-bounds ()
  "Return a pair with each side containing a pair of lower and upper bounds, gathered from the current line."
  (interactive "P")
  (beginning-of-line)
  (let ((out (cons (cons 0 0) (cons 0 0))))
    ;; Get first number (first lower bound)
    (setcar (car out) (thing-at-point 'word t))
    ;; Skip to next number
    (re-search-forward "-")
    ;; Get second number (first upper bound)
    (setcdr (car out) (thing-at-point 'word t))

    ;; Skip to next number (second lower bound)
    (re-search-forward ",")

    ;; Get second lower bound
    (setcar (cdr out) (thing-at-point 'word t))
    ;; Skip to second upper bound
    (re-search-forward "-")
    ;; Get second upper bound
    (setcdr (cdr out) (thing-at-point 'word t))

    out))

(with-temp-buffer
  (insert-file-contents "input.txt")
  (print (decode-bounds))
  )
