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
    (setcar (car out) (thing-at-point 'number t))
    ;; Skip to next number
    (re-search-forward "-")
    ;; Get second number (first upper bound)
    (setcdr (car out) (thing-at-point 'number t))

    ;; Skip to next number (second lower bound)
    (re-search-forward ",")

    ;; Get second lower bound
    (setcar (cdr out) (thing-at-point 'number t))
    ;; Skip to second upper bound
    (re-search-forward "-")
    ;; Get second upper bound
    (setcdr (cdr out) (thing-at-point 'number t))

    out))

(defun bounds-l-contains-r-p (bounds)
  "Return T iff the lower bound of lhs of BOUNDS is less than the lower bound of rhs,
AND the upper bound of lhs is greater than the upper bound of rhs."
  (let ((lhs (car bounds))
        (rhs (cdr bounds)))
    (when (< (car lhs) (car rhs))
      (> (cdr lhs) (cdr rhs)))))

(defun bounds-r-contains-l-p (bounds)
  "Return T iff the lower bound of rhs of BOUNDS is less than the lower bound of lhs,
AND the upper bound of rhs is greater than the upper bound of lhs."
  (let ((lhs (car bounds))
        (rhs (cdr bounds)))
    (when (< (car rhs) (car lhs))
      (> (cdr rhs) (cdr lhs)))))


(defun bounds-contains-p (bounds)
  "Return T iff one pair of BOUNDS is entirely contained by the other.
Otherwise NIL."
  (or (bounds-l-contains-r-p bounds)
      (bounds-r-contains-l-p bounds)))

(print (bounds-contains-p '((0 . 4) 2 . 3)))
(print (bounds-contains-p '((2 . 3) 0 . 4)))

(print (bounds-contains-p '((3 . 7) 2 . 4)))
(print (bounds-contains-p '((2 . 4) 3 . 7)))

(with-temp-buffer
  (insert-file-contents "input.txt")
  (let ((total 0))
    (while (not (eobp))
      (when (bounds-contains-p (decode-bounds))
        (setq total (1+ total)))
      (next-line)
      )
    (print total)))
