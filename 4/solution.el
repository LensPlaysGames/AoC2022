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
    (setcdr (car out) (abs (thing-at-point 'number t)))

    ;; Skip to next number (second lower bound)
    (re-search-forward ",")

    ;; Get second lower bound
    (setcar (cdr out) (thing-at-point 'number t))
    ;; Skip to second upper bound
    (re-search-forward "-")
    ;; Get second upper bound
    (setcdr (cdr out) (abs (thing-at-point 'number t)))

    out))

(defun bounds-l-contains-r-p (bounds)
  "Return T iff the lower bound of lhs of BOUNDS is less than the lower bound of rhs,
AND the upper bound of lhs is greater than the upper bound of rhs."
  (let ((lhs (car bounds))
        (rhs (cdr bounds)))
    (when (<= (car lhs) (car rhs))
      (>= (cdr lhs) (cdr rhs)))))

(defun bounds-r-contains-l-p (bounds)
  "Return T iff the lower bound of rhs of BOUNDS is less than or equal to the lower bound of lhs,
AND the upper bound of rhs is greater than or equal to the upper bound of lhs."
  (let ((lhs (car bounds))
        (rhs (cdr bounds)))
    (when (<= (car rhs) (car lhs))
      (>= (cdr rhs) (cdr lhs)))))

(defun bounds-contains-p (bounds)
  "Return T iff one pair of BOUNDS is entirely contained by the other.
Otherwise NIL."
  (or (bounds-l-contains-r-p bounds)
      (bounds-r-contains-l-p bounds)))


(defun bounds-l-overlaps-r-p (bounds)
  "Return T iff the lower bound of lhs of BOUNDS is less than the upper bound of rhs,
AND the lower bound of lhs is greater than the lower bound of rhs."
  (let ((lhs (car bounds))
        (rhs (cdr bounds)))
    (when (<= (car lhs) (cdr rhs))
      (>= (car lhs) (car rhs)))))

(defun bounds-r-overlaps-l-p (bounds)
  "Return T iff the lower bound of rhs of BOUNDS is less than the upper bound of lhs,
AND the lower bound of rhs is greater than the lower bound of lhs."
  (let ((lhs (car bounds))
        (rhs (cdr bounds)))
    (when (<= (car rhs) (cdr lhs))
      (>= (car rhs) (car lhs)))))

(defun bounds-overlaps-p (bounds)
  "Return T iff one pair of BOUNDS is overlapped at all by the other.
Otherwise NIL."
  (or (bounds-l-overlaps-r-p bounds)
      (bounds-r-overlaps-l-p bounds)))


(with-temp-buffer
  (insert-file-contents "input.txt")
  (let ((total-contained  0)
        (total-overlapped 0))
    (while (not (eobp))
      (let ((bounds (decode-bounds)))
        (when (bounds-contains-p bounds)
          (setq total-contained (1+ total-contained)))
        (when (bounds-overlaps-p bounds)
          (setq total-overlapped (1+ total-overlapped))))
      (next-line))
    (print total-contained)
    (print total-overlapped)))
