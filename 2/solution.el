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

;; I'm supposed to be playing in some crazy ro sham bo tournament.
;; Rules:
;; Each round you get points for the shape you select,
;;   ROCK     -> 1
;;   PAPER    -> 2
;;   SCISSORS -> 3
;; as well as the outcome of the round:
;;   LOSS  -> 0
;;   DRAW  -> 3
;;   WIN   -> 3

;;; Code:

(message "%s" "Lens_r Advent of Code 2022 day 2 in Emacs Lisp")

(defun shape-to-score (shape-character)
  "Convert a shape string into a score value."
  (if (or (string= "A" shape-character) (string= "X" shape-character))
      1
    (if (or (string= "B" shape-character) (string= "Y" shape-character))
        2
      (if (or (string= "C" shape-character) (string= "Z" shape-character))
          3
        (error "Unsupported shape")))))

(defun scores-from-shapes (left right)
  "Convert two ro sham bo shapes in a given round into a score pair."
  (let ((left-score  (shape-to-score left))
        (right-score (shape-to-score right)))
    ;; DRAW
    (when (= left-score right-score)
      (setq left-score  (+ 3 left-score))
      (setq right-score (+ 3 right-score)))
    ;; LEFT WIN
    (when (and (= 1 left-score) (= 3 right-score))
      (setq left-score (+ 6 left-score)))
    (when (and (= 2 left-score) (= 1 right-score))
      (setq left-score (+ 6 left-score)))
    (when (and (= 3 left-score) (= 2 right-score))
      (setq left-score (+ 6 left-score)))
    ;; RIGHT WIN
    (when (and (= 1 left-score) (= 2 right-score))
      (setq right-score (+ 6 right-score)))
    (when (and (= 2 left-score) (= 3 right-score))
      (setq right-score (+ 6 right-score)))
    (when (and (= 3 left-score) (= 1 right-score))
      (setq right-score (+ 6 right-score)))
    (cons left-score right-score)))

(defun shapes-from-current-line ()
  (interactive "i")
  (let ((shapes '("X" . "Y")))
    (setcar shapes (thing-at-point 'word t))
    (forward-char 2)
    (setcdr shapes (thing-at-point 'word t))
    shapes))

(defun calculate-strategy-guide-scores ()
  (interactive "i")
  (with-temp-buffer
    (insert-file-contents "input.txt")
    (let ((count 0)
          (shapes)
          (scores)
          (totals '(0 . 0)))
      (while (not (eobp))
        (setq shapes (shapes-from-current-line))
        (next-line)
        (beginning-of-line)
        (setq scores (scores-from-shapes (car shapes) (cdr shapes)))
        ;;(message "scores -> shapes :: %S -> %S" scores shapes)
        (setq count (+ 1 count))
        (setcar totals (+ (car scores) (car totals)))
        (setcdr totals (+ (cdr scores) (cdr totals))))
      totals)))

(message "solution: %S" (calculate-strategy-guide-scores))

;;; solution.el ends here
