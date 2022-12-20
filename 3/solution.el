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

;; Rucksacks with items in them, or something.
;; Split each line of characters in half, and find the character common
;; to both of them. There should only be one.
;; a-z have priorities 1-26, while A-Z have priorities of 27-52.

;;; Code:

(message "%s" "Lens_r Advent of Code 2022 day 3 in Emacs Lisp")

(require 'simple)
(require 'subr-x)

(defvar total-priority 0)
(defvar total-badge-sum 0)

(defun priority-value-from-char (c)
  (if (char-uppercase-p c)
      ;; Uppercase ASCII starts at 41, but we need it to start at 27.
      (- c (- ?A 27))
      ;; Lowercase ASCII starts at 97, but we need it to start at 1.
      (- c (- ?a 1))))

(defun sum-alike-priorities ()
  (interactive "i")
  (setq total-priority 0)
  (with-temp-buffer
    (insert-file-contents "input.txt")
    ;; Loop over each line of input.txt
    (while (not (eobp))
      ;; Collect rucksack as word at point from beginning of line.
      (beginning-of-line)
      (let ((rucksack (thing-at-point 'word t))
            (rucksack-length)
            (rucksack-compartment-length))
        ;; Get rucksack total length and rucksack compartment length.
        (setq rucksack-length (length rucksack))
        (setq rucksack-compartment-length (/ rucksack-length 2))
        ;; Extract left and right compartments as substrings from full rucksack.
        (let ((rucksack-left  (substring rucksack 0 rucksack-compartment-length))
              (rucksack-right (substring rucksack rucksack-compartment-length))
              (priority 0)
              (case-fold-search nil))
          ;;(message "left: %S    right: %S" rucksack-left rucksack-right)
          ;; For every item in left compartment...
          (while (and (= 0 priority) (not (= 0 rucksack-compartment-length)))
            ;; if it is also present in the right compartment, we found priority.
            (if (string-match (char-to-string (string-to-char rucksack-left)) rucksack-right)
                (progn
                  ;;(message "found match in both compartments: %S" (char-to-string (string-to-char rucksack-left)))
                  (setq priority (priority-value-from-char (string-to-char rucksack-left))))
              ;; Pop character
              (progn
                (setq rucksack-left (substring rucksack-left 1))
                (setq rucksack-compartment-length (1- rucksack-compartment-length)))))
          (setq total-priority (+ total-priority priority))))
      (next-line))
    ))

(defun sum-alike-group-badge-priorities ()
  (interactive "i")
  (setq total-badge-sum 0)
  (with-temp-buffer
    (insert-file-contents "input.txt")
    ;; Loop over each line of input.txt
    (while (not (eobp))
      ;; Collect group of three rucksacks.
      (beginning-of-line)
      (let ((rucksack-1 "")
            (rucksack-1-length 0)
            (rucksack-2 "")
            (rucksack-3 "")
            (priority 0)
            (case-fold-search nil))
        (setq rucksack-1 (thing-at-point 'word t))
        (setq rucksack-1-length (length rucksack-1))
        (next-line)
        (beginning-of-line)
        (setq rucksack-2 (thing-at-point 'word t))
        (next-line)
        (beginning-of-line)
        (setq rucksack-3 (thing-at-point 'word t))

        ;; Find character that is present in all three...

        ;; For every character in ONE, check if it is in TWO and THREE.
        ;; If present in all three, set priority (which exits loop).
        (while (and (= 0 priority) (not (= 0 rucksack-1-length)))
          (if (string-match (char-to-string (string-to-char rucksack-1)) rucksack-2)
              (if (string-match (char-to-string (string-to-char rucksack-1)) rucksack-3)
                  (setq priority (priority-value-from-char (string-to-char rucksack-1)))
                (progn
                  (setq rucksack-1 (substring rucksack-1 1))
                  (setq rucksack-1-length (1- rucksack-1-length))))
            (progn
              (setq rucksack-1 (substring rucksack-1 1))
              (setq rucksack-1-length (1- rucksack-1-length)))))
        ;; Add priority to current sum.
        (setq total-badge-sum (+ total-badge-sum priority)))
      ;; Handle next group of three rucksacks.
      (next-line))
    ))

(sum-alike-priorities)
(message "solution pt. 1: %S" total-priority)

(sum-alike-group-badge-priorities)
(message "solution pt. 2: %S" total-badge-sum)

(setq total-priority 0)

;;; solution.el ends here
