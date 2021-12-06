;;; Day 6 - Lantern Fish

(ql:quickload "str")          ; for string split
(ql:quickload "alexandria")   ; for array element rotate

(defpackage :day6
  (:use :common-lisp)
  (:import-from :alexandria :rotate)
  (:import-from :str :split)
  (:export :*test* :*data* :run-part1 :run-part2))
(in-package :day6)

(defun parse-fish (line)
  "Parse a comma separated line of numbers into a list"
  (mapcar #'parse-integer (split #\, line)))

(defvar *test* (parse-fish "3,4,3,1,2"))
(defvar *data* (parse-fish "2,1,2,1,5,1,5,1,2,2,1,1,5,1,4,4,4,3,1,2,2,3,4,1,1,5,1,1,4,2,5,5,5,1,1,4,5,4,1,1,4,2,1,4,1,2,2,5,1,1,5,1,1,3,4,4,1,2,3,1,5,5,4,1,4,1,2,1,5,1,1,1,3,4,1,1,5,1,5,1,1,5,1,1,4,3,2,4,1,4,1,5,3,3,1,5,1,3,1,1,4,1,4,5,2,3,1,1,1,1,3,1,2,1,5,1,1,5,1,1,1,1,4,1,4,3,1,5,1,1,5,4,4,2,1,4,5,1,1,3,3,1,1,4,2,5,5,2,4,1,4,5,4,5,3,1,4,1,5,2,4,5,3,1,3,2,4,5,4,4,1,5,1,5,1,2,2,1,4,1,1,4,2,2,2,4,1,1,5,3,1,1,5,4,4,1,5,1,3,1,3,2,2,1,1,4,1,4,1,2,2,1,1,3,5,1,2,1,3,1,4,5,1,3,4,1,1,1,1,4,3,3,4,5,1,1,1,1,1,2,4,5,3,4,2,1,1,1,3,3,1,4,1,1,4,2,1,5,1,1,2,3,4,2,5,1,1,1,5,1,1,4,1,2,4,1,1,2,4,3,4,2,3,1,1,2,1,5,4,2,3,5,1,2,3,1,2,2,1,4"))

(defun live (fish)
  "Takes a fish, returns a list of that fish and any children"
  (if (= fish 0)
    (list 6 8)
    (list (1- fish))))

(defun do-day (fishes)
  (loop for f in fishes append (live f)))

(defun run-part1 (fishes days)
  (if (zerop days)
    fishes
    (run-part1 (do-day fishes) (1- days))))

;; Part two blows up badly, so a different approach is needed
;; Some smarter people than me suggested "inverting" the domain
;; of the problem, which turns out to be very simple.
;; Basically, we represent "fish per day" not "days per fish".

(defun run-part2 (fishes days)
  (let ((daygrid (make-array 9 :initial-element 0)))
    ;; How many fish in each day?
    (dolist (f fishes)
      (incf (aref daygrid f)))
    ;; Run the days
    (dotimes (d days)
      (rotate daygrid -1)
      (incf (aref daygrid 6) (aref daygrid 8)))
    ;; Add up all the fish. That's the answer.
    (loop for d across daygrid summing d)))

