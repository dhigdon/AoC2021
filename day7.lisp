;;; Day 7 - The Treachery of Whales

(ql:quickload "str")

(defun parse-int-list (s)
  (sort (loop for n in (str:split #\, s)
              collect (parse-integer n))
        #'<))

(defun read-int-list (fn)
  (with-open-file (f (pathname fn))
    (parse-int-list (read-line f ""))))

(defconstant +test+ (parse-int-list "16,1,2,0,4,2,7,1,2,14"))
(defconstant +data+ (read-int-list "day7.txt"))

(defun diff (x y) (abs (- x y)))

(defun cost1 (crabs n)
  "Fuel cost for all given crabs to move to position n"
  (loop for c in crabs summing (diff c n)))

(defun median (crabs)
  "The central value of crabs. Assumes list is sorted."
  (elt crabs (floor (length crabs) 2)))

(defun part1 (crabs)
  "Part 1 is easy - just take the middle value.
  Retursn the fuel cost and position"
  (let ((v (median crabs)))
    (values (cost1 crabs v) v)))

;; Part 2 actually requires search it would seem.

(defun sum-to (n)
  "Sum of numbers from 0 to n"
  (values (floor (* n (1+ n)) 2)))

(defun cost2 (crabs n)
  "Fuel cost for all crabs to reach position n"
  (loop for c in crabs summing (sum-to (diff c n))))

;; Crabs is a sorted list, so we can easily
;; find the largest and smallest values
(defun min-val (crabs) (first crabs))
(defun max-val (crabs) (car (last crabs)))

(defun part2 (crabs)
  "Search numbers looking for minimum cost"
  (loop for n from (min-val crabs) to (max-val crabs)
        minimizing (cost2 crabs n)))

