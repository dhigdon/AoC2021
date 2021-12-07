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
  "Compute the fuel cost for all given crabs to move to position n"
  (loop for c in crabs summing (diff c n)))

(defun median (crabs)
  (elt crabs (floor (length crabs) 2)))

(defun part1 (crabs)
  "Part 1 is easy - just take the middle value"
  (let ((v (median crabs)))
    (cons v (cost1 crabs v))))

;; Part 2 actually requires search it would seem.
;; There are smarter ways to do this, but this
;; runs nearly instantly and is easy to undertand.

(defun sum-to (n)
  "Sum of numbers from 0 to n"
  (values (floor (* n (1+ n)) 2)))

(defun cost2 (crabs n)
  (loop for c in crabs summing (sum-to (diff c n))))

(defun part2 (crabs)
  "Search numbers looking for minimum cost"
  (let ((mv 0)
        (mc most-positive-fixnum))
    ; Scan over the range of values in the data
    ; The data's sorted, so we can just look at the last
    ; value for the maximum possible position.
    (dotimes (n (1+ (elt crabs (1- (length crabs))))
                (cons mv mc))
      (let ((c (cost2 crabs n)))
        (when (< c mc)
          (setf mv n
                mc c))))))

