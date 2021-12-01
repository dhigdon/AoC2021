;; Day 1

;; Utility to read a list of numbers from a file
(defun read-numbers (file)
  (with-open-file (s (pathname file))
    (loop for line = (read-line s nil)
          while line collect (parse-integer line))))

;; Problem is to count the number of increasing readings
;; in the data set.
(defun scanr (last data count)
  "Call as (scanr (car l) (cdr l) 0)"
  (if data
    (scanr (first data) 
           (rest data)
           (if (> (first data) last)
             (1+ count)
             count))
    count))

(defun scan (numbers)
  (scanr (car numbers) (cdr numbers) 0))

;; Part 1
(defun run-part1 ()
  (scan (read-numbers "day1.txt")))

;; For part 2, we need to preprocess the data
;; The data is in a 3-element sliding window,
;; So items (0 1 2) sum to one window,
;; and (1 2 3) sum to another window, etc.
;; Use 3 variables as a sliding window, shifting
;; values through them and taking their sum.
;; If later problems have us using variable or large
;; windows, then we can replace 'a b c' with
;; a vector.

;; Note that this algorithm requires at least 3
;; elements in data, or it will fail
(defun windowize (data)
  (do* ((a (first  data)   b)
        (b (second data)   c)
        (c (third  data)   (first d))
        (d (nthcdr 3 data) (rest d))
        (w (+ a b c)       (+ a b c))
        (r (list w)        (cons w r)))
    ((null d) (nreverse r))))

(defun run-part2 ()
  (scan (windowize (read-numbers "day1.txt"))))

