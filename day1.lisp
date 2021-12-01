;; Day 1

;; Utility to read a list of numbers from a file
(defun read-numbers (file)
  (with-open-file (s (pathname file))
    (loop for line = (read-line s nil)
          while line collect (parse-integer line))))

;; Counts number of times paired numbers in data are ascending.
(defun scan (data)
  "Counting the number of times the value in the list increases."
  (let ((count 0))
    ;; There is probably a cleaner way to do this....
    (reduce (lambda (e1 e2)
              (when (< e1 e2) (incf count))
              e2)
            data)
    count))

;; N-wide windowizer, converts list of numbers
;; into a list of sums of N-element long overlapping windows
(defun sum (seq) (reduce #'+ seq))
(defun rotseq (seq v) (append (rest seq) (list v)))
(defun windowize (len data)
  ;; TODO - implement with 'w' as an array
  (do* ((w (subseq data 0 len) (rotseq w (first d)))
        (d (subseq data len)   (rest d))
        (r (list (sum w))      (cons (sum w) r)))
    ((null d) (nreverse r))))

;; Part 1 - count raw data
(defun run-part1 ()
  (scan (read-numbers "day1.txt")))

;; Part 2 - count windowed data
(defun run-part2 ()
  (scan (windowize 3 (read-numbers "day1.txt"))))

;; Some test data and unit tests
(defvar *test* '(199 200 208 210 200 207 240 269 260 263))
(assert (= (scan *test*) 7))
(assert (equal (windowize 3 *test*) '(607 618 618 617 647 716 769 792)))
(assert (= (scan (windowize 3 *test*)) 5))

