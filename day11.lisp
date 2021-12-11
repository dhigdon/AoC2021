;;; Day 11 - Dumbo Octopus

(defun make-test ()
  (make-array '(10 10) :initial-contents
              '((5 4 8 3 1 4 3 2 2 3)
                (2 7 4 5 8 5 4 7 1 1)
                (5 2 6 4 5 5 6 1 7 3)
                (6 1 4 1 3 3 6 1 4 6)
                (6 3 5 7 3 8 5 4 7 8)
                (4 1 6 7 5 2 4 6 4 5)
                (2 1 7 6 8 4 1 7 2 1)
                (6 8 8 2 8 8 1 1 3 4)
                (4 8 4 6 8 4 8 5 5 4)
                (5 2 8 3 7 5 1 5 2 6))))

(defun make-data ()
  (make-array '(10 10) :initial-contents
              '((6 6 3 6 8 2 7 4 6 5)
                (6 7 7 4 2 4 8 4 3 1)
                (4 2 2 7 3 8 6 3 6 6)
                (7 4 4 7 4 5 2 6 1 3)
                (6 2 2 3 1 2 2 5 4 5)
                (2 8 1 4 3 8 8 7 6 6)
                (6 6 1 5 5 5 1 1 4 4)
                (4 8 3 6 2 3 5 8 3 6)
                (5 3 3 4 7 8 3 2 5 6)
                (4 1 2 8 3 4 4 8 4 3))))

(defun make-simple ()
  (make-array '(5 5) :initial-contents
              '((1 1 1 1 1)
                (1 9 9 9 1)
                (1 9 1 9 1)
                (1 9 9 9 1)
                (1 1 1 1 1))))

(defvar simple (make-simple))
(defvar test (make-test))
(defvar data (make-data))

;; ------------------------------------------------------------

(defconstant +flash-threshold+ 10)

(defun can-flash (data row col)
  (let ((v (aref data row col)))
    (and (numberp v) (>= v +flash-threshold+))))

(defun prime (data row col)
  "Mark this octopus as flashing"
  (setf (aref data row col) nil))

(defun primed (data row col)
  (null (aref data row col)))

(defun flash (data row col)
  (setf (aref data row col) 0))

(defun jolt (data row col)
  "Jolt only affects octopi that are not flashing"
  (unless (primed data row col)
    (incf (aref data row col))))

;; ------------------------------------------------------------

(defun jolt-neighbors (data row col)
  "Increment the halo around (row col)"
  ;; Not the most elegant solution, but....
  (let ((rows (1- (array-dimension data 0)))
        (cols (1- (array-dimension data 1))))
    (when (> row 0)
      (when (> col 0) (jolt data (1- row) (1- col)))
      (jolt data (1- row) col)
      (when (< col cols) (jolt data (1- row) (1+ col))))

    (when (> col 0) (jolt data row (1- col)))
    ;; Center
    (when (< col cols) (jolt data row (1+ col)))

    (when (< row rows)
      (when (> col 0) (jolt data (1+ row) (1- col)))
      (jolt data (1+ row) col)
      (when (< col cols) (jolt data (1+ row) (1+ col))))))

;; ------------------------------------------------------------

(defun advance-time (data)
  "Increase each element's value"
  (dotimes (row (array-dimension data 0))
    (dotimes (col (array-dimension data 1))
      (jolt data row col))))

;; ------------------------------------------------------------

(defun flash-1 (data)
  "Run the data and prime the first eligible octopus.
  Return T if a flash occurs, and NIL otherwise."
  (dotimes (row (array-dimension data 0))
    (dotimes (col (array-dimension data 1))
      (when (can-flash data row col)
        (prime data row col)
        (jolt-neighbors data row col)
        (return-from flash-1 t)))))

;; ------------------------------------------------------------

(defun collect-flashes (data)
  "Flash any charged octopi, resetting them to zero.
  Return number of flashed octopi"
  (let ((flashes 0))
    (dotimes (row (array-dimension data 0))
      (dotimes (col (array-dimension data 1))
        (when (primed data row col)
          (incf flashes)
          (flash data row col))))
    flashes))

;; ------------------------------------------------------------

(defun advance (data)
  "Advance data one turn, returning number of flashes"
  (advance-time data)
  (loop while (flash-1 data))
  (collect-flashes data))

;; ------------------------------------------------------------

(defun part1 (data)
  (let ((count (apply #'* (array-dimensions data))))
    (loop for i below count
          summing (advance data))))

;; ------------------------------------------------------------

(defun part2 (data)
  (let ((count (apply #'* (array-dimensions data))))
    (loop for i upfrom 1
          until (= (advance data) count)
          finally (return i))))

