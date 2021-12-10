;;; Day 9 - Smoke Basin

(ql:quickload :hadt)

(declaim (ftype (function (character) (integer 0 9)) char-digit))
(defun char-digit (c)
  (- (char-code c) (char-code #\0)))

(defun read-data-size (file)
  "Find the array bounds of the data in file"
  (with-open-file (s (pathname file))
    (loop for l = (read-line s nil)
          while l
          for height upfrom 1
          maximizing (1- (length l)) into width
          finally (return (list height width)))))

(defun read-data (file)
  (with-open-file (s (pathname file))
    (loop with field = (make-array (read-data-size file)
                                   :element-type '(integer 0 9)
                                   :initial-element 0)
          for l = (read-line s nil)
          for row upfrom 0
          while l
          do (loop for i below (1- (length l))
                   do (setf (aref field row i)
                            (char-digit (char l i))))
          finally (return field))))

(defun get-value (field row col)
  (declare (fixnum row col) (array field))
  (aref field row col))

(defun neighbor-values (field row col)
  (declare (fixnum row col) (array field))
  (let ((rows (1- (array-dimension field 0)))
        (cols (1- (array-dimension field 1)))
        result)
    (declare (fixnum rows cols))
    (unless (zerop row) (push (get-value field (1- row) col) result))
    (when (< row rows)  (push (get-value field (1+ row) col) result))
    (unless (zerop col) (push (get-value field row (1- col)) result))
    (when (< col cols)  (push (get-value field row (1+ col)) result))
    result))

(defun lowest-p (field row col)
  "A location is lowest if all neighbors are higher."
  (let ((self (get-value field row col))
        (neighbors (neighbor-values field row col)))
    (< self (loop for n in neighbors minimizing n))))

(defun lowest-points (field)
  "Returns a list of (row . col) for all the low points in field"
  (loop for row below (array-dimension field 0)
        nconc (loop for col below (array-dimension field 1)
                    when (lowest-p field row col)
                    collect (cons row col))))

(defun risk-level (field row col)
  "The risk level of a square is depth + 1"
  (1+ (get-value field row col)))

(defun part1 (file)
  "Sum of risk level in all lowest points"
  (let ((field (read-data file)))
    (loop for lows in (lowest-points field)
          summing (risk-level field (car lows) (cdr lows)))))


;; Part 2 - flood fill basins
; Basins surround low points up to the "barrier" height 9 values

; This is a flood fill from <lowpoint> to the 9's
(defun floodfill (field row col border)
  (let ((q (hadt:make-queue))
        (rows (1- (array-dimension field 0)))
        (cols (1- (array-dimension field 1))))
    (hadt:enqueue (cons row col) q)
    (do ((result))
      ((hadt:queue-empty-p q) result)
      (let* ((n (hadt:dequeue q))
             (row (car n))
             (col (cdr n)))
        (when (and (< (get-value field row col) border)
                   (not (member n result :test #'equal)))
          (push n result)
          (when (> row 0)    (hadt:enqueue (cons (1- row) col) q))
          (when (> col 0)    (hadt:enqueue (cons row (1- col)) q))
          (when (< row rows) (hadt:enqueue (cons (1+ row) col) q))
          (when (< col cols) (hadt:enqueue (cons row (1+ col)) q))
          )))))

(defun part2 (file)
  (let* ((field (read-data file))
         (basin (mapcar (lambda (l)
                          (length (floodfill field (car l) (cdr l) 9)))
                        (lowest-points field))))
    ; Return product of largest 3
    (reduce #'* (sort basin #'>) :end 3)))

