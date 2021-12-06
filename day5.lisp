;;; Day 5


(defstruct seg
  (from-x   0 :type fixnum)
  (from-y   0 :type fixnum)
  (to-x     0 :type fixnum)
  (to-y     0 :type fixnum))

(defun parse-seg (str idx)
  (parse-integer str :start idx :junk-allowed t))

(defun parse-line (str)
  "from-x,from-y -> to-x,to-y"
  (let (v i (seg (make-seg)))
    (multiple-value-setq (v i) (parse-seg str 0))
    (setf (seg-from-x seg) v)
    (multiple-value-setq (v i) (parse-seg str (+ i 1)))
    (setf (seg-from-y seg) v)
    (multiple-value-setq (v i) (parse-seg str (+ i 4)))
    (setf (seg-to-x seg) v)
    (multiple-value-setq (v i) (parse-seg str (+ i 1)))
    (setf (seg-to-y seg) v)
    seg))

(defun read-segs (filename)
  (with-open-file (s (pathname filename))
    (loop for l = (read-line s nil)
          while l
          collecting (parse-line l))))

;;; Bresenham's full line algorithm
;;; Which in hindsight was way overkill.
;;; I didn't read the problem through well enough,
;;; though this did make part 2 a breeze.

(defun plotline-low (x0 y0 x1 y1)
  (let ((dx (- x1 x0))
        (dy (- y1 y0))
        (yi 1)
        (d 0)
        (y y0))
    (declare (fixnum x0 y0 x1 y1 dx dy yi d y))
    (when (< dy 0)
      (setf yi -1
            dy (- dy)))
    (setf d (- (* 2 dy) dx))
    (loop for x from x0 to x1
          collecting (cons x y)
          do (cond ((> d 0)
                    (incf y yi)
                    (incf d (* 2 (- dy dx))))
                   (t (incf d (* 2 dy)))))))

(defun plotline-high (x0 y0 x1 y1)
  (let ((dx (- x1 x0))
        (dy (- y1 y0))
        (xi 1)
        (d 0)
        (x x0))
    (declare (fixnum x0 y0 x1 y1 dx dy xi d x))
    (when (< dx 0)
      (setf xi -1
            dx (- dx)))
    (setf d (- (* 2 dx) dy))
    (loop for y from y0 to y1
          collecting (cons x y)
          do (cond ((> d 0)
                    (incf x xi)
                    (incf d (* 2 (- dx dy))))
                   (t (incf d (* 2 dx)))))))

(defun plotline (x0 y0 x1 y1)
  (if (< (abs (- y1 y0)) (abs (- x1 x0)))
    (if (> x0 x1)
      (plotline-low x1 y1 x0 y0)
      (plotline-low x0 y0 x1 y1))
    (if (> y0 y1)
      (plotline-high x1 y1 x0 y0)
      (plotline-high x0 y0 x1 y1))))


(defun run-seg (seg)
  (plotline (seg-from-x seg) (seg-from-y seg)
            (seg-to-x seg) (seg-to-y seg)))

(defun render (field)
  (let ((width 0) (height 0))
    (loop for v being each hash-key of field
          do (setf width (max width (car v))
                   height (max height (cdr v))))
    (dotimes (y (1+ height))
      (dotimes (x (1+ width))
        (if (gethash (cons x y) field)
          (format t "~A" (gethash (cons x y) field))
          (format t "~A" #\.)))
      (format t "~%"))))

;; PART1 - find overlaps of NON-DIAGONAL LINES ONLY

(defun diagonal-p (seg)
  "Is the seg a diagonal line?"
  (and (/= (seg-from-x seg) (seg-to-x seg))
       (/= (seg-from-y seg) (seg-to-y seg))))

(defun part1 (fn)
  (let ((data (remove-if 'diagonal-p (read-segs fn)))
        (field (make-hash-table :test #'equal)))
    (dolist (d data)
      (dolist (c (run-seg d))
        (if (gethash c field)
          (incf (gethash c field))
          (setf (gethash c field) 1))))
    (loop for v being each hash-value of field
          counting (> v 1))))

(defun part2 (fn)
  (let ((data (read-segs fn))
        (field (make-hash-table :test #'equal)))
    (dolist (d data)
      (dolist (c (run-seg d))
        (if (gethash c field)
          (incf (gethash c field))
          (setf (gethash c field) 1))))
    (loop for v being each hash-value of field
          counting (> v 1))))
