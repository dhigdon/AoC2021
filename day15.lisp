;;; Day 15 - Chiton
;;; Minimal cost pathfinding

(ql:quickload "priority-queue")
(import :priority-queue)

(defun trim-eos (str)
  (string-right-trim '(#\Return #\Linefeed) str))

; best-first is a decent solution, though it may be overkill

(defun read-data (fn)
  (let ((cols 0) (rows 0) result)
    ; First, scan for size
    (with-open-file (s (pathname fn))
      (loop for l = (read-line s nil)
            and h upfrom 0
            while l
            maximizing (length (trim-eos l)) into w
            finally (setf cols w  rows h)))
    (setf result (make-array (list rows cols)
                             :initial-element 0
                             :element-type '(integer 0 9)))
    (with-open-file (s (pathname fn))
      (loop for l = (read-line s nil)
            and r upfrom 0
            while l
            do (dotimes (c cols)
                 (setf (aref result r c)
                       (- (char-code (char l c))
                          (char-code #\0))))))
    result))

;; Best-first search

(defstruct coord
  (row 0 :type fixnum)
  (col 0 :type fixnum))

(let (costs bounds)
  (defun init-costs (b)
    (setf bounds b
          costs (make-hash-table :test #'equalp)))

  (defun first-coord ()
    (make-coord :row 0 :col 0))

  (defun last-coord ()
    (make-coord :row (1- (first bounds)) :col (1- (second bounds))))

  (defun get-cost (c) (gethash c costs 0))

  (defun set-cost (c v) (setf (gethash c costs) v))

  (defun valid-coord-p (c)
    (let ((end (last-coord)))
      (and (>= (coord-row end) (coord-row c) 0)
           (>= (coord-col end) (coord-col c) 0))))

  (defun has-cost-p (c)
    "See if a value has been stored for c"
    (nth-value 1 (gethash c costs)))

  )

(defun coord< (a b) (< (get-cost a) (get-cost b)))

(defun merge-neighbors (n q)
  "Priority queue by way of keeping our list sorted."
  ;; This is a shitty way to do a priority queue, but it will work for now
  (merge 'list (sort n #'coord<) q #'coord<))

(defun neighbors (c)
  "Returns all valid unvisited neighbors"
  (let ((row (coord-row c))
        (col (coord-col c)))
    (delete-if
      (lambda (c) (or (not (valid-coord-p c))
                      (has-cost-p c)))
      (list ; No diagonal motion
        (make-coord :row (1- row) :col col)
        (make-coord :row row :col (1- col)) 
        (make-coord :row row :col (1+ col))
        (make-coord :row (1+ row) :col col)))))

(defun get-risk (c grid)
  (aref grid (coord-row c) (coord-col c)))

;; Use BFS to find the cheapest path
(defun find-path-cost (grid start end)
  (set-cost start (get-risk start grid))
  (do ((pq nil)
       (v start (pop pq)))
    ((or (null v) (equalp v end))
     (and v (- (get-cost v) (get-risk start grid))))

    (let ((ns (neighbors v)))
      ;; Compute costs of the neigbors
      (dolist (n ns)
        (set-cost n (+ (get-risk n grid) (get-cost v))))
      ;; Now we can merge them into the priority queue
      (setf pq (merge-neighbors ns pq)))))

;;; -----------------------------------------------------------

(defvar test (read-data "day15_test.txt"))
(defvar data (read-data "day15.txt"))

(defun part1 (d)
  (init-costs (array-dimensions d))
  (find-path-cost d (first-coord) (last-coord)))

;; Part 2 - the maps tesselate in both directions,
;; adding (tile# * (threat + 1)) MOD 9 each direction
;; We can do that by 
