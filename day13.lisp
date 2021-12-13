;;; Day 13 - Transparent Origami

(defun trim-eol (str)
  (string-right-trim '(#\Return #\Newline) str))

(defstruct paper
  dots   ; (list (x . y) ...)
  folds  ; (cons 'x n) or (cons 'y n)
  bounds ; (cons width height)
  )

(defun parse-pair (str)
  "Parse 'x,y' into (x . y)"
  (let ((comma (position #\, str)))
    (assert (and comma (> comma 0)))
    (cons (parse-integer (subseq str 0 comma))
          (parse-integer (subseq str (1+ comma))))))

(defun parse-fold (str)
  "Parse '.... x=num' into (axis . value)"
  (let ((split (position #\= str)))
    (assert (and split (> split 0)))
    (cons (ecase (char str (1- split)) (#\x 'x) (#\y 'y))
          (parse-integer (subseq str (1+ split))))))

(defun find-bounds (dots x y)
  "Return (cols . rows) that will contain all dots"
  (if (null dots)
    (cons x y)
    (let ((dx (caar dots))
          (dy (cdar dots)))
      (find-bounds (cdr dots) (max x dx) (max y dy)))))

(defun read-data (fn)
  "Returns a paper filled from the given file"
  (with-open-file (s (pathname fn))
    (let* ((dots (loop for line = (trim-eol (read-line s ""))
                       until (zerop (length line))
                       collecting (parse-pair line)))
           (folds (loop for l = (read-line s nil)
                        while l
                        collecting (parse-fold (trim-eol l)))))
      (make-paper
        :dots dots
        :folds folds
        :bounds (find-bounds dots 0 0)))))

(defun fold-value (c v)
  "Reflects c over the given axis value"
  (if (<= c v) c (- c (* 2 (- c v)))))

(defun fold-dot (dot fold)
  "Returns the dot as folded by the given fold"
  (ecase (car fold)
    (x (cons (fold-value (car dot) (cdr fold)) (cdr dot)))
    (y (cons (car dot) (fold-value (cdr dot) (cdr fold))))))

(defun fold-dots (dots fold)
  ;; TODO: find a way to only keep non-duplicated dots
  (delete-duplicates 
    (mapcar (lambda (dot) (fold-dot dot fold)) dots)
    :test #'equal))

(defun fold-bounds (bounds fold)
  "Return the bounds of the given bounds after the specified fold."
  (let ((axis (car fold))
        (point (cdr fold)))
    (case axis
      (x (cons point (cdr bounds)))
      (y (cons (car bounds) point)))))

(defun fold-paper (paper)
  "Takes the first fold and runs it. Returns a new, folded paper"
  (let* ((folds   (paper-folds paper))
         (dots    (paper-dots paper))
         (bounds  (paper-bounds paper))
         (fold    (car folds))
         (rest    (cdr folds)))
    (make-paper :dots   (fold-dots dots fold)
                :folds  rest
                :bounds (fold-bounds bounds fold))))

(defun fold-all (paper)
  "Run fold-paper until no folds remain"
  (if (null (paper-folds paper))
    paper
    (fold-all (fold-paper paper))))

(defun show-paper (paper)
  "Output the paper's grid in ASCII art format."
  (dotimes (row (cdr (paper-bounds paper)))
    (dotimes (col (car (paper-bounds paper)))
      (if (member (cons col row) (paper-dots paper) :test #'equal)
        (princ #\#)
        (princ #\.)))
    (terpri)))

(defun part1 (file)
  "How many unique dots after the first fold?"
  (let ((paper (read-data file)))
    (fold-dots (paper-dots paper)
               (car (paper-folds paper)))))

(defun part2 (file)
  "Runs the whole set of folds and displays the final page."
  (let ((paper (read-data file)))
    (show-paper (fold-all paper))))

