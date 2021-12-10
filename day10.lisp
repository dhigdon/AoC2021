;;; Day 10 - Syntax Scoring
(ql:quickload "str")

(defun read-data (fname)
  (with-open-file (f (pathname fname))
    (loop for l = (read-line f nil)
          while l
          collecting (str:trim l))))

(defparameter *openers* "([{<")
(defparameter *closers* ")]}>")

(defun score (term)
  (ecase term
    (#\) 3)
    (#\] 57)
    (#\} 1197)
    (#\> 25137)))

(defun get-closer (char)
  (let ((i (position char *openers*)))
    (when i (char *closers* i))))

(defun open-p (char)
  (numberp (position char *openers*)))

(defun match1 (input)
  (with-input-from-string (s input)
    (let (stack (result 0))
      (loop named check
            for c = (read-char s nil)
            while c
            do (cond ((open-p c)
                      (push (get-closer c) stack))

                     ((char= c (car stack))
                      (pop stack))

                     (t (format t "Expecting ~A but got ~A~%"
                                (car stack) c)
                        (setf result (score c)
                              stack nil)
                        (return-from check))))
      result)))

(defun part1 (data)
  (reduce #'+ (mapcar #'match1 data)))

;; Part 2 - discard corrupted lines, repair incomplete lines

(defun score2 (endings)
  (let ((result 0))
    (dolist (e endings result)
      (setf result (+ (* result 5)
                      (position e *closers*)
                      1)))))

(defun match2 (input)
  (with-input-from-string (s input)
    (let (stack)
      (loop named check
            for c = (read-char s nil) while c
            do (cond ((open-p c)
                      (push (get-closer c) stack))

                     ((char= c (car stack))
                      (pop stack))

                     (t ; Corrupted line - cancel
                       (setf stack nil)
                       (return-from check))))
      (score2 stack))))

(defun part2 (data)
  (let ((scores (sort (remove 0 (mapcar #'match2 data)) #'<)))
    (elt scores (floor (length scores) 2))))


(defvar test (read-data "day10_test.txt"))
(defvar data (read-data "day10.txt"))
