;;; Day 10 - Syntax Scoring

(defparameter *openers* "([{<")
(defparameter *closers* ")]}>")

(defun get-closer (char)
  (let ((i (position char *openers*)))
    (when i (char *closers* i))))

(defun open-p (char)
  (numberp (position char *openers*)))

(defun score-error (term)
  (let ((i (position term *closers*)))
    (if i
      (svref #(3 57 1197 25137) i)
      0)))

(defun score-incomplete (endings)
  (let ((result 0))
    (dolist (e endings result)
      (setf result (+ (* result 5)
                      (position e *closers*)
                      1)))))

(defun read-to-eol (s)
  (loop for c = (read-char s nil nil)
        while c
        until (char= #\Newline c)))

(defun day10 (fn)
  (with-open-file (s (pathname fn))
    (let (stack errors incompletes)
      (loop
        for c = (read-char s nil nil)
        do (cond
             ;; End of signal
             ((or (null c) (char= c #\Newline))
              (when stack
                (push (score-incomplete stack) incompletes)
                (setf stack nil)))

             ;; Open bracket
             ((open-p c) (push (get-closer c) stack))

             ;; Matched an end
             ((and stack (char= c (car stack))) (pop stack))

             ;; Skip CRs in the data
             ((char= c #\Return))

             ;; Error in signal
             (t (read-to-eol s)
                (push (score-error c) errors)
                (setf stack nil)))
        while c)

      (values
        ; Part1 - sum of the first 3 error values
        (reduce #'+ errors)

        ; Part2 - middle 
        (let ((scores (sort incompletes #'<)))
          (elt scores (floor (length scores) 2)))))))

