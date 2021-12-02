;; Day 2, Ghetto version

(defun part1 (file)
  (with-open-file (s (pathname file))
    (do ((pos 0)
         (depth 0)
         (line (read-line s nil) (read-line s nil)))
      ((null line) (* pos depth))
      (let* ((p (position #\Space line))
             (n (subseq line 0 p))
             (v (parse-integer line :start (1+ p))))
        (cond ((string= n "forward") (incf pos v))
              ((string= n "down")    (incf depth v))
              ((string= n "up")      (decf depth v)))))))

(defun part2 (file)
  (with-open-file (s (pathname file))
    (do ((pos 0)
         (depth 0)
         (aim 0)
         (line (read-line s nil) (read-line s nil)))
      ((null line) (* pos depth))
      (let* ((p (position #\Space line))
             (n (subseq line 0 p))
             (v (parse-integer line :start (1+ p))))
        (cond ((string= n "forward") (incf pos v)
                                     (incf depth (* v aim)))
              ((string= n "down")    (incf aim v))
              ((string= n "up")      (decf aim v)))))))
