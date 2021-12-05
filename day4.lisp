;;; Day 4 - Bingo!

(ql:quickload "str")

(defconstant +size+ 5)
(defconstant +bounds+ (list +size+ +size+)) 

(defun parse-moves (line)
  "Parse a comma separated line of numbers into a list"
  (mapcar #'parse-integer (str:split #\, line)))

(defun make-card ()
  (make-array +bounds+ :initial-element 0 :element-type '(integer -1 99)))

(defun parse-card-row (line card row)
  "Parse a line of 5 bingo card numbers and place them
  in the specified row of card"
  (dotimes (col +size+ card)
    (setf (aref card row col)
          (parse-integer line
                         :start (* col 3)
                         :end   (* (1+ col) 3)))))

(defun read-card (stream)
  "Read a whole card from the stream."
  (let ((card (make-card)))
    (dotimes (row +size+ card)
      (let ((line (read-line stream nil)))
        (unless line (error "Badly formed data"))
        (parse-card-row line card row)))))

;;

(defun mark (card row col)
  (setf (aref card row col) -1))

(defun marked-p (card row col)
  (minusp (aref card row col)))


(defun check-col (card col)
  (loop for row below +size+
        always (marked-p card row col)))

(defun check-row (card row)
  (loop for col below +size+
        always (marked-p card row col)))

(defun mark-spot (card spot)
  "Mark a Row/Col spot and return T if you have bingo"
  (let ((row (car spot))
        (col (cdr spot)))
    (mark card row col)
    (or (check-col card col)
        (check-row card row))))

;;

(defun find-number (card val)
  "Returns the pair (row . col) where 'val' is on the given card,
  or nil if it does not appear."
  (dotimes (row +size+)
    (dotimes (col +size+)
      (when (= val (aref card row col))
        (return-from find-number (cons row col))))))

(defun check-number (card val)
  "If 'val' is in the card, mark it. Returns T if BINGO, nil otherwise."
  (let ((spot (find-number card val)))
    (and spot (mark-spot card spot))))

(defun eval-card (card)
  (loop for row below +size+
        summing (loop for col below +size+
                      sum (if (marked-p card row col)
                            0
                            (aref card row col)))))

(defun read-data (filename)
  (with-open-file (s (pathname filename))
    ;; First, we read the line of numbers to be called
    (let ((numbers (parse-moves (read-line s)))
          (boards  (loop for l = (read-line s nil)
                         while l
                         collecting (read-card s))))
      (values numbers boards))))

(defun part1 (filename)
  "Find the winning card"
  (multiple-value-bind (numbers cards) (read-data filename)
    (do* ((turn 0         (1+ turn))
          (nums numbers   (cdr nums))
          (num (car nums) (car nums)))
      ((null nums))
      (format t "Turn ~A, Draw ~A~%" turn num)
      (dolist (c cards)
        (when (check-number c num)
          (format t "BINGO in turn ~A.~% ~A~%Value = ~A~%"
                  turn c (* num (eval-card c)))
          (return-from part1))))
    (format t "No winner!~%")))


;; Part 2

(defun separate (cards num)
  "Apply 'num' to cards, returning winners and losers lists"
  (let ((winners '())
        (losers '()))
    (dolist (c cards)
      (if (check-number c num)
        (push c winners) ; ideally, only one of these 
        (push c losers)))
    (values winners losers)))

(defun part2 (filename)
  "Find the last card to win"
  (let (winner)
    (multiple-value-bind (numbers cards) (read-data filename)
      (do* ((turn 0         (1+ turn))
            (nums numbers   (cdr nums))
            (num (car nums) (car nums)))
        ((or (null cards) (null nums)))
        (format t "Turn ~A, Draw ~A~%" turn num)
        (multiple-value-bind (win lose) (separate cards num)
          (when win (setf winner (cons num win)))
          (setf cards lose))))
    (when winner
      (* (car winner)
         (eval-card (cadr winner))))))

