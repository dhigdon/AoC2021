;;; Day 21 - Dirac Dice

;; Dice generator (deterministic)
(let ((count 0)
      (val 0))

  ;; Part 1 - deterministic d100
  (defun roll-dice ()
    (incf count)
    (when (= val 100) (setf val 0))
    (incf val))

  (defun reset-dice ()
    (setf count 0 val 0))
  
  (defun dice-count () count)
  (defun dice-last () val)

  )

;; Pawns

(defstruct pawn
  (pos 0 :type integer)
  (score 0 :type integer))

(defun pawn-move (pawn spaces)
  (let* ((next (+ spaces (pawn-pos pawn)))
         (dest (1+ (mod (1- next) 10))))
    (incf (pawn-score pawn) dest)
    (setf (pawn-pos pawn) dest)
    pawn))

(defun pawn-won (pawn)
  (>= (pawn-score pawn) 1000))

(defun roll-turn ()
  (+ (roll-dice) (roll-dice) (roll-dice)))

(defun play1 (start1 start2)
  (let ((p1 (make-pawn :pos start1))
        (p2 (make-pawn :pos start2)))
    (loop
      ;; Player 1's turn
      (pawn-move p1 (roll-turn))
      (when (pawn-won p1)
        (return-from play1
                     (* (pawn-score p2) (dice-count))))

      ;; Player 2's turn
      (pawn-move p2 (roll-turn))
      (when (pawn-won p2)
        (return-from play1
                     (* (pawn-score p1) (dice-count)))))))

(defun part1 ()
  (reset-dice)
  (play1 1 3) ; input data
  )

;; For part 2, I need a way to solve the game without iterating
;; presumably through the use of fancy math.
