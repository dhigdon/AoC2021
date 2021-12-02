;; Day 2
;;


;; Utility to read a list of numbers from a file

(defstruct cmd
  (name  'nop :type symbol)
  (value 0    :type integer))

;; The test data, pre-parsed
(defvar *test*
  (list (make-cmd :name '|forward| :value 5)
        (make-cmd :name '|down|    :value 5)
        (make-cmd :name '|forward| :value 8)
        (make-cmd :name '|up|      :value 3)
        (make-cmd :name '|down|    :value 8)
        (make-cmd :name '|forward| :value 2)))

;; Convert "down 8" into #S(CMD :NAME |down| :VALUE 8)
(defun parse-command (line)
  (let ((space (position #\Space line)))
    (make-cmd
      :name (intern (subseq line 0 space))
      :value (parse-integer line :start (1+ space)))))

(defun read-commands (file)
  "Reads contents of file, returning a list of cmds"
  (with-open-file (s (pathname file))
    (loop for line = (read-line s nil)
          while line
          collect (parse-command line))))

;; The submarine's position structure
(defstruct sub
  (pos   0 :type fixnum)
  (aim   0 :type fixnum)
  (depth 0 :type fixnum))

(defun sub-value (sub)
  (* (sub-pos sub) (sub-depth sub)))

(defun interp1 (sub cmd)
  "Part 1 rules for running commands"
  (case (cmd-name cmd)
    (|forward| (incf (sub-pos sub) (cmd-value cmd)))
    (|down|    (incf (sub-depth sub) (cmd-value cmd)))
    (|up|      (decf (sub-depth sub) (cmd-value cmd))))
  sub)

(defun part1 (cmds)
  (let ((r (reduce #'interp1 cmds :initial-value (make-sub))))
    (values (sub-value r) r)))

(assert (= (part1 *test*) 150))

(defun run-part1 ()
  (part1 (read-commands "day2.txt")))


;; Part 2 changes how commands are interpreted.
;; It also needed a new field in sub
(defun interp2 (sub cmd)
  (case (cmd-name cmd)
    (|forward| (incf (sub-pos sub) (cmd-value cmd))
               (incf (sub-depth sub) (* (cmd-value cmd) (sub-aim sub))))
    (|down|    (incf (sub-aim sub) (cmd-value cmd)))
    (|up|      (decf (sub-aim sub) (cmd-value cmd))))
  sub)

(defun part2 (cmds)
  (let ((r (reduce #'interp2 cmds :initial-value (make-sub))))
    (values (sub-value r) r)))

(assert (= (part2 *test*) 900))

(defun run-part2 ()
  (part2 (read-commands "day2.txt")))

