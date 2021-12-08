;;; Day 8 - Seven Segment Search

(ql:quickload :str)

; "acedgfb cdfbe gcdfa fbcad dab cefabd cdfgeb eafb cagedb ab | cdfeb fcadb cdfeb cdbaf"

(defstruct entry signal output )

(defun sort-chars (s) (sort s #'char<))

(defun parse-entry (str)
  (let ((tokens (str:split #\Space (str:trim str))))
    (map-into tokens #'sort-chars tokens)
    (make-entry
      :signal (subseq tokens 0 10)
      :output (subseq tokens 11))))

(defun read-file (fn)
  (with-open-file (f (pathname fn))
    (loop for entry = (read-line f nil)
          while entry
          collecting (parse-entry entry))))


(defconstant +data+ (read-file "day8.txt"))
(defconstant +test+ (read-file "day8_test.txt"))

;; Part 1 - count the easy to detect digits
;; 1 has 2 segments (c f)
;; 4 has 4 segments (b c d f)
;; 7 has 3 segments (a c f)
;; 8 has 7 segments (a b c d e f g)
;; The rest are not unique
;; 0, 6, 9 have 6 segments in common (a b f g)
;; 2, 3, 5 have 5 segments in common (a d g)

(defun unique-segs (str)
  "Return candidate numbers for each string length"
  (case (length str)
    (2 #\1)
    (3 #\7)
    (4 #\4)
    (5 nil)
    (6 nil)
    (7 #\8)))

(defun count1 (e)
  "Count the segments that match"
  (loop for i in e when (unique-segs i) count 1))

(defun part1 (db)
  "Return number of easily classified segments in the output field"
  (loop for entry in db sum (count1 (entry-output entry))))

;; Part 2 - Deduce the connections

; I failed to do this myself, so I'm taking advantage
; of some found knowledge - once you isolate 4 and 7,
; all other numbers can be detected by their overlaps
; with those two numbers.

; This assoc maps (wires, shared-with-4, shared-with 7) into a digit
(defconstant +numbers-map+
  `(((6 3 3) #\0)
    ((2 2 2) #\1)
    ((5 2 2) #\2)
    ((5 3 3) #\3)
    ((4 4 2) #\4)
    ((5 3 2) #\5)
    ((6 3 2) #\6)
    ((3 2 3) #\7)
    ((7 4 3) #\8)
    ((6 4 3) #\9)))

; There are more efficient ways to do this, but
; efficiency isn't really a problem here.
(defun overlap (a b) (length (intersection a b)))

(defun solve (signal)
  (let (seven four result)

    ; Find the seven and four values from the signal
    ; the inputs are sorted, so these will be unique
    (dolist (s (entry-signal signal))
      (case (length s)
        (4 (setf four (coerce s 'list)))
        (3 (setf seven (coerce s 'list)))))
    (assert (and four seven))

    ; Iterate all digits in the signal
    ; Return value is a number - convert our accumulated value
    (dolist (d (entry-output signal)
               (parse-integer (coerce (nreverse result) 'string)))
      (let* ((lnum (coerce d 'list))
             (key (list (length d)
                        (overlap lnum four)
                        (overlap lnum seven)))
             (spec (assoc key +numbers-map+ :test #'equal)))
        (assert spec)
        (push (cadr spec) result)))))

(defun part2 (data)
  (loop for d in data summing (solve d)))

