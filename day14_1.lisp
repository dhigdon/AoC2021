;;; Day 14 - Extended Polymerization

(defun trim-eol (str)
  (string-right-trim '(#\Return #\Newline) str))

;; Rules = "AB -> C"
;; parse to ((A . B) . C)

(defun parse-rule (str)
  ;; Brutally simple-minded, since the format never varies"
  (cons (cons (char str 0)
              (char str 1))
        (char str 6)))

(defun rule-a (rule) (caar rule))
(defun rule-b (rule) (cdar rule))
(defun rule-c (rule) (cdr rule))

(defun parse-polymer (str)
  (loop for c across (trim-eol str) collecting c))

(defun read-data (fn)
  (with-open-file (s (pathname fn))
    (let ((polymer (parse-polymer (read-line s))))
      (read-line s)
      (cons polymer (loop for l = (read-line s nil)
                        while l collecting (parse-rule l))))))

(defun make-histogram ()
  (make-array 26 :initial-element 0 :element-type 'integer))

(defun elem-index (element)
  (- (char-code element) (char-code #\A)))

(defun index-elem (index)
  (code-char (+ index (char-code #\A))))

(defun add-element (histogram element)
  (incf (aref histogram (elem-index element))))


(defvar test (read-data "day14_test.txt"))
(defvar data (read-data "day14.txt"))

;; ------------------------------------------------------------

(defun run-rule (a b rules histogram)
  "Match (a . b) against rules. If there's a match,
  return (list c a)."
  (let ((rule (first rules))
        (tail (rest rules)))
    (cond ((null rules) (list a b))
          ((and (char= a (rule-a rule))
                (char= b (rule-b rule)))
           (add-element histogram (rule-c rule))
           (list (rule-c rule) a))
          (t (run-rule a b tail histogram)))))

(defun run-polymer (polymer rules histogram result)
  (cond ((null polymer)
         (nreverse result))
        ((null (rest polymer))
         (nreverse (cons (first polymer) result)))
        (t (run-polymer (rest polymer) rules histogram
                        (nconc (run-rule (first polymer)
                                         (second polymer)
                                         rules histogram)
                               result)))))

(defun iterate-polymer (polymer rules histogram iterations)
  (cond ((zerop iterations) polymer)
        (t (iterate-polymer (run-polymer polymer rules histogram nil)
                            rules histogram
                            (1- iterations)))))

(defun count-elements (histogram)
  (let ((most 0) (least most-positive-fixnum))
    (dotimes (i (length histogram))
      (let ((element (aref histogram i)))
        (unless (zerop element)
          (when (> element most) (setf most element))
          (when (< element least) (setf least element)))))
    (values most least)))

(defun part1 (data iterations)
  (let ((polymer (car data))
        (rules (cdr data))
        (histogram (make-histogram)))
    (dolist (element polymer) (add-element histogram element))
    (iterate-polymer polymer rules histogram iterations)
    (multiple-value-bind (most least) (count-elements histogram)
      (- most least))))

