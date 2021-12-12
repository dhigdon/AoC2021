;;; Day 12 - Passage Pathing

(defun trim-eos (str)
  "Remove the CRLF from a string's end"
  (string-right-trim '(#\Return #\Newline) str))

(defun small-p (from)
  (declare (string from))
  (lower-case-p (char from 0)))

(defun large-p (from)
  (declare (string from))
  (not (small-p from)))

(defun push-hashvalue (key v table)
  (declare (string key v) (hash-table table))
  (let ((ov (gethash key table)))
    (pushnew v ov :test #'string=)
    (setf (gethash key table) ov)))

(defun add-dag (from to table)
  "Generate directed links between from and to according
  to the rule that you can't come from end, go to start"
  ; Note that the data is not provide directed, but
  ; we build our "next" list assuming all nodes but
  ; start and end are bidirectional. The caller is
  ; responsible for making sure that the big/small room
  ; restrictions are obeyed.
  (declare (string from to) (hash-table table))
  (cond ((string= from "start") (push-hashvalue from to table))
        ((string= to   "start") (push-hashvalue to from table))
        ((string= from "end")   (push-hashvalue to from table))
        ((string= to   "end")   (push-hashvalue from to table))
        (t (push-hashvalue from to table)
           (push-hashvalue to from table))))

(defun read-data (fn)
  (with-open-file (s (pathname fn))
    (loop
      with connections = (make-hash-table :test 'equal)
      for l = (read-line s nil)
      while l 
      for line = (trim-eos l)
      for dash = (position #\- line)
      do (add-dag (subseq line 0 dash)
                  (subseq line (1+ dash))
                  connections)
      finally (return connections))))

;; ------------------------------------------------------------

(defstruct srch
  (pos  ""  :type string)
  (path nil :type cons)
  freebee)

(defun get-nexts (context table)
  "Return the list of neighbor nodes in table"
  (gethash (srch-pos context) table))

;; ------------------------------------------------------------

(defun has-duplicate-smalls-p (l)
  (cond ((null l) nil)
        ((large-p (car l))
         (has-duplicate-smalls-p (cdr l)))
        ((member (car l) (cdr l) :test #'string=) t)
        (t (has-duplicate-smalls-p (cdr l)))))

(defun count-of (i list)
  "Number of times the string i appears in list"
  (loop for l in list counting (string= i l)))

(defun disallowed (dest context)
  "Is dest disallowed in the given context?"
  (and (small-p dest)
       (> (count-of dest (srch-path context))
          (if (srch-freebee context) 0 1))))

(defun new-srch (pos path)
  "Create a srch from pos, having come from path"
  (make-srch :pos pos 
             :path (or path (list pos))
             :freebee (has-duplicate-smalls-p path)))

(defun visit (links)
  (let (paths queue)
    (push (new-srch "start" nil) queue)
    (loop
      until (null queue)
      for current = (pop queue)
      do (loop
           for dest in (get-nexts current links)
           for path = (cons dest (srch-path current))
           unless (disallowed dest current)
           do (if (string= dest "end")
                (push (reverse path) paths)
                (push (new-srch dest path) queue))))
    paths))


(defun part2 (fn)
  (length (visit (read-data fn))))

