;;; Day 12 - Passage Pathing

(ql:quickload "hadt")

(defun trim-eos (str)
  "Remove the CRLF from a string's end"
  (string-right-trim '(#\Return #\Newline) str))

(defun small-p (from)
  (lower-case-p (char from 0)))

(defun push-hashvalue (key v table)
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
  (cond ((string= from "start") (push-hashvalue from to table))
        ((string= to   "start") (push-hashvalue to from table))
        ((string= from "end")   (push-hashvalue to from table))
        ((string= to   "end")   (push-hashvalue from to table))
        (t (push-hashvalue from to table)
           (push-hashvalue to from table))))

(defun read-data (fn)
  (with-open-file (s (pathname fn))
    (loop
      with rooms = (make-hash-table :test 'equal)
      for l = (read-line s nil)
      while l 
      for line = (trim-eos l)
      for dash = (position #\- line)
      do (add-dag (subseq line 0 dash)
                  (subseq line (1+ dash))
                  rooms)
      finally (return rooms))))

(defun show-data (data)
  (maphash (lambda (key val) (format t "Key ~A = ~A~%" key val))
           data))

;; ------------------------------------------------------------

(defstruct srch pos nexts)

;; ------------------------------------------------------------

(defun disallowed (dest path)
  (and (small-p dest)
       (member dest path :test #'string=)))

(defun enqueue-srch (pos nexts queue)
  (hadt:enqueue (make-srch :pos pos :nexts nexts)
                queue))

(defun visit (links)
  (let (paths (queue (hadt:make-queue)))
    (enqueue-srch "start" (list "start") queue)
    (loop
      until (hadt:queue-empty-p queue)
      for current = (hadt:dequeue queue)
      do (progn
           (loop
             for dest in (gethash (srch-pos current) links)
             for nextpath = (cons dest (srch-nexts current))
             unless (disallowed dest (srch-nexts current))
             do (if (string= dest "end")
                  (push (reverse nextpath) paths)
                  (enqueue-srch dest nextpath queue)))
           ))
    paths))


(defun part1 (fn)
  (length (visit (read-data fn))))




