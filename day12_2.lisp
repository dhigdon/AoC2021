;;; Day 12 - Passage Pathing

(ql:quickload "hadt")

(defun trim-eos (str)
  "Remove the CRLF from a string's end"
  (string-right-trim '(#\Return #\Newline) str))

(defun small-p (from)
  (lower-case-p (char from 0)))

(defun large-p (from) (not (small-p from)))

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
      with connections = (make-hash-table :test 'equal)
      for l = (read-line s nil)
      while l 
      for line = (trim-eos l)
      for dash = (position #\- line)
      do (add-dag (subseq line 0 dash)
                  (subseq line (1+ dash))
                  connections)
      finally (return connections))))

(defun show-data (data)
  (maphash (lambda (key val) (format t "Key ~A = ~A~%" key val))
           data))

;; ------------------------------------------------------------

(defstruct srch pos path freebee)

;; ------------------------------------------------------------

(defun has-duplicate-smalls (l)
  (cond ((null l) nil)
        ((large-p (car l))
         (has-duplicate-smalls (cdr l)))
        ((member (car l) (cdr l) :test #'string=) t)
        (t (has-duplicate-smalls (cdr l)))))

(defun count-of (i list)
  (loop for l in list counting (string= i l)))

(defun disallowed (dest context)
  (and (small-p dest)
       (> (count-of dest (srch-path context))
          (if (srch-freebee context) 0 1))))

(defun enqueue-srch (pos path queue)
  (hadt:enqueue (make-srch :pos pos 
                           :path (or path (list pos))
                           :freebee (has-duplicate-smalls path))
                queue))

(defun visit (links)
  (let (paths (queue (hadt:make-queue)))
    (enqueue-srch "start" nil queue)
    (loop
      until (hadt:queue-empty-p queue)
      for current = (hadt:dequeue queue)
      do (progn
           (loop
             for dest in (gethash (srch-pos current) links)
             for path = (cons dest (srch-path current))
             unless (disallowed dest current)
             do (if (string= dest "end")
                  (push (reverse path) paths)
                  (enqueue-srch dest path queue)))))
    paths))


(defun part2 (fn)
  (length (visit (read-data fn))))

