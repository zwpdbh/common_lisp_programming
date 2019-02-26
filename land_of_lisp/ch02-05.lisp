(defparameter *node* '((living-room (you are in the living-room. A wizard is snoring loudly on the couch))
		       (garden (you are in a beautiful garden. there is a well in front of you))
		       (attic (you are in the attic. there is a giant welding torch in the corner.))))


(defun describe-location (location nodes)
  (cadr (assoc location nodes)))
(describe-location 'living-room *node*)



(defparameter *edge* '((living-room (garden west door) (attic upstairs ladder))
		       (garden (living-room east door))
		       (attic (living-room downstairs ladder))))

(defun describe-path (edge)
  `(there is a ,(caddr edge) going ,(cadr edge) from here.))
;; notice the ` not '
;; caddr is the third element in the list
;; cadddr is the fourth element in the list
(describe-path '(garden west door))

(defun describe-paths (location edges)
  (apply #'append (mapcar #'describe-edge (cdr (assoc location edges)))))

(defparameter *objects* '(whiskey bucket frog chain))

(defparameter *object-locations* '((whiskey living-room)
				   (bucket living-room)
				   (chain garden)
				   (frog garden)))

(defun objects-at (loc objs obj-locs)
  ;; use labels to define local function
  (labels ((at-loc-p (obj)
	     (eq (cadr (assoc obj obj-locs)) loc)))
    ;; remove all things from a list for which a passed-in function doesn't return true.
    (remove-if-not #'at-loc-p objs)))
;; the result of this function returns a filtered list of objects consisting of those items for which at-loc-p is true

(objects-at 'living-room *objects* *object-locations*)

(defun describe-objects (loc objs obj-loc)
  (labels ((describe-obj (obj)
	     `(you see a ,obj on the floor.)))
    (apply #'append (mapcar #'describe-obj (objects-at loc objs obj-loc)))))


(defparameter *location* 'living-room)
(defun look ()
  (append (describe-location *location* *node*)
	  (describe-paths *location* *edge*)
	  (describe-objects *location* *objects* *object-locations*)))

(defun walk (direction)
  (let ((next (find direction
		    (cdr (assoc *location* *edge*))
		    :key #'cadr)))
    (if next
	(progn
	  (setf *location* (car next))
	  (look))
	'(you cannot go that way.))))

(defun pickup (object)
  (cond ((member object (objects-at *location* *objects* *object-locations*))
	 (push (list object 'body) *object-locations*)
	 `(you are now carrying the ,object))
	(t '(you cannot get that.))))


(defun inventory ()
  (cons 'items- (objects-at 'body *objects* *object-locations*)))
