;; Chapter 8 
(load "graph-util")

(defparameter *congestion-city-nodes* nil)
(defparameter *congestion-city-edges* nil)
(defparameter *visted-nodes* nil)
(defparameter *node-num* 30)
(defparameter *edge-num* 40)
(defparameter *worm-num* 3)
(defparameter *cop-odds* 15)

;; create a random list of edges to connect all nodes:
(defun random-node ()
  ;; random function returns a ruandom natural number less than the integer you pass to it. Here it generate number 1~30
  (1+ (random *node-num*)))

(defun edge-pair (a b)
  (unless (eq a b)
    (list (cons a b) (cons b a))))

(defun make-edge-list ()
  (apply #'append (loop repeat *edge-num*
		       collect (edge-pair (random-node) (random-node)))))

;; Preventing island

;; utility function which find all the edges connecting with a given node
(defun direct-edges (node edge-list)
  (remove-if-not (lambda (x)
		   (eql (car x) node))
		 edge-list))

;; to find islands
;; builds a list of all nodes connected to a given node in edge-list
(defun get-connected (node edge-list)
  (let ((visisted nil))
    (labels ((traverse (node)
	       (unless (member node visisted)
		 (push node visisted)
		 (mapc (lambda (edge)
			 (traverse (cdr edge)))
		       (direct-edges node edge-list)))))
      (traverse node))
    visisted))

;; find isolated islands in nodes given edge-list
(defun find-islands (nodes edge-list)
  (let ((islands nil))
    (labels ((find-island (nodes)
	       (let* ((connected (get-connected (car nodes) edge-list))
		      (unconnected (set-difference nodes connected)))
		 (push connected islands)
		 (when unconnected
		   (find-island unconnected)))))
      (find-island nodes))
    islands))

(defun connect-with-bridge (islands)
  (when (cdr islands)
    (append (edge-pair (caar islands) (caadr islands))
	    (connect-with-bridge (cdr islands)))))

(defun connect-all-islands (nodes edge-list)
  (append (connect-with-bridge (find-islands nodes edge-list)) edge-list))

;; To complete our edges for gongestion city, we need to convert the edges from an edge list into an alist.
(defun make-city-edges ()
  (let* ((nodes (loop for i from 1 to *node-num*
		   collect i))
	 (edge-list (connect-all-islands nodes (make-edge-list)))
	 (cops (remove-if-not (lambda (x)
				(zerop (random *cop-odds*)))
			      edge-list)))
    (add-cops (edges-to-alist edge-list) cops)))

;; when call (edges-to-alist '((1 . 2) (2 . 1)))
;; => ((1 (2)) (2 (1)))
(defun edges-to-alist (edge-list)
  (mapcar (lambda (node1)
	    (cons node1
		  (mapcar (lambda (edge)
			    (list (cdr edge)))
			  (remove-duplicates (direct-edges node1 edge-list)
					     :test #'equal))))
	  (remove-duplicates (mapcar #'car edge-list))))

;; randomly mark some of edges to show that they have cops on them.
;; use the list of cop edges to mark the edges in our alist that contain cops.
;; intersection tell us whichitems are shared between two lists.
(defun add-cops (edge-alist edges-with-cops)
  (mapcar (lambda (x)
	    (let ((node1 (car x))
		  (node1-edges (cdr x)))
	      (cons node1
		    (mapcar (lambda (edge)
			      (let ((node2 (car edge)))
				(if (intersection (edge-pair node1 node2)
						  edges-with-cops
						  :test #'equal)
				    (list node2 'cops)
				    edge)))
			    node1-edges))))
	  edge-alist))

 
;; Building the nodes for Congestion city
