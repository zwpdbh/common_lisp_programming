;; saying hello to the user 
(defun say-hello ()
  (print "Please type your name")
  (let ((name (read)))
    (print "Nice to meet you, ")
    (print name)))


(defun add-five ()
  (print "please enter a number:")
  (let ((num (read)))
    (print "when I add five I get:")
    (print (+ num 5))))

;; use princ for humans
(progn
  (princ "This sentence will be interrupted")
  (princ #\newline)
  (princ "by an annoying newline character."))

;; setting up a custom REPL simply by calling read, eval, print, and loop
(defun game-repl ()
  (let ((cmd (game-read)))
    (unless (eq (car cmd) 'quit)
      (game-print (game-eval cmd))
      (game-repl))))

;; our custom read
(defun game-read ()
  (let ((cmd (read-from-string (concatenate 'string "(" (read-line) ")"))))
    (flet ((quote-it (x)
	     (list 'quote x)))
      (cons (car cmd) (mapcar #'quote-it (cdr cmd))))))

;; our game-eval which will protect us from hackers to execute any command.
;; game-eval will only be able to execute command related to our game-read
(defparameter *allowed-commands* '(look walk pickup inventory))
(defun game-eval (sexp)
  (if (member (car sexp) *allowed-commands*)
      (eval sexp)
      '(I do not know that command)))

;; Use the benefits of separating the presentation details from the data model
(defun tweak-text (lst caps lit)
  (when lst
    (let ((item (car lst))
	  (rest (cdr lst)))
      (cond ((eq item #\space) (cons item (tweak-text rest caps lit)))
	    ((member item '(#\! #\? #\.)) (cons item (tweak-text rest t lit)))
	    ((eq item #\") (tweak-text rest caps (not lit)))
	    (lit (cons item (tweak-text rest nil lit)))
	    ((or caps lit) (cons (char-upcase item) (tweak-text rest nil lit)))
	    (t (cons (char-downcase item) (tweak-text rest nil nil)))))))

(defun game-print (lst)
  (princ (coerce (tweak-text (coerce (string-trim "() "
						  (prin1-to-string lst))
				     'list)
			     t
			     nil)
		 'string))
  (fresh-line))

;; 6.5 Lambda
(mapcar (lambda (n) (/ n 2)) '(10 20 30))


;; Chapter 7 Going beyong basic lists
;; Creating a Graph
(defparameter *wizard-nodes* '((living-room (you are in the living-room. a wizard is snoring loudly on the couch.))
			       (garden (you are in a beautiful garden. there is a well in front of you.))
			       (attic (you are in the attic. there is a giant welding torch in the corner))))
(defparameter *wizard-edges* '((living-room (garden west door)
				(attic upstairs ladder))
			       (garden (living-room east door))
			       (attic (living-room downstairs ladder))))

;; converting node identifier 
(defun dot-name (exp)
  (substitute-if #\_ (complement #'alphanumericp) (prin1-to-string exp)))

;; adding labels to graph nodes
;; a function that specify the content of the node label
(defparameter *max-label-length* 30)
(defun dot-label (exp)
  (if exp
      (let ((s (write-to-string exp :pretty nil)))  ; it is similar with prin1-to-string, except it does not insert a #\n to the string
	(if (> (length s) *max-label-length*)
					; if the length of s is greater than 30, then concatenate its first 27 + ...
	    (concatenate 'string (subseq s 0 (- *max-label-length* 3)) "...")
	    s))
      ""))

;; Now we can generate both a name and label for each node.
(defun nodes->dot (nodes)
  (mapc (lambda (node)
	  (fresh-line)
	  (princ (dot-name (car node)))
	  (princ "[label=\"")
	  (princ (dot-label node))
	  (princ "\"];"))
	nodes))

;; generate the DOT information for the edges that link our nodes.
(defun edges->dot (edges)
  (mapc (lambda (node)
	  (mapc (lambda (edge)
		  (fresh-line)
		  (princ (dot-name (car node)))
		  (princ "->")
		  (princ (dot-name (car edge)))
		  (princ "[label=\"")
		  (princ (dot-label (cdr edge)))
		  (princ "\"];"))
		(cdr node)))
	edges))

;; Generating all the dota data
(defun graph->dot (nodes edges)
  (princ "digraph{")
  (nodes->dot nodes)
  (edges->dot edges)
  (princ "}"))


;; turning the dot file into a picture
(defun dot->png (fname thunk)
  (with-open-file (*standard-output*
		   fame
		   :direction :output
		   :if-exists :supersede)
    (funcall thunk))
  (ext:shell (concatenate 'string "dot -Tpng -O " fname)))


;; wirting to a file
(with-open-file (my-stream
		 "textfile.txt"
		 :direction :output
		 :if-exists :supersede)
  (princ "Hello File!" my-stream))

;; a function that ties together all the pieces to let us easily create a graph from some nodes and edges
(defun graph->png (fname nodes edges)
  (dot->png fname
	    (lambda ()
	      (graph->dot nodes edges))))


;; for undirected graph
(defun uedges->dot (edges)
  (maplist (lambda (lst)
	     (mapc (lambda (edge)
		     (unless (assoc (car edge) (cdr lst))
		       (fresh-line)
		       (princ (dot-name (caar lst)))
		       (princ "--")
		       (princ (dot-name (car edge)))
		       (princ "[label=\"")
		       (princ (dot-label (cdr edge)))
		       (princ "\"];")))
		   (cdar lst)))
	   edges))

(defun ugraph->dot (nodes edges)
  (princ "graph{")
  (nodes->dot nodes)
  (uedges->dot edges)
  (princ "}"))

(defun ugraph->png (fname nodes edges)
  (dot->png fname
	    (lambda ()
	      (ugraph->dot nodes edges))))

(defun test-edges (edges)
  (let ((count 0))
    (maplist (lambda (lst)
	       (print (length lst))
	       (print lst)
	       (mapc (lambda (edge)
		       (print edge))
		     (cdar lst)))
	     edges)))
