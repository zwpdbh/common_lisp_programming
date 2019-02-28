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
