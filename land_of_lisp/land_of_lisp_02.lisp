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
