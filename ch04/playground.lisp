
;; the body of a defun consists of any number of lisp expressions.
;; They will be evaluated in order when the function is called and the value of the last expressions 
;; is returned as the value of the function
;; or the return-from special operator can be used to return immediately from anythwere in a function.

(defun verbose-sum (x y)
  "Sum any two numbers after printing a message."
  (format t "Summing ~d and ~d.~%" x y)
  (+ x y))

;; optional parameters
(defun foo (a b &optional c (d 4 d-supplied-p))
  (list a b c d d-supplied-p))

;; Rest parameters
;; Lisp lets you include a catchall parameter after the symbol &rest. If a function includes a &rest parameter, any arguments remaining after values have been doled out to all the required and optional parameters are gathered up into a list that becomes the value of the &rest parameter.


;; Keyword parameters
;; To give a function keyword parameters, after any required, &optional, and &rest parameters you include the symbol &key
(defun foo2 (&key
	       (a 0) 
	       (b 0 b-supplied-p) 
	       (c (+ a b)))
  (list a b c b-supplied-p))
;; Whenever more than one flavor of parameter is used, they must be declared in the order:
;; 1) the name of the required parameters
;; 2) the optional parameters
;; 3) the rest parameters
;; 4) finally the keyword parameters



;; Functions as DATA, a.k.a Higher-order functions
(defun plot (fn min max step)
  (loop for i from min to max by step do
       (loop repeat (funcall fn i) do (format t "*"))
       (format t "~%")))


;; Anonymous Functions
(plot #'(lambda (x) (* 2 x)) 0 10 1)
;; another important use of lambda is makeing closures, functions that 
;; capature part of the environment where they are created
