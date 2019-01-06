;; brute force to decide if the number is prime number or not
(defun primep (number)
  (when (> number 1)
    (loop for fac from 2 to (isqrt number) 
       never (zerop (mod number fac)))))

;; given a number, get its next prime number
(defun next-prime (number)
  (loop for n from number when (primep n) return n))


;; (do (variable-definitions*)
;;     (end-test-form result-form*)
;;  statement*)
(do ((p (next-prime 0) 
	(next-prime (1+ p))))  ; variable-definitions
    ((> p 19))                 ; end-test-form 
  (format t "~d " p))          ; statement


;; the macro we indent to use is:
(do-primes (p 0 19)
  (format t "~d " p))
;; which print out prime number from 0 to 19, p will hold the prime number

;; (defmacro do-primes (var-and-range &rest body)
;;   (let ((var (first var-and-range))
;; 	(start (second var-and-range))
;; 	(end (third var-and-range)))
;;     `(do ((,var (next-prime ,start) (next-prime (1+ ,var))))
;; 	 ((> ,var ,end))
;;        ,@body)))

;; (defmacro do-primes ((var start end) &body body)
;;   `(do ((,var (next-prime ,start) (next-prime (1+ ,var)))
;; 	(ending-value ,end))
;;        ((> ,var ,ending-value))
;;      ,@body))

(defmacro do-primes ((var start end) &body body)
  (let ((ending-value-name (gensym)))
    `(do ((,var (next-prime ,start) (next-prime (1+ ,var)))
	  (,ending-value-name ,end))
	 ((> ,var ,ending-value-name))
       ,@body)))


;; you want to write a micro to generate micro to capture the pattern when you write micro
;; you want to create a macro with-gensyms, which could be used as
(defmacro do-primes ((var start end) &body body)
  (with-gensyms (ending-value-name)
    `(do ((,var (next-prime ,start) (next-prime (1+ ,var)))
          (,ending-value-name ,end))
         ((> ,var ,ending-value-name))
       ,@body)))
;; so the result will be a macro which is = to previous macro
;; In other words, the with-gensyms needs to expand into a LET that bind each named variable, ending-value-name in this case to a gensymed symbol.
(defmacro with-gensyms ((&rest names) &body body)
  `(let ,(loop for n in names collect `(,n (gensym)))
     ,@body))


