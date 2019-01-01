;; when macro
(defmacro when (condition &rest body)
  `(if ,condition (progn ,@body)))

;; unless macro
(defmacro unless (condition &rest body)
  `(if (not ,condition) (progn ,@body)))

;; cond macro
(if a
    (do-x)
    (if b
	(do-y)
	(do-z)))
; will be == 
(cond (a (do-x))
      (b (do-y))
      (t (do-z)))


;; loop a list
(dolist (x '(1 2 3)) (print x))

;; loop construct for counting
(dotimes (i 4) (print i))

;; do template
(do (variable-definition*)
    (end-test-form result-form*)
  statement*)

(do ((i 0 (+ i 1))) ;; variable definition
    ((>= i 4)) ;; end-test-form 
  (print i)) ;; statements

;; loop which compute the 10th Fibonacci number
(do (
     (n 0 (+ 1 n))
     (cur 0 next)
     (next 1 (+ cur next)))
    (
     (= 10 n) 
     (print cur)))

;; use loop to compute 11th fibonacci
(loop for i below 10
     and a = 0 then b
     and b = 1 then (+ b 1)
     finally (return a))

