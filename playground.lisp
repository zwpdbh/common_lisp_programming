;; usage of let
(let (var)
  (princ var))

(let ((x 5)
      (y 3))
  (terpri)
  (princ x)
  (terpri)
  (princ y)
  (terpri)
  (princ (+ x y)))

;; let you bind variables sequentially
(let* ((x 5)
       (y x))
  (terpri)
  (princ (+ x y)))

(let (x)
  (terpri)
  (princ x)
  (setq x 5)
  (terpri)
  (princ x))


(defun do-command (c)
  (if (eq c 'print)
      (princ 5)
      (if (eq c 'read)
	  (princ 10)))) 
(do-command 'read)


(defun sum (l)
  (if (null l)
      0
      (+ (car l) (sum (cdr l))))) 

;; process list and accumulate the sum of only numbers
(defun sum-numbers (list)
  (cond
    ((null list)
     0)
    ((numberp (car list))
     (+ (car list) (sum-numbers (cdr list))))
    ((symbolp (car list))
     (sum-numbers (cdr list)))
    (t
     (+ (sum-numbers (car list)) (sum-numbers (cdr list))))))
(princ (sum-numbers '((2 3 4) o 1 5 (x (7 8)) (a))))
