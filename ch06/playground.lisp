;; simple variable
(setf x 10)

;; Array
(setf (aref a 0) 10) ; something like, a[10] = 10

;; Hash table
(setf (gethash 'key hash) 10) ; something like, hash['key'] = 10

;; Slot named 'field':
;; something like, o.field = 10
(setf (field o) 10) 

;; swap a, b
(rotatef a b)

;; shift
(shift a b 10)
;; ==
(let (tmp a) (setf a b b 10) tmp)

(defun foo (x)
  (format t "parameter: ~a~%" x)
  (let ((x 2))
    (format t "outer LET: ~a~%" x)
    (let ((x 3))
      (format t "inner LET: ~a~%" x))
    (format t "outer LET: ~a~%" x))
  (format t "parameter: ~a~%" x))
