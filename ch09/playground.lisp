;; For instance you wre writing tests for the built-in + function.
;; simplest thing, you want to know if all cases passed
(defun test-+ ()
  (and
   (= (+ 1 2) 3)
   (= (+ 1 2 3) 6)
   (= (+ -1 -3) -4)))

;; you want to know each case's situation
(defun test-+ ()
  (format t "~:[FAIL~;pass~] ... ~a~%" (= (+ 1 2) 3) '(= (+ 1 2) 3))
  (format t "~:[FAIL~;pass~] ... ~a~%" (= (+ 1 2 3) 6) '(= (+ 1 2 3) 6))
  (format t "~:[FAIL~;pass~] ... ~a~%" (= (+ -1 -3) -4) '(= (+ -1 -3) -4)))

;; get rid of the repeated similar calls to FORMAT
(defun report-result (result form)
  (format t "~:[FAIL~;pass~] ... ~a~%" result form))

;; now the test-+ become
(defun test-+ ()
  (report-result (= (+ 1 2) 3) '(= (+ 1 2) 3))
  (report-result (= (+ 1 2 3) 6) '(= (+ 1 2 3) 6))
  (report-result (= (+ -1 -3) -4) '(= (+ -1 -3) -4)))

;; next get rid of the duplication of the test case expression
;; You;d really like is to be able to treat the expression as both cod (to get the result), and data (to use as the label)
;; Whenever you want to treat code as data, there is a sure sign you need a macro
;; (check (= (+ 1 2) 3))
;; should be equal
;; (report-result (= (+ 1 2) 3) '(= (+ 1 2) 3))
(defmacro check (form)
  `(report-result ,form ',form))
(defun test-+ ()
  (check (= (+ 1 2) 3))
  (check (= (+ 1 2 3) 6))
  (check (= (+ -1 -3) -4)))

;; why not get rid of those repeated calls to check?
;; define check to take an arbitary number of forms and wrap them each in a call to report-result
(defmacro check (&body forms)
  `(progn
     ,@(loop for f in forms collect `(report-result ,f ',f))))
(defun test-+ ()
  (check
    (= (+ 1 2) 3)
    (= (+ 1 2 3) 6)
    (= (+ -1 -3) -4)))


;; fix the return value, so it returns the result of the test case it is reporting
(defun report-result (result form)
  (format t "~:[FAIL~;pass~] ... ~a~%" result form)
  result)
;; Now the report-result returns the result of its test case.
;; You need an AND without short-circuiting, and use it in the place of progn and you would be done.
;; You wish
;; (combine-results
;;   (foo)
;;   (bar)
;;   (baz))
;; to be equal
;; (let ((result t))
;;   (unless (foo) (setf result nil))
;;   (unless (bar) (setf result nil))
;;   (unless (baz) (setf result nil))
;;   result)
(defmacro with-gensyms ((&rest names) &body body) ; see chapter08, macro-writing macros
  `(let ,(loop for n in names collect `(,n (gensym)))
     ,@body))

(defmacro combine-results (&body forms)
  (with-gensyms (result)
    `(let ((,result t))
       ,@(loop for f in forms collect `(unless ,f (setf ,result nil)))
       ,result)))

(defmacro check (&body forms)
  `(combine-results
     ,@(loop for f in forms collect `(report-result ,f ',f))))

(defun test-+ ()
  (check
    (= (+ 1 2) 3)
    (= (+ 1 2 3) 6)
    (= (+ -1 -3) -4)))


;; Better Result Reporting
;; Notice the usage of dynamic variables
(defvar *test-name* nil)
(defun report-result (result form)
  (format t "~:[FAIL~;pass~] ... ~a: ~a~%" result *test-name* form)
  result)

(defmacro with-gensyms ((&rest names) &body body) ; see chapter08, macro-writing macros
  `(let ,(loop for n in names collect `(,n (gensym)))
     ,@body))

(defmacro combine-results (&body forms)
  (with-gensyms (result)
    `(let ((,result t))
       ,@(loop for f in forms collect `(unless ,f (setf ,result nil)))
       ,result)))

(defmacro check (&body forms)
  `(combine-results
     ,@(loop for f in forms collect `(report-result ,f ',f))))

(defun test-+ ()
  (let ((*test-name* 'test-+))
    (check
      (= (+ 1 2) 3)
      (= (+ 1 2 3) 6)
      (= (+ -1 -3) -4))))

(defun test-* ()
  (let ((*test-name* 'test-*))
    (check
      (= (* 2 2) 4)
      (= (* 3 5) 15))))

(defun test-arithmetic ()
  (combine-results
   (test-+)
   (test-*)))

;; An abstraction Emerges, need a complete abstraction
;; Express "this is a test function"
(defmacro deftest (name parameters &body body)
  `(defun ,name ,parameters
     (let ((*test-name* ',name))
       ,@body)))
; now you could rewrite test-+ as:
(deftest test-+ ()
  (check
    (= (+ 1 2) 3)
    (= (+ 1 2 3) 6)
    (= (+ -1 -3) -4)))


;; A hierarchy of tests, want a "fully qualified" path, like:
;; pass ... (TEST-ARITHMETIC TEST-+): (= (+ 1 2) 3)
;; To make *test-name* hold a list of test function names instead 
;; of just the name of the most recently entered test function
;; from (let ((*test-name* ',name))
;; to (let ((*test-name* (append *test-name* (list ',name))))

(defmacro deftest (name parameters &body body)
  `(defun ,name ,parameters
     (let ((*test-name* (append *test-name* (list ',name))))
       ,@body)))
; now you could rewrite test-+ as:
(deftest test-+ ()
  (check
    (= (+ 1 2) 3)
    (= (+ 1 2 3) 6)
    (= (+ -1 -3) -4)))

(deftest test-arithmetic ()
  (combine-results
   (test-+)
   (test-*)))


;; summary
;; 1) just AND a bunch of boolean expressions and find out if they all returned true
;; write some simpleminded code, chock-full of duplication 
;; 2) wirte report-result 
;; 3) find out it is tedious and error-prone since you had to pass the test expression twice, once for the value and once as quoted data.
;; 4) so write check macro 
;; 5) modify check to enhance it
;; 6) What if--you fantasized--there was already a non-short-circuiting AND construct. So, you 
;; write combine-result macro 
;; 7) you realize some functions represent a special category of function that deserved its own abstraction
;; so, you write deftest
