(defun make-cd (title artist rating ripped)
  (list :title title :artist artist :rating rating :ripped ripped))

(defvar *db* nil)

(defun add-record (cd) (push cd *db*))

(defun dump-db ()
  (dolist (cd *db*)
    ;; things between "~{" and "~}" must be a list.
    (format t "~{~a:~10t~a~%~}~%" cd)))

(defun prompt-read (prompt)
  ;; *query-io* is a global variable that contains the input stream connected to the terminal.
  (format *query-io* "~a: " prompt)
  (force-output *query-io*)
  ;; read-line, returns the string it read
  (read-line *query-io*))

;; now combine the existing make-cd function with prompt-read to build a function that makes a new cd record from data it gets by prompting for each value in turn
(defun prompt-for-cd ()
  (make-cd
   (prompt-read "Title")
   (prompt-read "Artist")
   (prompt-read "Rating")
   (prompt-read "Ripped [y/n]")))

;; next line to read: "That's almost right. Except prompt-read returns a string,"






