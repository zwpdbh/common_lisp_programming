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
   (or (parse-integer (prompt-read "Rating") :junk-allowed t) 0)
   (y-or-n-p "Ripped [y/n]: ")))



;; loops until the user is done
(defun add-cds ()
  (loop (add-record (prompt-for-cd))
     (if (not (y-or-n-p "Another? [y/n]")) (return))))

;; save the current state of database
(defun save-db (filename)
  (with-open-file (out filename
		       :direction :output
		       :if-exists :supersede)
    (with-standard-io-syntax
	(print *db* out))))

;; load the database back
(defun load-db (filename)
  (with-open-file (in filename)
    (with-standard-io-syntax
      (setf *db* (read in)))))



;; a more general select function which taks a function as an argument
(defun select (selector-fn)
  (remove-if-not selector-fn *db*))

;; wrap up the creation of the anonymous function 
(defun artist-selector (artist)
  #'(lambda (cd) (equal (getf cd :artist) artist)))

;; This function returns an anonymous function that returns the logical AND of one clause per field in our CD records. It returns a selector-fn which combines the four fields
(defun where (&key title artist rating (ripped nil ripped-p))
  #'(lambda (cd)
  (and
   (if title (equal (getf cd :title) title) t)
   (if artist (equal (getf cd :artist) artist) t)
   (if rating (equal (getf cd :rating) rating) t)
   (if rating (equal (getf cd :ripped) ripped) t))))

;; similar, use a passed-in selector function to choose the records to update
;; and using keyword arguments to specfiy the values to change.
(defun update (selector-fn &key title artist rating (ripped nil ripped-p))
  (seft *db*
	(mapcar
	 #'(lambda (row)
	     (when (funcall selector-fn row)
	       (if title (setf (getf row :title) title))
	       (if artist (setf (getf row :artist) artist))
	       (if rating (setf (getf row :rating) rating))
	       (if ripped-p (setf (getf row :ripped) ripped)))
	     row)
	 *db*)))

;; Removing Duplication and Winning big

