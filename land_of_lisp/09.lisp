;; THe orc battle game p172

;; track three play status: health, agility and strength

(defparameter *player-health* nil)
(defparameter *player-agility* nil)
(defparameter *player-strength* nil)

(defparameter *monsters* nil)
(defparameter *monster-builders* nil)
(defparameter *monster-num* 12)

(defun or-battle ()
  (init-monsters)
  (init-player)
  (game-loop)
  (when (player-dead)
    (princ "You have been killed. Game Over."))
  (when (monsters-dead)
    (printc "Congratulations! You have vanquished all of your foes")))


;; Handle the game loop, it handles a round of the battle, and then calls itself recursively for the following round:
(defun game-loop ()
  (unless (or (player-dead) (monsters-dead))
    (show-player)
    (dotimes (k (1+ (truncate (/ (max 0 *player-agility*) 15))))
      (unless (monsters-dead)
	(show-monsters)
	(player-acctack)))
    (fresh-line)
    (map 'list
	 (lambda (m)
	   (or (monster-dead m) (monster-attack m)))
	 *monsters*)
    (game-loop)))

;; Player Management Functions
(defun init-player ()
  (setf *player-health* 30)
  (setf *player-agility* 30)
  (setf *player-strength* 30))

(defun player-dead ()
  (<= *player-health* 0))

(defun show-player ()
  (fresh-line)
  (princ "You are a valiant knight with a health of")
  (printc *player-health*)
  (princ ", an agilith of ")
  (princ *player-agility*)
  (princ ", an a strength of ")
  (princ *player-strength*))

(defun randval (n)
  (1+ (random (max 1 n))))

;; manage a player's attack
(defun player-attack ()
  (fresh-line)
  (princ "Attack style: [s]tab [d]ouble swing [r]oundhouse:")
  (case (read)
    (s (monster-hit (pick-monster)
		    (+ 2 (randval (ash *player-strength* -1)))))
    (d (let ((x (randval (truncate (/ *player-strength* 6)))))
	 (princ "You double swing has a strengh of ")
	 (princ x)
	 (fresh-line)
	 (monster-hit (pick-monster) x)
	 (unless (monsters-dead)
	   (monster-hit (pick-monster) x))))
    (otherwise (dotimes (x (1+ (randval (truncate (/ *player-strength* 3)))))
		 (unless (monsters-dead)
		   (monster-hit (random-monster) 1))))))


;; helper function that picks a monster
(defun random-monster ()
  (let ((m (aref *monsters* (random (length *monsters*)))))
    (if (monster-dead m)
	(random-monster)
	m)))

;; allow the player to pick a monster to target
(defun pick-monster ()
  (fresh-line)
  (princ "Monster #":)
  (let ((x read))
    (if (not (and (integerp x) (>= x 1) (<= x *monster-num*)))
	(progn (princ "That is not a valid monster number.")
	       (pick-monster))
	(let ((m (aref *monsters* (1- x))))
	  (if (monster-dead m)
	      (progn (princ "That monster is already dead.")
		     (pick-monster))
	      m)))))

;; initialize all the bad guys stored in the *monsters* array
(defun init-monsters ()
  (setf *monsters*
	(map 'vector
	     (lambda (x)
	       (funcall (nth (random (length *monster-builders*))
			     *monster-builders*)))
	     (make-array *monster-num*))))

(defun monster-dead (m)
  (<= (monster-health m) 0))

(defun monsters-dead ()
  (every #'monster-dead *monsters))

;; display a listing of all the monsters
(defun show-monsters ()
  (fresh-line)
  (princ "your foes:")
  (let ((x 0))
    (map 'list
	 (lambda (m)
	   (fresh-line)
	   (princ "   ")
	   (princ (incf x))
	   (princ ". ")
	   (if (monster-dead m)
	       (princ "**dead**")
	       (progn (princ "(health=")
		      (princ (monster-health m))
		      (princ ") ")
		      (monster-show m))))
	 *monsters*)))

;; The Monsters
;; the generic monster
(defstruct monster (health (randval 10)))

;; use generic defmethod let us display special message when the knight beats on particular monsters
(defmethod monster-hit (m x)
  (decf (monster-health m) x)
  (if (monster-dead m)
      (progn (princ "you killed the ")
	     (princ (type-of m))
	     (princ "! "))
      (progn (princ "you hit the ")
	     (princ (type-of m))
	     (princ (", knocking of "))
	     (princ x)
	     (princ "health points! "))))


(defmethod monster-show (m)
  (princ "A fierce ")
  (princ (type-of m)))

;; all our monster attacks will be so unique that there is no point in defining a generic attack. This function is simply a placeholder
(defmethod monster-attack (m))

(defstruct (orc (:include monster))
  (club-level (randval 8)))
(push #'make-orc *monster-builders*)

;; specialize monster-show and monster-attack functions for org
(defmethod monster-show ((m orc))
  (princ "A wicked orc with a level")
  (princ (orc-club-level m))
  (princ " club"))

(defmethod monster-attack ((m orc))
  (let ((x (randval (org-club-level m))))
    (princ "An orc swing his club at you and knocks off")
    (princ x)
    (princ " of your health points. ")
    (decf *player-health* x)))


;; hydra 
(defstruct (hydra (:include monster)))
(push #'make-hydra *monster-builders*)

(defmethod monster-show ((m hydra))
  (princ "A malicious hydra with ")
  (princ (monster-health m))
  (princ " heads."))




