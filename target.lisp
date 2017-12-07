(defparameter *target* nil)
(defparameter *target-mode* nil)

(defun nearest_creature ()
  (reduce #'nearer_creature (visible_creatures))
)

(defun nearer_creature (c1 c2)
	(if (< (creature_distance *player* c1)
	       (creature_distance *player* c2)
	    )
	    c1 
	    c2
	)
)

(defun creature_line (c1 c2)
	(coordinate_list (creature-x-location c1) (creature-y-location c1) (creature-x-location c2) (creature-y-location c2))
)
	
(defun creature_distance (c1 c2)
	(euclidean_distance (creature-x-location c1) (creature-y-location c1) (creature-x-location c2) (creature-y-location c2))
)

(defun t_line (a b)
    (when *target-mode*
	(some (lambda (z) (and (equal a (first z)) (equal b (second z)))) (creature_line *player* *target*))
    )
)

;;We want target to cycle through creatures, so that when it is first called, it chooses nearest creature.  If it is called again, it chooses next nearest creature, and so on.  
(defun target ()
    (cond ((not (visible_creatures)) nil)
	  ((not *target*) (setf *target* (nearest_creature)))
	  ((creature-dead *target*) (setf *target* (nearest_creature)))
	  ((not (remove *target* (visible_creatures))) (setf *target-mode* nil))
	  (t (setf *target* (reduce #'nearer_creature (remove *target* (visible_creatures)))))
    )
)

(defun visible_creatures ()
	(let ((l nil))
	    (loop for i repeat (length *creatures*) do
		(when (creature_los *player* (nth i *creatures*)) (push (nth i *creatures*) l)) 
	    )
	l)
)

(defun creature_los (c1 c2)
	(in_line_of_sight (creature-x-location c1) (creature-y-location c1) (creature-x-location c2) (creature-y-location c2))
)
