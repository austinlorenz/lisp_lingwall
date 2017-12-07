(defun make_rats (n) 
    (let ((z (rand_accessible)))
	(cond 
	    ((equal n 0) n)
	    (t (push (make-rat :x-location (first z) :y-location (second z)) *creatures*)
	       (make_rats (1- n))
	    )
	)
    )
)

(defun make_wolves (n) 
	(let ((x (random *level-width*)) (y (random *level-height*)))
	(cond 
	    ((equal n 0) n)
	    ((is_accessible x y) 
	    (progn
	        (push (make-wolf :x-location x :y-location y) *creatures*)
	        (make_wolves (1- n))
	    ))
	    (t (make_wolves n))
	)
	)
)

(defun make_cyborgs (n) 
	(let ((x (random *level-width*)) (y (random *level-height*)))
	(cond 
	    ((equal n 0) n)
	    ((is_accessible x y) 
	        (push (make-cyborg :x-location x :y-location y) *creatures*)
		(make_cyborgs (1- n)
	    ))
	    (t (make_cyborgs n))
	)
	)
)

(defun make_dogs (n)
	(cond 
	    ((equal n 0) n)
	    (t (push (make-dog :x-location *stair-x* :y-location *stair-y*) *creatures*)
	       (make_dogs (1- n))
	    )
	)
)

(defun make_miners (n) 
    (let ((z (rand_accessible)))
	(cond 
	    ((equal n 0) n)
	    (t (push (make-mining_machine :x-location (first z) :y-location (second z)) *creatures*)
	       (make_miners (1- n))
	    )
	)
    )
)

(defun make_objects (n) 
	(let ((x (random *level-width*)) (y (random *level-height*)))
	(cond 
	    ((equal n 0) (push (make-sword :x-location x :y-location y) *objects*) 
			 (push (make-shield :x-location (1+ x) :y-location y) *objects*))
	    ((is_floor x y) 
		(let ((z (random 3)))
		 (cond
	            ((equal z 0) (push (make-object :x-location x :y-location y :type "battery" :group 'equipment :tile "-" 
		                        :effect '(progn (incf (player-energy *player*) 10) (message "You use the battery."))) 
		       *objects*))
	            ((equal z 1) (push (make-object :x-location x :y-location y :type "root" :group 'foods :tile "%"
				   :effect '(progn (incf (player-satiation *player*) 300) (message "You eat the root."))) 
		       *objects*))
		    ((equal z 2) (push (make-object :x-location x :y-location y :type "canteen" :group 'drinks :tile "!"
				   :effect '(progn (incf (player-hydration *player*) 400) (message "You drink some water."))) 
		       *objects*))
		)
	     )
	     (make_objects (1- n))
	    )
	    (t (make_objects n))
	)
	)
)
