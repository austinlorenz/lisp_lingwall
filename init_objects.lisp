(defun make_objects (n) 
	(let ((x (random *level-width*)) (y (random *level-height*)))
	(cond 
	    ((equal n 0) nil)
	    ((is_floor x y) 
		(let ((z (random 3)))
		 (cond
	            ((equal z 0) (push (make-object :x-location x :y-location y :type "battery" :group 'equipment :tile "+" 
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

(defun make_weapons (n)
	(let ((x (random *level-width*)) (y (random *level-height*)))
	(cond 
	    ((equal n 0) nil)
	    ((is_floor x y) 
		(let ((z (random 4)))
		 (cond
		    ((equal z 0) (push (make-sword :x-location x :y-location y) *objects*))
		    ((equal z 1) (push (make-shield :x-location x :y-location y) *objects*))
		    ((equal z 2) (push (make-gun :x-location x :y-location y) *objects*))
		    ((equal z 3) (push (make-object :x-location x :y-location y :type "ammo" :group 'equipment :tile "-" 
				  :effect '(progn (incf *ammo* 10) (message "You equip the ammo."))) *objects*))
		)
	     )
	     (make_weapons (1- n))
	    )
	    (t (make_weapons n))
	)
	)
)


