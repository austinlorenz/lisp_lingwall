(defparameter *player-inventory* nil)
;;(defstruct object x-location y-location group type tile effect)

(defstruct object (x-location (random *level-width*)) (y-location (random *level-height*)) group type tile effect)

(defun make_objects (n)
    (setf *objects* (mapcar (lambda (x) (funcall (nth (random (length *item-builders*)) *item-builders*)))
      (make-list n))))

(defun pick_up_object ()
 (if (>= (length *player-inventory*) 26) (message "Your inventory is full.")
     (progn (let ((x (get_object_at_position *player-x* *player-y* *objects*)))
	     (push x *player-inventory*)
	     (setf *objects* (remove x *objects*))
	     (message (concatenate 'string "You pick up " (object-type x) ".")) 
	    )
     )
 )
)

(defun walk_over ()
	(mapcar (lambda (z) (when (and (equal (object-x-location z) *player-x*)
				       (equal (object-y-location z) *player-y*)
				  )
			          (message (concatenate 'string "There is " (object-type z) " here."))
			    )
	        )
	*objects*)
)

(defstruct (battery (:include object (type "a battery") 
				     (group 'equipment) 
				     (tile "+") 
		                     (effect '(progn (incf (player-energy *player*) 10) (message "You use the battery."))) 
		    )
	    )
)

(defstruct (root (:include object (type "a root") 
				  (group 'foods) 
				  (tile "%") 
				  (effect '(progn (incf (player-satiation *player*) 100) (message "You eat the root."))) 
		 )
	    )
)

(defstruct (mushroom (:include object (type "a mushroom") 
				  (group 'foods) 
				  (tile "%") 
				  (effect '(progn (incf (player-satiation *player*) 300) (message "You eat the mushroom."))) 
		 )
	    )
)

(defstruct (canteen (:include object (type "a canteen") 
				     (group 'drinks) 
				     (tile "!") 
		                     (effect '(progn (incf (player-hydration *player*) 400) (message "You drink some water."))) 
		    )
	    )
)

(defstruct (armor (:include object
		  (group 'equipment))) 
)

(defstruct (shield (:include armor
		 (type "a shield")
		 (tile ")")
		 (effect '(progn (setf (player-shield *player*) 10) (setf (player-AC *player*) 20) (message "You wield a shield (+10).")))
		 )
	   )
)

(defstruct (weapon (:include object
		  (group 'equipment))) 
		  damage
)

(defstruct (sword (:include weapon
		 (type "a sword")
		 (tile "/")
		 (damage '(2 6))
		 (effect '(progn (setf (creature-weapon *player*) '(2 6)) (message "You wield a sword (2d6).")))
		 ))
)

(defstruct (plasma_sword (:include weapon
		 (type "a plasma sword")
		 (tile "/")
		 (damage '(4 6))
		 (effect '(progn (setf (creature-weapon *player*) '(4 6)) (message "You wield a plasma sword (4d6).")))
		 ))
)

(defstruct (stick (:include weapon
		 (type "a stick")
		 (tile "/")
		 (damage '(1 3))
		 (effect '(progn (setf (creature-weapon *player*) '(1 3)) (message "You wield a stick (1d3).")))
		 ))
)

(defstruct (gun (:include weapon
		 (type "a gun")
		 (tile "=")
		 (damage '(1 6))
		 (effect '(progn (setf (creature-ranged_weapon *player*) '(1 6)) (message "You equip a gun (1d6).")))
		 ))
)

(defstruct (ammo (:include object 
		 (type "ammo") 
		 (group 'equipment) 
		 (tile "-") 
		 (effect '(progn (incf *ammo* 30) (message "You equip ammo.")))
		 )
	   )
)


(push #'make-battery *item-builders*)
(push #'make-root *item-builders*)
(push #'make-mushroom *item-builders*)
(push #'make-canteen *item-builders*)
(push #'make-sword *item-builders*)
(push #'make-stick *item-builders*)
(push #'make-gun *item-builders*)
(push #'make-ammo *item-builders*)
(push #'make-shield *item-builders*)
;;(push #'make-plasma_sword *item-builders*)
