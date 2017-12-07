(defstruct (weapon (:include object
		  (group 'equipment))) 
)

(defstruct (sword (:include weapon
		 (type "sword")
		 (tile "/")
		 (effect '(progn (setf (creature-weapon *player*) '(2 6)) (message "You wield a sword (2d6).")))
		 ))
)
