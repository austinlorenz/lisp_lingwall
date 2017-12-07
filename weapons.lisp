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

(push #'make-sword *item-builders*)
(push #'make-stick *item-builders*)
(push #'make-gun *item-builders*)
(push #'make-ammo *item-builders*)
(push #'make-plasma_sword *item-builders*)
