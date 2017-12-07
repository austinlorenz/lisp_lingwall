(defstruct (armor (:include object
		  (group 'equipment))) 
)

(defstruct (shield (:include armor
		 (type "a shield")
		 (tile ")")
		 (effect '(progn (setf (creature-AC *player*) 20) (message "You wield a shield (+10).")))
		 )
	   )
)
