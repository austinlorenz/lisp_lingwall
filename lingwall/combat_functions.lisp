(defun to_damage (c)
	(+ (weapon_damage c) (floor (/ (creature-strength c) 5)))
)

(defun melee_skill (c)
	(+ (creature-base_melee_skill c) (* 3 (floor (+ (/ (creature-strength c) 10) (/ (creature-dexterity c) 10)))))
)

(defun hit_probability (c1 c2)
	(/ (- (creature-base_melee_skill c1) 
	      (* 0.75 (creature-AC c2)) 
	   )
	   (creature-base_melee_skill c1)
	)
)

(defun hit_percentage (c1 c2)
	(max 5 (floor (* 100 (hit_probability c1 c2))))
)

(defun weapon_damage (c)
	(cond ((not (creature-weapon c)) 1)
	      (t (roll_dice (first (creature-weapon c)) (second (creature-weapon c))))
	)
)
