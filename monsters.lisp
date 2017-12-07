(defparameter *creatures* nil)
(defparameter *killed-creatures* nil)
(defparameter *objects* nil)

(defstruct creature
	   x-location
	   y-location
	   x-destination
	   y-destination
	   type
	   species
	   class
	   height
	   weight
	   age
	   strength 
	   dexterity
	   constitution
	   hitpoints
	   AC
	   weapon
	   ranged_weapon
	   shield
	   base_melee_skill
	   jumpcost
	   status_effects
	   attack_style
	   unique
	   expval
	   inventory
	   tile)

(defun creature-dead (x)
    (if (equal x nil) x)
    (<= (creature-hitpoints x) 0))

(defun creature-alive (x)
    (if (equal x nil) x)
    (not (creature-dead x)))

(defun creatures-dead ()
    (every #'creature-dead *creatures*))

(defstruct (player 
    (:include creature (x-location *player-x*) 
		       (y-location *player-y*) 
		       (tile "@")
		       (weapon '(0 0))
		       (ranged_weapon '(0 0)) 
		       (shield 0))) 
    name 
    experience 
    level
    energy
    satiation
    hydration)

(defstruct (rat 
		(:include creature 
		(type "rat") 
		(species 'rat) 
		(strength 5) 
		(dexterity 3) 
		(hitpoints (randval 15)) 
		(base_melee_skill 15) 
		(AC 0) 
		(attack_style "bites")
		(expval 5) 
		(tile "r"))))

(defstruct (wolf 
		(:include creature 
		(type "wolf") 
		(species 'wolf) 
		(strength 8) 
		(dexterity 5) 
		(hitpoints (randval 45)) 
		(base_melee_skill 25) 
		(AC 5) 
		(attack_style "bites")
		(expval 15) 
		(tile "w"))))

(defstruct (dog 
		(:include creature 
		(type "dog") 
		(species 'dog) 
		(strength 9) 
		(dexterity 4) 
		(hitpoints (randval 75)) 
		(base_melee_skill 20) 
		(AC 10) 
		(attack_style "bites")
		(expval 25) 
		(tile "d"))))



(defstruct (cyborg 
		(:include creature 
		(type "cyborg") 
		(species 'human) 
		(strength 10) 
		(dexterity 5) 
		(hitpoints (randval 105)) 
		(base_melee_skill 40) 
		(AC 50) 
		(attack_style "hits")
		(expval 100) 
		(tile "c"))))

(defstruct (huckleberry 
		(:include dog 
		(type "Huckleberry")
		(hitpoints 200)
		(strength 15) 
		(dexterity 15)
		(base_melee_skill 50) 
		(AC 75)
		(expval 1000) 
		(unique T)
		(tile "H"))))

(defstruct (mining_machine 
		(:include creature 
		(type "mining machine") 
		(species 'machine) 
		(strength 1)
		(dexterity 1)
		(base_melee_skill 1)
		(AC 1)
		(hitpoints 200) 
		(expval 10) 
		(tile "m"))))

;;(defstruct object x-location y-location group type tile effect)

(defmethod creature-hit (m x type)
    (cond ((or (< (randval 100) (hit_percentage *player* m)) (equal type "fire"))
	      (decf (creature-hitpoints m) x)
	      (if (creature-dead m)
	          (progn (incf (player-experience *player*) (creature-expval m)) 
			 (message (concatenate 'string "You hit " (unique_the m) " " (creature-type m) " and kill it."))
			 (setf *creatures* (remove m *creatures*))
			 (push m *killed-creatures*)
			 (drop_inventory m)
		  )
	          (message (concatenate 'string "You hit " (unique_the m) " " (creature-type m) "."))
	      )
	  )
	  (t (message (concatenate 'string "You miss " (unique_the m) " " (creature-type m) ".")) (trash_talk m))
    )
)

(defun unique_the (m)
    (when (= 1 1) "the")
)

(defun drop_inventory (m)
    (when (creature-inventory m)
	(mapcar (lambda (z) (progn (setf (object-x-location z) (creature-x-location m))
				   (setf (object-y-location z) (creature-y-location m))
				   (push z *objects*)
			    )
		)
		(creature-inventory m)
	)
    )
)

(defun is_player_position (x y)
    (and (equal x *player-x*) (equal y *player-y*)))

(defun is_object_position (x y) 
        (get_object_at_position x y *objects*)
)

(defun is_creature_position (x y) 
    (or
	(is_player_position x y)
	(some (lambda (z) (and (equal x (creature-x-location z)) (equal y (creature-y-location z)))) *creatures*) 
    )
)

(defun get_object_at_position (x y z) 
    (cond ((not z) z) 
          (
	      (and 
	  	(equal x (object-x-location (car z))) 
		(equal y (object-y-location (car z))) 
	      )
	      (car z)
	  ) 
          (t 
	      (get_object_at_position x y (cdr z))
	  )
    )
)

(defun get_creature_at_position (x y z) 
    (cond ((not z) z) 
          (
	      (and 
	  	(equal x (creature-x-location (car z))) 
		(equal y (creature-y-location (car z))) 
	      )
	      (car z)
	  ) 
          (t 
	      (get_creature_at_position x y (cdr z))
	  )
    )
)
 
(defun is_accessible (x y) 
    (and
        (not (is_creature_position x y))
        (is_floor x y)
	(>= x 0)
	(>= y 0)
	(< x *level-width*)
	(< y *level-height*)
    )
)

(defun adjacent (a b x y) (and (<= (abs (- a x)) 1) (<= (abs (- b y)) 1)))
