(defun make_fire ()
    (setf *fuego* T)
    (fuego_attack *creatures*)
    (decf (player-energy *player*) 10)
)
 

(defun teleport () 
    (random_player_location)
    (decf (player-energy *player*) 5)
)

(defun heal () 
    (incf (player-hitpoints *player*) (floor (* (max_hitpoints *player*) 0.25)))
    (cure *player*)
    (decf (player-energy *player*) 5)
    (message "You repair yourself.")
)
    
(defun random_player_location ()
    (let ((x (random *level-width*)) (y (random *level-height*)))
	(cond ((is_accessible x y) (progn (setf *player-x* x) (setf *player-y* y)))
	      (t (random_player_location))
        )
    )
)

(defun is_fire (x y) 
    (and (is_floor x y) (> 5 (euclidean_distance *player-x* *player-y* x y))))

