(defun update_hydration ()
	(decf (player-hydration *player*) 1)
	(when (> (player-hydration *player*) 2500) (setf (player-hydration *player*) 2500))
	(when (= (player-hydration *player*) 2500) (message "You are not thirsty."))
	(when (> (player-hydration *player*) 200) (clear_hydration_status *player*))
	(when (= (player-hydration *player*) 200) (hydration_status 'thirsty *player*))
	(when (= (player-hydration *player*) 100) (hydration_status 'dehydrated *player*))
	(when (= (player-hydration *player*) 50) (hydration_status 'dehydrated! *player*))
	(when (< (player-hydration *player*) 1) (message "You die of thirst.") 
						(setf (player-hitpoints *player*) 0)
						(setf *cause-of-death* "died of thirst"))
)

(defun hydration_status (s creature)
	(clear_hydration_status creature)
	(push s (creature-status_effects creature))
)

(defun clear_hydration_status (creature)
	(setf (creature-status_effects creature) (remove 'thirsty (creature-status_effects creature)))
	(setf (creature-status_effects creature) (remove 'dehydrated (creature-status_effects creature)))
	(setf (creature-status_effects creature) (remove 'dehydrated! (creature-status_effects creature)))
)
