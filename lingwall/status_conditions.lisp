(defun status_effect (s)
	(cond ((and (equal s 'poisoned) (= (random 5) 0)) (decf (player-hitpoints *player*) 1))
	)
)

(defun update_status_effects ()
	(mapcar #'status_effect (player-status_effects *player*))
)

(defun poison (creature)
	(cure creature)
	(push 'poisoned (creature-status_effects creature))
)

(defun cure (creature)
	(setf (creature-status_effects creature) (remove 'poisoned (creature-status_effects creature)))
)
