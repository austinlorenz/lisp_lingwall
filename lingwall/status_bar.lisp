(defun get_status (y)  
    (cond ((equal y 5) (player-name *player*))
	  ((equal y 6) (player-species *player*))
	  ((equal y 7) *L2*)
	  ((equal y 19) (status_line "DL " *level*))
	  ((equal y 17) (status_line "Level " (player-level *player*)))
	  ((equal y 16) (status_line "EXP " (player-experience *player*)))
	  ((equal y 15) (status_line "AC " (player-AC *player*)))
	  ((equal y 14) (status_line "STR " (player-strength *player*)))
	  ((equal y 13) (status_line "DEX " (player-dexterity *player*)))
	  ((equal y 12) (status_line "CON " (player-constitution *player*)))
	  ((equal y 11) (concatenate 'string (status_line "HP " (player-hitpoints *player*)) (status_line "/" (max_hitpoints *player*))))
	  ((equal y 10) (concatenate 'string (status_line "ENG " (player-energy *player*)) (status_line "/" (max_energy *player*))))
          (t *blank-status*)
    )
)

(defun status_line (string number)
    (concatenate 'string string (write-to-string number))
)
