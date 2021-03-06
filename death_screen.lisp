(defparameter *cause-of-death* "is dead")

(defun death_screen ()
	(cls)
	(princ (player-name *player*))
	(princ " the level ")
	(princ (player-level *player*))
	(princ " ")
	(princ (player-species *player*))
	(princ " ")
	(princ *cause-of-death*)
        (princ ".  ")
	(terpri) (terpri)
	(princ "He made it to dungeon level ")
	(princ *level*)
	(princ ".  He killed ")
	(princ (length *killed-creatures*))
	(princ " creatures and issued ")
	(princ *commands*)
	(princ " commands in ")
	(princ *L2*)
	(princ ", with ")
	(princ *errors*)
	(princ " errors, for a total accuracy of ")
	(princ (total_accuracy)) 
	(princ " percent.")
	(terpri) (terpri)
	(princ "He played for ")
	(princ (minutes))
	(princ " minutes and ")
	(princ *turn*)
	(princ " turns, issuing ")
	(princ (float (/ *commands* (minutes))))
	(princ " commands per minute and ")
	(princ (float (/ *commands* *turn*)))
	(princ " commands per turn.  ")
	(princ "His average command time was ")
        (princ (average_command_time))
        (princ " seconds.")
	(princ "  He spent ")
	(princ (command_percent_time))
	(princ " percent of his time issuing commands in ")
	(princ *L2*)
	(princ ".")
	(terpri) (terpri)
)

(defun total_time ()
	(- *end-time* *start-time*)
)

(defun minutes ()
	(ceiling (float (/ (total_time) 60)))
)

(defun total_accuracy ()
	(if (= *commands* 0) 0 (float (* 100 (/ (- *commands* *errors*) *commands*))))
)

(defun average_command_time ()
	(if (= *commands* 0) 0 (float (/ *total-command-time* *commands*)))
)

(defun command_percent_time ()
	(float (* 100 (/ *total-command-time* (total_time))))
)
