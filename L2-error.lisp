(defun penalty ()
    (decf (player-energy *player*) (randval (player-level *player*)))
)

