(defun is_fire (x y) 
    (and (is_floor x y) (< 10 (euclidean_distance *player-x* *player-y* x y))))

(defun make_fire ()
    (setf *fuego* T))
    
