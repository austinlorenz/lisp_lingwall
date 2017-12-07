(defun new_level ()
 (if (equal *level* 0) (message "You win!")
 (progn 
  (setf *creatures* nil)
  (setf *objects* nil)
  (decf *level* 1)
  (message "Loading...")
  (when (< *level* 20) (draw_world))
  (load "code\\map.fas")
  (setf *message-buffer* nil)
  (make_new_stairs)
  (make_rats (randval (+ *rat-maximum* (- 20 *level*))))
  (make_wolves (randval (+ *wolf-maximum* (- 20 *level*))))
  (make_cyborgs (randval (- 20 *level*)))
  (make_dogs (randval (- 20 *level*)))
  (make_miners (random 3))
  (when (equal *level* 0) (make_dogs 5) (push (make-huckleberry :x-location *stair-x* :y-location *stair-y*) *creatures*))
  (make_objects (randval 25))
  (random_player_location)
 )
 )
)
 
(defun make_new_stairs ()
    (let ((x (randval *level-width*)) (y (randval *level-height*)))
	(cond 
	    ((is_accessible x y) 
	    (setf *stair-x* x) (setf *stair-y* y))
	    (t (make_new_stairs))
	)
    )
)

(defun is_stair (x y)
  (and
	(equal x *stair-x*)
        (equal y *stair-y*)
  )
)

