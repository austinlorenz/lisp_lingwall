(defun make_rooms (n) 
	(let ((x (randval *level-width*)) (y (randval *level-height*)))
	(cond 
	    ((equal n 0) n)
	    ((is_accessible x y) 
	    (progn
	        (setf *rooms* (cons (make-building :min-x x 
						   :max-x (+ x (+ (randval 20) 2))
						   :min-y y
						   :max-y (+ x (+ (randval 20) 2))
				    )
			*rooms*))
	        (make_rooms (1- n))
	    ))
	    (t (make_rooms n))
	)
	)
)

