(setf *random-state* (make-random-state t))

(defun randval (n)
(1+ (random (max 1 n))))

;;returns random integer from -n to n
(defun random-n (n)
    (- (random (1+ (* 2 n))) n))

;;Given two values a b, returns a random number between them.
(defun rand_interval (a b) (+ (random (1+ (- (max (1+ a) b) (min (1+ a) b)))) (min (1+ a) b)))

;;Returns a pair (x y) of coordinates of an accessible tile.
(defun rand_accessible ()
    (let ((x (random *level-width*)) (y (random *level-height*)))
	(cond 
	    ((is_accessible x y) (cons x (cons y ())))
	    (t (rand_accessible))
	)
    )
)


(defun rand_adjacent (a b)
    (let ((x (random-n 1)) (y (random-n 1)))
	(cons (+ x a) (cons (+ y b) ()))
    )
)
