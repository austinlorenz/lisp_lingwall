(defparameter *particle-x* 65)
(defparameter *particle-y* 17)
(defparameter *particle2-x* 60)
(defparameter *particle2-y* 17)
(defparameter *tile_string* "")
(defparameter *tile_list* ())
(defparameter *map-array* (make-array '(125 40)))
(defparameter *dig* 0)

(defstruct wall
	   x
	   y
	   seen)

(defstruct particle
	   x
	   y)

(defparameter *particle* (make-particle :x 65 :y 17))
(defparameter *particle2* (make-particle :x 60 :y 17))

(defun init_tiles ()
	(loop for y below *level-height* do
		(loop for x below *level-width* do
			(setf (aref *map-array* x y) (make-wall :x x :y y)) 
		)
	)	
	;;Dig out an initial square
	(mapcar (lambda (z) (dig (first z) (second z))) 
		'((60 17)))
)

(defun dig (x y)
	(setf (aref *map-array* x y) nil)
	(incf *dig* 1)
)

(defun is_wall (x y)
     (when (and (>= y 0) (< y *level-height*) (>= x 0) (< x *level-width*))
	(aref *map-array* x y)
     )
)

(defun is_floor (x y)
	(not (is_wall x y))
)

(defun is_particle_position (x y)
	(and (equal (particle-x *particle*) x) (equal (particle-y *particle*) y))
)

(defun is_particle2_position (x y)
	(and (equal (particle-x *particle2*) x) (equal (particle-y *particle2*) y))
)


(defun random_location (p) 
	(let ((x (randval (- *level-width* 2))) (y (randval (- *level-height* 2))))
		(if (is_floor x y) (random_location p) (progn (setf (particle-x p) x) (setf (particle-y p) y)))
	)
)

(defun return_to_default (p)
	(setf (particle-x p) *particle2-x*)
	(setf (particle-y p) *particle2-y*)
)

(defun new_particle_position (p)
	(let ((a (+ (random-n 1) (particle-x p))) (b (+ (random-n 1) (particle-y p))))
	    (cond 
		((or (<= (1- *level-height*) b) (>= 0 b)) (new_particle_position p))
		((or (<= (1- *level-width*) a) (>= 0 a)) (new_particle_position p))
		((is_wall a b) (dig a b) (return_to_default p)) 
		(t (setf (particle-x p) a) (setf (particle-y p) b))
	    )
	)	
)


(defun new_rook_position (p)
	(let ((d (randval 4)))
		(cond ((= d 1) (try_position p 0 1))
		      ((= d 2) (try_position p 1 0))
		      ((= d 3) (try_position p 0 -1))
		      ((= d 4) (try_position p -1 0))
		)
	)
)


(defun try_position (p x y)
	(let ((a (+ x (particle-x p))) (b (+ y (particle-y p))))
	    (cond 
		((or (<= (1- *level-height*) b) (>= 0 b)) (new_rook_position p))
		((or (<= (1- *level-width*) a) (>= 0 a)) (new_rook_position p))
		((is_floor a b) (dig (particle-x p) (particle-y p)) (random_location p)) 
		(t (setf (particle-x p) a) (setf (particle-y p) b))
	    )
	)	
)


(defun make_map (a b)
	(loop while (> a *dig*) do (progn (loop repeat b do (new_rook_position *particle*)) 
						     (new_particle_position *particle2*)
						     ))
)

(when (< *level* 21)
    (init_tiles)
    (make_map (rand_interval 2000 3000) (rand_interval 5 500))
)
