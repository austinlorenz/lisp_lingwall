(defparameter *level-height* 35)
(defparameter *level-width* 120)
(defparameter *visited-tiles* nil)
(defparameter *unvisited-tiles* nil)
(defparameter *walls* nil)
(defparameter *particle-x* 65)
(defparameter *particle-y* 17)
(defparameter *particle2-x* 60)
(defparameter *particle2-y* 17)
(load "rand_functions.lisp")

(defstruct wall
	   x
	   y)

(defstruct particle
	   x
	   y)

(defparameter *particle* (make-particle :x 65 :y 17))
(defparameter *particle2* (make-particle :x 60 :y 17))

(defun get_tile (x y)
	(cond 
	    ((is_particle_position x y) "p")
	    ((is_particle2_position x y) "q")
	    ((is_wall x y) "#")
	    (t ".")
	)
)

(defun init_tiles ()
	(loop for y below *level-height* do
		(loop for x below *level-width* do
			(push (make-wall :x x :y y) *walls*) 
		)
	)	
	;;Dig out an initial 4x4 square
	(mapcar (lambda (z) (dig (first z) (second z))) 
		'((58 15) (58 16) (58 17) (58 18) (58 19)
		  (59 15) (59 16) (59 17) (59 18) (59 19) 
		  (60 15) (60 16) (60 17) (60 18) (60 19) 
		  (61 15) (61 16) (61 17) (61 18) (61 19) 
		  (62 15) (62 16) (62 17) (62 18) (62 19)))
)

(defun dig (x y)
	(delete (get_wall_at_position x y *walls*) *walls*)
)

(defun is_wall (x y)
	(some (lambda (z) (and (equal x (wall-x z)) (equal y (wall-y z)))) *walls*)
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

(defun draw_world ()
	(loop for y below *level-height* do
		(loop for x below *level-width* do
			(princ (get_tile x y))
		)
		(fresh-line)
	)
)

(defun get_wall_at_position (x y z)
	(cond 
		((not z) z)
		((and
			(equal x (wall-x (car z)))
			(equal y (wall-y (car z)))
		 )
		 (car z))
		(t (get_wall_at_position x y (cdr z)))
	)
)

(defun random_location (p) 
	(let ((x (randval (- *level-width* 2))) (y (randval (- *level-height* 2))))
		(if (is_floor x y) (random_location p) (progn (setf (particle-x p) x) (setf (particle-y p) y)))
	)
)

(defun return_to_default (p)
	(setf (particle-x p) *particle-x*)
	(setf (particle-y p) *particle-y*)
)

(defun new_particle_position (p)
	(let ((a (+ (random-n 1) (particle-x p))) (b (+ (random-n 1) (particle-y p))))
	    (cond 
		((or (<= (1- *level-height*) b) (>= 0 b)) (new_particle_position p))
		((or (<= (1- *level-width*) a) (>= 0 a)) (new_particle_position p))
		((is_wall a b) (dig (particle-x p) (particle-y p))) 
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

(init_tiles)
