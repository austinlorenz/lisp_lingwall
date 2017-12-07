(defparameter *level-height* 35)
(defparameter *level-width* 150)
(defparameter *visited-tiles* nil)
(defparameter *unvisited-tiles* nil)
(defparameter *walls* nil)
(defparameter *particle-x* 65)
(defparameter *particle-y* 17)
(defparameter *particle2-x* 60)
(defparameter *particle2-y* 17)
(defparameter *tile_string* "")
(defparameter *tile_list* ())
(defparameter *map-array* (make-array '(120 35)))
(load "rand_functions.lisp")
(load "display_functions.lisp")
(load "change_color.lisp")

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
	;;changed to 1x1
	(mapcar (lambda (z) (dig (first z) (second z))) 
		'((60 17)))
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
			(setf *tile_string* (concatenate 'string *tile_string* (get_tile x y)))
		)
		(push *tile_string* *tile_list*)
		(setf *tile_string* nil)
	)
	(print_list *tile_list*)
	(setf *tile_list* nil)
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
    (loop while (> (length *walls*) a) do (progn (loop repeat b do (draw_world) (new_rook_position *particle*)) 
						     (new_particle_position *particle2*)
						     ))
;;	(fresh-line)
;;	(princ a)
;;	(fresh-line)
;;	(princ b)
)
(init_tiles)
(change_text_color 'red)
(make_map 4000 150)
