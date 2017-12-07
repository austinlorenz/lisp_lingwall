(defparameter *buildings* nil)


(defstruct building
	   min-x
	   max-x
	   min-y
	   max-y
	   doors)


(defun common_x_coordinate (a b) 
  (or
    (and (>= (building-max-x a) (building-min-x b)) (<= (building-max-x a) (building-max-x b)))
    (and (>= (building-min-x a) (building-min-x b)) (<= (building-min-x a) (building-max-x b)))
    (and (<= (building-min-x a) (building-min-x b)) (>= (building-max-x a) (building-min-x b)))
    (and (>= (building-max-x b) (building-min-x a)) (<= (building-max-x b) (building-max-x a)))
    (and (>= (building-min-x b) (building-min-x a)) (<= (building-min-x b) (building-max-x a)))
    (and (<= (building-min-x b) (building-min-x a)) (>= (building-max-x b) (building-min-x a)))
  )
)

(defun common_y_coordinate (a b)
  (or
    (and (>= (building-max-y a) (building-min-y b)) (<= (building-max-y a) (building-max-y b)))
    (and (>= (building-min-y a) (building-min-y b)) (<= (building-min-y a) (building-max-x b)))
    (and (<= (building-min-y a) (building-min-y b)) (>= (building-max-y a) (building-min-y b)))
    (and (>= (building-max-y b) (building-min-y a)) (<= (building-max-y b) (building-max-y a)))
    (and (>= (building-min-y b) (building-min-y a)) (<= (building-min-y b) (building-max-y a)))
    (and (<= (building-min-y b) (building-min-y a)) (>= (building-max-y b) (building-min-y a)))

  )
)

(defun tunnel_maker (a b)
	(cond ((and (common_x_coordinate a b) (< (building-min-y a) (building-min-y b))) (v_tunnel_maker a b)) 
	      ((and (common_x_coordinate a b) (> (building-min-y a) (building-min-y b))) (v_tunnel_maker b a)) 
	      ((and (common_y_coordinate a b) (< (building-min-x a) (building-min-x b))) (h_tunnel_maker a b))
	      ((and (common_y_coordinate a b) (> (building-min-x a) (building-min-x b))) (h_tunnel_maker b a))
	      (t nil)
	)
)

;;Now we create a horizontal tunnel maker
(defun h_tunnel_maker (a b) 
  (let ((y (rand_interval (- (max (building-min-y a) (building-min-y b)) 2) (+ (min (building-max-y a) (building-max-y b)) 2))))
  (when (is_wall (building-max-x a) y)
    (progn
	(push (cons (building-max-x a) (cons y ())) (building-doors a))
	(push (cons (building-min-x b) (cons y ())) (building-doors b))
        (push (make-building :min-x (building-max-x a) 
			     :max-x (building-min-x b) 
			     :min-y (1- y) :max-y (1+ y) 
			     :doors (cons (cons (building-max-x a) (cons y ()))
				     (cons (cons (building-min-x b) (cons y ())) ())
				    )
				)
			   *buildings*)
    )
  )
  )
)



;;Now we create a vertical tunnel maker
(defun v_tunnel_maker (a b) 
  (let ((x (rand_interval (max (building-min-x a) (building-min-x b)) (min (building-max-x a) (building-max-x b)))))
    (progn
	(push (cons x (cons (building-max-y a) ())) (building-doors a))
	(push (cons x (cons (building-min-y b) ())) (building-doors b))
        (push (make-building :min-y (building-max-y a) 
	                     :max-y (building-min-y b) 
			     :min-x (1- x) :max-x (1+ x) 
			     :doors (cons (cons x (cons (building-max-y a) ()))
				     (cons (cons x (cons (building-min-y b) ())) ())
				    )
	      )
	 *buildings*)
    )
  )
)


(defun get_building_at_position (x y z) 
    (cond ((not z) z) 
          (
	      (and 
	  	    (and (<= x (building-max-x (car z))) 
		         (>= x (building-min-x (car z)))) 
	  	    (and (<= y (building-max-y (car z))) 
		         (>= y (building-min-y (car z)))) 
	      )
	      (car z)
	  ) 
          (t 
	      (get_building_at_position x y (cdr z))
	  )
    )
)

(defun is_wall (x y) 
    (let ((r (get_building_at_position x y *buildings*)))
    	(cond 
	   ((not r) r)
       	   (t 
		(is_wall_parameterized x y (building-min-x r) (building-max-x r) (building-min-y r) (building-max-y r) (building-doors r))
	   )
    	)
    )
)



(defun is_wall_parameterized (x y min-x max-x min-y max-y doors) 
   (and (not (some (lambda (z) (and (= (first z) x) (= (second z) y))) doors))
        (or 
	    (and (= x min-x) (>= y min-y) (<= y max-y))
	    (and (= y min-y) (>= x min-x) (<= x max-x))
	    (and (= x max-x) (>= y min-y) (<= y max-y))
	    (and (= y max-y) (>= x min-x) (<= x max-x))
	)
    )
    
)

(defun in_a_room (x y) 
    (let ((r (get_building_at_position x y *buildings*)))
    	(cond 
	   ((not r) r)
       	   (t (equal 1 1))
    	)
    )
)


(defun is_floor (x y) (and
    (not (is_wall x y))
    (in_a_room x y)))

(defun overlapping (a b)
	(when (and a b)
		(or  (and (common_x_coordinate a b) (common_y_coordinate a b))
	             (< (abs (- (building-max-x a) (building-min-x b))) 10)
		     (< (abs (- (building-min-x a) (building-max-x b))) 10)
	             (< (abs (- (building-max-y a) (building-min-y b))) 3)
		     (< (abs (- (building-min-y a) (building-max-y b))) 3)
		)
	)
)

;;To do:
;; 1.  make it so that we make tunnels only between rooms which have no overlapping floor tiles

;;(mapcar (lambda (x) (tunnel_maker (car z) x) *buildings*)

;; 2.  make it so that the max values of our rooms cannot exceed *level-height* or *level-width* 
;; 3.  make it so that every room within a short distance of another is connected by a tunnel to it with map
;; 4.  make it so that tunnels do not overlap, or better: when tunnels overlap, or walls in general, remove that section of wall.
;; x 5.  make rooms much smaller--large rooms are boring.
;; x 6.  Make it so that new buildings are added only if they are non-overlapping with other buildings in *buildings*.
;; 7.  Make it so that rooms are big enough to have floors
;; 8.  Make it so that rooms are generated in a zig zag pattern, then connected by tunnels.

;; So- push a random room onto the list.
;; Then initialize another random room.
;; If this room overlaps, throw it away and initialize another; repeat until we have two non-overlapping random rooms.
;; If the rooms have no common coordinate (not (tunnel_maker a b)), start over. 
;; Repeat until we have *room-number* rooms, all connected by tunnels.
;; Let's separate the room builder and the tunnel connector into two functions.
;; Room builder just makes a bunch or rooms which do not overlap.
;; Initial problems: makes rooms that go off edge of screen.  Makes rooms which do not overlap, but which are touching.  Prefer that not touching.
;; Makes ugly buildings with no space between walls.
;; Change overlap rules to require a square between buildings.

(defun make_rooms (n c) 
	(let (
	      (x (randval (- *level-width* 3))) 
	      (y (randval (- *level-height* 5)))
	     )
	     (let ((z (make-building :min-x (max x 6) 
		                :max-x (max (min (rand_interval x (1- *level-width*)) (+ x 10)) (+ x 3))
				:min-y (1- y)
				:max-y (max (rand_interval y (- *level-height* 3)) (+ y 3))))
		     )
	      
	      (cond ((= c 10000) nil)
		    ((= n 100) nil)
		    ((not *buildings*) (push z *buildings*) (make_rooms (1- n) 0))
	            ((not (some (lambda (x) (overlapping z x)) *buildings*)) 
		           (push z *buildings*)
			   (make_rooms (1- n) c))
		    (t (make_rooms n (1+ c)))
	      )
	)
     )
)

(defun connect_rooms (l) 
  (progn
    (when l
        (mapcar (lambda (x) (when (< (abs (- (building-min-x (car l)) (building-min-x x))) 50)
			     (tunnel_maker (car l) x))
		) 
	l)
	(connect_rooms (cdr l))
    )
  )
)

(make_rooms 15 0)
(connect_rooms *buildings*)

