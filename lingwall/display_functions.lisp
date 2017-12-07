(defparameter *tile-list* nil)
;; (defparameter *command-list* '("to go" "ascend" "descend" "to burn" "to eat" "to drink"))
(defun get_command (y)
    (cond ((equal y 5) (teleport_command))
	  ((equal y 6) (make_fire_command))
	  ((equal y 7) (eat_command))
	  ((equal y 8) (drink_command))
	  ((equal y 9) (heal_command))
	  ((equal y 10) (use_command))
	  (t "")
    )
)

(defun square (x) (* x x))
(defun euclidean_distance (a b x y) 
	(floor (sqrt (+ (square (- x a)) (square (- b y)))))) 

;;clears screen
(defun cls () (format t "~A[H~@*~A[J" #\escape))
;;(defun cls () 
;;(screen:with-window (screen:clear-window screen:*window*))
;;(screen:clear-window-to-eot (screen:make-window)))

;;prints each element of a list on a new line
(defun print_list (x) (maplist #'(lambda (x) (progn (format t (car x)) (fresh-line))) x))

;;Draws status bar and world in terminal 1
(defun draw_world () 
    (progn 
        (loop for y below *level-height* do 
            (progn 
                (fresh-line) 
                ;;(loop for x below *level-width* do 
	        (loop for x from (max 0 (- *player-x* *player-vision*)) to (min (+ *player-x* *player-vision*) 119) do
                    (if 
			(in_term_1 x y) 
			(setf *tile-string* (concatenate 'string *tile-string* (get_tile x y)))
			)
		)
	    )
	    (setf *tile-list* (cons *tile-string* *tile-list*))
	    (setf *tile-string* ())
	    )
	(cls)
	(disp_messages)
	;;(print_list *tile-list*)
	(fresh-line)
	(loop for z below (length *tile-list*) do
	   ;;(format t "~0t~a ~12t~a ~80t~a ~103t~a~%" (get_status z) (nth z *tile-list*) (get_command z) (command_border z))
	   (format t "~0t~a ~15t~a ~83t~c[34m~a ~c[0m~%" (get_status z) (nth z *tile-list*) #\ESC (get_command z) #\ESC)
	)
	(hp_warning)
	(fresh-line)
	(if (player-status_effects *player*) (princ (player-status_effects *player*)) (princ ""))
	(fresh-line)
	(princ *L2-pronoun*)
	(fresh-line)
	(setf *tile-list* nil)))


(defun in_term_1 (x y) 
    (or 
	(is_sidebar x y)
	(and 
	    (< 
	        (abs (- x *player-x*)) 
	        *player-vision*
	    ) 
	    (< 
	        (abs (- y *player-y*)) 
	        *player-vision*
	    )
        )
    )
)

(defun get_tile (x y) 
    (when (and (is_wall x y) (in_line_of_sight *player-x* *player-y* x y)) (setf (wall-seen (aref *map-array* x y)) T))
    (cond ((is_player_position x y) *player-tile*) 
          ((and (is_wall x y) (wall-seen (aref *map-array* x y))) *wall-tile-1*)
	  ((not (in_line_of_sight *player-x* *player-y* x y)) " ")
	  ((and (get_creature_at_position x y *creatures*) (creature-alive (get_creature_at_position x y *creatures*)))
	  	(creature-tile (get_creature_at_position x y *creatures*))) 
	  ((or (is_line x y) (and *fuego* (is_fire x y))) "*")
	  ((is_object_position x y) (object-tile (get_object_at_position x y *objects*)))
	  ((is_stair x y) ">")
	  ((is_floor x y) *floor-tile*)
           (t *blank-tile*)
    )
)

(defun is_sidebar (x y) (< x 6))

(defun print_player_position () (progn
    (princ "x: ")
    (princ *player-x*)
    (princ " y: ")
    (princ *player-y*)
))

(defun print-pronoun () (princ *pronoun*))
 
(defun pass_message () (princ *message*))

;; (format t "~c[34mHuman~c[0m~%" #\ESC #\ESC)
