(defparameter *height* 30)
(defparameter *max-height* (1- *height*))
(defparameter *width* 30)
(defparameter *max-width* (1- *width*))
(defparameter *player-x* 14)
(defparameter *player-y* 14)
(defparameter *player-vision* 100)
(defparameter *blank* "        ")
(defparameter *tile_string* "")
(defparameter *message* *blank*)

(defun square (x) (* x x))

(defun cls () (format t "~A[H~@*~A[J" #\escape))
(defun clispcls () (screen:clear-window (screen:make-window))

(defun draw_world () 
    (progn 
        (loop for y below *height* do 
            (progn 
                (fresh-line) 
                (loop for x below *width* do 
                    (if (is_visible x y) (draw_tile x y)))))
        (fresh-line)))


(defun draw_tile (x y) 
    (cond ((is_sidebar x y) (display_status x y))
	  ((and (equal x *player-x*) (equal y *player-y*)) (princ " @ ")) 
          ((is_wall x y) (princ " # "))
           (t (princ " . "))))

(defun is_sidebar (x y) (< x 6))

(defun display_status (x y) (if (equal x 0) 
    (cond ((equal y 10) (princ "Human   "))
	  ((equal y 11) (princ "Level 0 "))
	  ((equal y 12) (princ "Gold: 0 "))
	  ((equal y 14) (princ "STR 10  "))
	  ((equal y 15) (princ "DUR 8   "))
          (t (princ *blank*)))))


;; (format t "~c[34mHuman~c[0m~%" #\ESC #\ESC)

(defun move_player (direction) 
    (cond ((and (equal direction '(:RIGHT)) (is_accessible (1+ *player-x*) *player-y*)) (setf *player-x* (1+ *player-x*))) 
          ((and (equal direction '(:LEFT)) (is_accessible (1- *player-x*) *player-y*)) (setf *player-x* (1- *player-x*))) 
	  ((and (equal direction '(:DOWN)) (is_accessible *player-x* (1+ *player-y*))) (setf *player-y* (1+ *player-y*))) 
	  ((and (equal direction '(:UP)) (is_accessible *player-x* (1- *player-y*))) (setf *player-y* (1- *player-y*))) 
          (t nil)))

(defun distance_from_player (x y) 
    (floor 
        (sqrt 
            (+ 
                (square (- *player-x* x)) 
		(square (- *player-y* y))))))

(defun is_visible (x y) (< (distance_from_player x y) *player-vision*))

(defun read_input () (EXT:WITH-KEYBOARD
 (LOOP :for char = (READ-CHAR EXT:*KEYBOARD-INPUT*)
   :for key = (OR (EXT:CHAR-KEY char) (CHARACTER char))
   :return (last (LIST char key)))))

(defun move_player_with_keyboard () (move_player (read_input)))

(defun is_wall (x y) 
    (or 
        (or 
            (and (equal y 6) (not (equal x 20))) 
            (equal x 6)) 
        (or 
            (equal x *max-height*) 
            (equal y *max-height*))))

(defun is_accessible (x y) (and (not (is_wall x y)) 
                                (and (< x *height*)
				     (< y *height*))))

(defun pass_message () (princ *message*))

(defun game_loop (count) (if (equal count 0) (progn 
    (cls)
    (setf *message* "The End.")
    (pass_message)
    (draw_world))
    (progn 
    (cls)
    (pass_message)
    (draw_world)
    (move_player_with_keyboard)
    (game_loop (- count 1)))))
    
