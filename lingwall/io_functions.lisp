(defparameter *test* nil)

;;input/output functions
(defun read_input () (ext:with-keyboard
    (loop :for char = (read-char ext:*keyboard-input*)
	:for key = (or (ext:char-key char) (character char))
	:return (last (list char key)))))

(defun read_keyboard () 
  (let ((ri (read_input))) 
    (cond ((equal ri '(#\:)) (command_mode)) 
	  ((equal ri '(#\;)) (language_mode))
	  ((equal ri '(#\i)) (show_inventory) (read_keyboard))
	  ((equal ri '(#\>)) (if (is_stair *player-x* *player-y*) (new_level) (read_keyboard)))
	  ((equal ri '(#\g)) (if (is_object_position *player-x* *player-y*) (pick_up_object) (read_keyboard)))
	  ((movement_key ri) (move_player ri))
          (t (read_keyboard))
    )
  )
)

(defun reinput ()
    (draw_world)
    (read_keyboard)
)

(defun command_mode () 
    (progn
        (princ ":")
	(case (read)
          (q (setf *session* "Quit"))
          (l (language_lesson) (read_keyboard))    
          (turn (message *turn*) (reinput))    
        )
    )
)

;; We have loaded trans and conj functions for the language defined in *L2* parameter.
(defun language_mode () 
    (progn
            (princ ";")
	    (let ((ri (read-line)))
	     ;; checking energy does not belong here
	     (cond 
		  ((equal ri (trans_drink_command)) (interface 'drinks) (randomize_language_parameters))
		  ((equal ri (trans_eat_command)) (interface 'foods) (randomize_language_parameters))
		  ((equal ri (trans_use_command)) (interface 'equipment) (randomize_language_parameters))
		  ((equal ri (trans_heal_command)) (if (>= (player-energy *player*) 5) 
						      (progn (heal) (randomize_language_parameters))    
						      (progn (message "You don't have enough energy.") 
							     (reinput))
						      )
		  )
		  ((equal ri (trans_teleport_command)) (if (>= (player-energy *player*) 5) 
						      (progn (teleport) (randomize_language_parameters))    
						      (progn (message "You don't have enough energy.") 
							     (reinput))
						      )
		   )
	           ((equal ri (trans_fire_command)) (if (>= (player-energy *player*) 10) 
			                                (progn (make_fire) (randomize_language_parameters)) 
							(progn (message "You don't have enough energy.") 
							       (reinput))
			                            )    
		   )
		   ((< (length ri) 2) (message "Command cancelled.") (reinput)) 
		   (t (decf (player-energy *player*) 1))
	    )
	   )
    )
)        

(defun interface (z)
    (cls)
    (princ z)
    (fresh-line)
  (let ((i 0))
    (loop for n from 0 to (1- (length *player-inventory*))
        do (when (and *player-inventory* (equal (object-group (nth n (reverse *player-inventory*))) z)) 
		  (fresh-line)
		  (princ (nth i *letter-list*))
	          (princ ") ")
	          (princ (object-type (nth n (reverse *player-inventory*))))
		  (incf i 1)
           )
    )
  )
    (fresh-line)
    (princ "Spacebar to exit")
    (fresh-line)
    (interface_input z)
    (draw_world)
)

(defun interface_input (z)
    (let ((ri (read_input)) (l nil))
	(let ((pos (position ri (mapcar #'eval *formatted-letter-list*) :test 'equal)))
	 (mapcar (lambda (x) (when (equal (object-group x) z) (push x l))) *player-inventory*)
	 (cond ((or (equal ri '(#\Space)) (equal ri '(#\Escape))) nil) 
	       ((and (numberp pos) 
		     (< pos (length l)) 
		)
		(eval (object-effect (nth pos l)))
		(setf *player-inventory* 
		    (remove (nth pos l) *player-inventory*))
               )
	       (t (interface_input z))
	 )
	)
     )
)

