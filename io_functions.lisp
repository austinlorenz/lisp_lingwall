(defparameter *commands* 0)
(defparameter *errors* 0)
(defparameter *cmd-start* 0)
(defparameter *cmd-end* 0)
(defparameter *total-command-time* 0)

;;input/output functions
(defun read_input () (ext:with-keyboard
    (loop :for char = (read-char ext:*keyboard-input*)
	:for key = (or (ext:char-key char) (character char))
	:return (last (list char key)))))

(defun read_keyboard () 
  (let ((ri (read_input))) 
    (cond ((equal ri '(#\:)) (command_mode)) 
	  ((equal ri '(#\;)) (language_mode))
	  ;;((equal ri '(#\t)) (setf *target-mode* T) (target) (reinput))
	  ((or (equal ri '(#\Space)) (equal ri '(#\Escape))) (setf *target-mode* nil) (reinput))
	  ((equal ri '(#\g)) (if (is_object_position *player-x* *player-y*) (pick_up_object) (read_keyboard)))
	  ((equal ri '(#\i)) (equipped) (read_keyboard))
	  ((equal ri '(#\f)) (fire_ranged_weapon))
	  ((equal ri '(#\>)) (if (is_stair *player-x* *player-y*) (new_level) (read_keyboard)))
	  ((movement_key ri) (move_player ri))
          (t (read_keyboard))
    )
  )
)

(defun reinput ()
    (draw_world)
    (read_keyboard)
)

(defun command_success ()
    (setf *command-end* (get-universal-time))
    (incf *total-command-time* (- *command-end* *command-start*))
    (randomize_language_parameters)
)

(defun command_failure ()
    (setf *command-end* (get-universal-time))
    (incf *total-command-time* (- *command-end* *command-start*))
    (incf *errors* 1) 
    (message "ERROR")
    (penalty)
)

(defun command_mode () 
    (progn
        (princ ":")
	(case (read)
          (q (setf *session* "Quit"))
          (l (language_lesson (player-level *player*)) (read_keyboard))    
	  (termless (setf *term-size* (decf *term-size* 5)) (reinput)) 
	  (termmore (setf *term-size* (incf *term-size* 5)) (reinput)) 
          (turn (message *turn*) (reinput))    
	  (otherwise (message "Command not recognized.") (reinput))
        )
    )
)

;; We have loaded trans and conj functions for the language defined in *L2* parameter.
(defun language_mode () 
    (progn
	    (incf *commands* 1)
            (princ ";")
	    (setf *command-start* (get-universal-time))
	    (let ((ri (read-line)))
	     ;; checking energy does not belong here
	     (cond 
		  ((equal ri (trans_drink_command)) (interface 'drinks) (command_success))
		  ((equal ri (trans_eat_command)) (interface 'foods) (command_success))
		  ((equal ri (trans_use_command)) (interface 'equipment) (command_success))
		  ((equal ri (trans_heal_command)) (if (>= (player-energy *player*) 5) 
						      (progn (heal) (command_success))    
						      (progn (message "You don't have enough energy.") 
							     (reinput))
						      )
		  )
		  ((equal ri (trans_teleport_command)) (if (>= (player-energy *player*) 5) 
						      (progn (teleport) (command_success))    
						      (progn (message "You don't have enough energy.") 
							     (reinput))
						      )
		   )
	           ((equal ri (trans_fire_command)) (if (>= (player-energy *player*) 10) 
			                                (progn (make_fire) (command_success)) 
							(progn (message "You don't have enough energy.") 
							       (reinput))
			                            )    
		   )
		   ((< (length ri) 2) (message "Command cancelled.") (decf *commands* 1) (reinput)) 
		   (t (message ri) (command_failure))
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
