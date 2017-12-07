(defparameter *message-buffer* nil)
(defparameter *messages* nil)

(defun message (m)
	(push m *message-buffer*) 
)

(defun get_messages ()
	(setf *messages* nil)
	(loop for i to 4 do 
		(push (nth i *message-buffer*) *messages*)
	)
)

(defun disp_messages ()
	(get_messages)
	(mapcar (lambda (x) (when x (progn (princ x) (princ " ")))) *messages*)
)

(defun hp_warning ()
	(when (< (player-hitpoints *player*) (* (max_hitpoints *player*) 0.20))
	      (message "**LOW HITPOINT WARNING**")
	)
)
