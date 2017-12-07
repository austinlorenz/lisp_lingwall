;; initialization procedure
(defun intro_screen () (progn 
    (change_text_color 'blue)
    (print_ascii_wolf)
    (fresh-line)
    (princ *blank-half-line*)
    (fresh-line)
    (change_text_color 'red)
    (princ "Press any key to continue.") 
    (change_text_color 'cyan)
    (read_input) 
    (cls)))

