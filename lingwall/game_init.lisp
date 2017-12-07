;; initialization procedure
(defun intro_screen () (progn 
    ;;(change_text_color 'purple)
    (print_ascii_wolf)
    (fresh-line)
    (draw_title)
    (princ *blank-half-line*)
    (fresh-line)
    ;;(change_text_color 'red)
    (princ "Press any key to continue.") 
    ;;(change_text_color 'blue)
    (read_input) 
    ;;(change_bg_color 'white)
    (cls)))
