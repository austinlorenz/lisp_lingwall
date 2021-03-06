(defun change_text_color (color) 
    (cond 
          ((equal color 'grey) (format t "~c[30m" #\ESC #\ESC))
          ((equal color 'red) (format t "~c[31m" #\ESC #\ESC))
          ((equal color 'green) (format t "~c[32m" #\ESC #\ESC))
          ((equal color 'yellow) (format t "~c[33m" #\ESC #\ESC))
          ((equal color 'blue) (format t "~c[34m" #\ESC #\ESC))
          ((equal color 'purple) (format t "~c[35m" #\ESC #\ESC))
          ((equal color 'cyan) (format t "~c[36m" #\ESC #\ESC))
          ((equal color 'white) (format t "~c[37m" #\ESC #\ESC))
          ((equal color 'default) (format t "~c[39m" #\ESC #\ESC))
	  (t (format t "~c[37m" #\ESC #\ESC))))

(defun change_bg_color (color) 
    (cond 
          ((equal color 'grey) (format t "~c[40m" #\ESC #\ESC))
          ((equal color 'red) (format t "~c[41m" #\ESC #\ESC))
          ((equal color 'green) (format t "~c[42m" #\ESC #\ESC))
          ((equal color 'yellow) (format t "~c[43m" #\ESC #\ESC))
          ((equal color 'blue) (format t "~c[44m" #\ESC #\ESC))
          ((equal color 'purple) (format t "~c[45m" #\ESC #\ESC))
          ((equal color 'cyan) (format t "~c[46m" #\ESC #\ESC))
          ((equal color 'white) (format t "~c[47m" #\ESC #\ESC))
          ((equal color 'default) (format t "~c[49m" #\ESC #\ESC))
	  (t (format t "~c[47m" #\ESC #\ESC))))
