(defparameter *formatted-letter-list* '( '(#\a) '(#\b) '(#\c) '(#\d) '(#\e) '(#\f) '(#\g) '(#\h) '(#\i) '(#\j) '(#\k) '(#\l) '(#\m) '(#\n) '(#\o) '(#\p) '(#\q) '(#\r) '(#\s) '(#\t) '(#\u) '(#\v) '(#\w) '(#\x) '(#\y) '(#\z) ) )

(defparameter *letter-list* '("a" "b" "c" "d" "e" "f" "g" "h" "i" "j" "k" "l" "m" "n" "o" "p" "q" "r" "s" "t" "u" "v" "w" "x" "y" "z"))

(defun show_inventory () (progn 
    (cls)
    (princ "Inventory")
    (loop for n from 0 to (1- (length *player-inventory*))
        do (when *player-inventory* 
		  (fresh-line)
		  (princ (nth n *letter-list*))
	          (princ ") ")
	          (princ (object-type (nth n *player-inventory*)))
           )
    )
    (fresh-line)
    (princ "Press spacebar to exit.")
    (fresh-line)
    (inventory_input)
    (draw_world)
    (fresh-line)
))

(defun inventory_input () 
  (let ((ri (read_input)))
    (cond ((or (equal ri '(#\Space)) (equal ri '(#\Escape))) nil) 
          (t (inventory_input))
    )
    
  )
)

