(defun choose_language ()
    (cls)
    (princ "Please select a cyberlect in which to construct your lingwall.")
    (fresh-line)
    (princ "[1] French")
    (fresh-line)
    (princ "[2] Spanish")
    (fresh-line)
    (let ((ri (read_input)))
        (cond ((equal ri '(#\1)) (setf *L2* "French"))
              ((equal ri '(#\2)) (setf *L2* "Spanish"))
	      (t (progn (cls) (choose_language)))
        )
    )
    (princ "Loading...")
    (fresh-line)
)