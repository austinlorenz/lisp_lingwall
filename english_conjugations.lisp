;;General function to send verbs to appropriate conjugation function.
(defun eng-conj (x)
    (cond ((equal x "to go") (go-conj))
	  ((equal x "to have") (have-conj))
	  ((equal x "to be") (be-conj))
	  (t (reg-conj (subseq x 3 (length x))))
    )
)


(defun reg-conj (stem)
	(cond ((or (equal *L1-pronoun* "she") (equal *L1-pronoun* "he")) (concatenate 'string stem "s"))
	      (t stem)
	)
)

(defun be-conj ()
	(cond ((equal *L1-pronoun* "I") "am")
	      ((or (equal *L1-pronoun* "he") (equal *L1-pronoun* "she")) "is")
	      (t "are")
	)
)

(defun go-conj ()
	(cond ((or (equal *L1-pronoun* "he") (equal *L1-pronoun* "she")) "goes")
	      (t "go")
	)
)

(defun have-conj ()
	(cond ((or (equal *L1-pronoun* "he") (equal *L1-pronoun* "she")) "has")
	      (t "have")
	)
)
