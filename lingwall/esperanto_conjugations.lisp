(defun trans (x)
    (cond ((equal x "to burn") "bruli")
	  ((equal x "to eat") "mangxi")
	  ((equal x "to drink") "trinki")
	  ((equal x "to go") "iri")
    )
)

(defun conj (verb)
    (let ((stem (subseq verb 0 (1- (length verb)))))
	(cond ((equal *pronoun* "I") (concatenate 'string "mi " stem "as"))
	      ((equal *pronoun* "he") (concatenate 'string "li " stem "as"))
	      ((equal *pronoun* "she") (concatenate 'string "sxi " stem "as"))
	      ((equal *pronoun* "it") (concatenate 'string "gxi " stem "as"))
	      ((equal *pronoun* "we") (concatenate 'string "ni " stem "as"))
	      ((equal *pronoun* "you") (concatenate 'string "vi " stem "as"))
	      ((equal *pronoun* "they") (concatenate 'string "ili " stem "as"))
	)
    )
)
