(defun verb_trans (x)
    (cond 
	  ((equal x "to go") "ir")
	  ((equal x "to have") "tener")
	  ((equal x "to eat") "comer")
	  ((equal x "to drink") "beber")
	  ((equal x "to burn") "quemar")
	  ((equal x "to make") "hacer")
	  ((equal x "to heal") "sanar")
	  ((equal x "to be") "estar")
	  ((equal x "to use") "usar")
	  ((equal x "to eat dinner") "cenar")
	  ((equal x "to eat lunch") "almorzar")
	  ((equal x "to eat breakfast") "desayunar")
    )
)

(defun article_trans (word)
    (cond 
      ((equal *gender* "m")
	(cond
		((equal word "the") "el")
		((equal word "a") "un")
		((equal word "this") "este")
		((equal word "that") "ese")
	)
      )
      ((equal *gender* "f")
	(cond
		((equal word "the") "la")
		((equal word "a") "una")
		((equal word "this") "esta")
		((equal word "that") "esa")
	)
    )
   )
)

(defun noun_trans (word)
	(cond
		((equal word "fire") "fuego")
		((equal word "bonfire") "fogata")
	)	
)
