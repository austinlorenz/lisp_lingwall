(defun verb_trans (x)
    (cond 
	  ((equal x "to go") "aller")
	  ((equal x "to have") "avoir")
	  ((equal x "to eat") "manger")
	  ((equal x "to drink") "boire")
	  ((equal x "to burn") "bruler")
	  ((equal x "to make") "faire")
	  ((equal x "to be") "etre")
	  ((equal x "to use") "utiliser")
    )
)

(defun adverb_trans (word)
   (cond 
	  ((equal word "well") "en bonne sante")
    )
)

(defun article_trans (word)
    (cond 
      ((equal *gender* "m")
	(cond
		((equal word "the") "le")
		((equal word "a") "un")
		((equal word "this") "ce")
		((equal word "that") "ce")
	)
      )
      ((equal *gender* "f")
	(cond
		((equal word "the") "la")
		((equal word "a") "une")
		((equal word "this") "cette")
		((equal word "that") "cette")
	)
    )
   )
)

(defun noun_trans (word)
	(cond
		((equal word "fire") "feu")
		((equal word "fireball") "boule de feu")
	)	
)

(defun plural (word)
    (if *plural*	
       (cond
		((equal "un" word) "des")
		((equal "une" word) "des")
		((equal "ce" word) "ces")
		((equal "cette" word) "ces")
		((equal "feu" word) "feux")
		((equal "boule de feu" word) "boules de feu")
		(t (concatenate 'string word "s"))
 	)
	word)
)


