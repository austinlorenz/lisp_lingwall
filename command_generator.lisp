(defparameter *command-list* nil)
(defparameter *gender* "m")
(defparameter *plural* nil)
(defparameter *article* "a")
(defparameter *articles* '("a"))

(defstruct command
	   name
	   verb
	   auxiliary
	   tense
	   mood
	   m_noun
	   f_noun
	   n_noun
	   plurality
	   sing_articles
	   plural_articles
	   effect
	   cost)

(defparameter *make-fire* (make-command :name "fire bomb" 
				        :verb "to make"
					:sing_articles '("a" "the" "this" "that")
					:m_noun "fire" 
					:f_noun "fireball"
			  )
)

(defparameter *eat* (make-command :name "eat" 
				        :verb "to eat"
			  )
)

(defparameter *drink* (make-command :name "drink" 
		                    :verb "to drink"
			  )
)

(defparameter *use* (make-command   :name "use" 
		                    :verb "to use"
			  )
)

(defparameter *go* (make-command :name "teleport" 
		                    :verb "to go"
			  )
)

(defparameter *heal* (make-command :name "heal" 
		                    :verb "to be"
			  )
)

(defun make_fire_command ()
    (concatenate 'string 
		*L1-pronoun* 
		" " 
		(eng-conj (command-verb *make-fire*)) 
		" " 
		(eng-plural *article*) 
		" " 
		(eng-plural (if (equal *gender* "m") (command-m_noun *make-fire*) (command-f_noun *make-fire*)))
    )
)

(defun eat_command ()
    (concatenate 'string 
		*L1-pronoun* 
		" " 
		(eng-conj (command-verb *eat*)) 
    )
)

(defun drink_command ()
    (concatenate 'string 
		*L1-pronoun* 
		" " 
		(eng-conj (command-verb *drink*)) 
    )
)


(defun use_command ()
    (concatenate 'string 
		*L1-pronoun* 
		" " 
		(eng-conj (command-verb *use*)) 
    )
)

(defun heal_command ()
    (concatenate 'string 
		*L1-pronoun* 
		" " 
		(eng-conj (command-verb *heal*)) 
	        " well"
    )
)

(defun teleport_command ()
    (concatenate 'string 
		*L1-pronoun* 
		" " 
		(eng-conj (command-verb *go*)) 
    )
)

(defun trans_fire_command ()
    (concatenate 'string
		*L2-pronoun*
		" "
		(conj (verb_trans (command-verb *make-fire*)))
		" "
		(plural (article_trans *article*))
		" "
		(plural (noun_trans (if (equal *gender* "m") (command-m_noun *make-fire*) (command-f_noun *make-fire*))))
    )
)
		
(defun trans_eat_command ()
    (concatenate 'string
		*L2-pronoun*
		" "
		(conj (verb_trans (command-verb *eat*)))
    )
)

(defun trans_heal_command ()
    (concatenate 'string
		*L2-pronoun*
		" "
		(conj (verb_trans (command-verb *heal*)))
	        " "
		(adverb_trans "well")
    )
)

(defun trans_drink_command ()
    (concatenate 'string
		*L2-pronoun*
		" "
		(conj (verb_trans (command-verb *drink*)))
    )
)

(defun trans_use_command ()
    (concatenate 'string
		*L2-pronoun*
		" "
		(conj (verb_trans (command-verb *use*)))
    )
)

(defun trans_teleport_command ()
    (concatenate 'string
		*L2-pronoun*
		" "
		(conj (verb_trans (command-verb *go*)))
    )
)

(defun random_gender ()
	(let ((r (random 2)))
		(cond
			((equal r 0) (setf *gender* "f"))
			((equal r 1) (setf *gender* "m"))
		)
	)
)

(defun random_article ()
	(setf *article* (nth (random (length *articles*)) *articles*))
)

(defun random_plural ()
	(if (= (random 2) 0) (setf *plural* T) (setf *plural* nil))
)

(defun eng-plural (word)
    (if *plural*	
       (cond
		((equal "a" word) "some")
		((equal "the" word) "the")
		((equal "this" word) "these")
		((equal "that" word) "those")
		(t (concatenate 'string word "s"))
 	)
	word)
)
