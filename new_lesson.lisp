(defun randomize_language_parameters ()
	(set-pronoun)
	(when (>= (player-level *player*) 1) (random_gender))
	(random_article)
	(when (>= (player-level *player*) 9) (random_plural))
)

(defun new_lesson ()
	(when (= 2 (player-level *player*)) (push "the" *articles*))
	(when (= 3 (player-level *player*)) (push "this" *articles*))
	(when (= 4 (player-level *player*)) (push "we" *L1-pronouns*))
	(when (= 5 (player-level *player*)) (push "that" *articles*))
	(when (= 6 (player-level *player*)) (push "he" *L1-pronouns*) (push "she" *L1-pronouns*))
	(when (= 7 (player-level *player*)) (push "they" *L1-pronouns*))
	(when (= 8 (player-level *player*)) (push "you" *L1-pronouns*))
)
