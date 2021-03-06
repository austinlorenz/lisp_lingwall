(defun language_lesson ()
    (cond ((= 0 (player-level *player*)) (lesson_one))
	  ((= 1 (player-level *player*)) (lesson_two))
	  ((= 2 (player-level *player*)) (lesson_three))
	  ((= 3 (player-level *player*)) (lesson_four))
	  ((= 4 (player-level *player*)) (lesson_five))
	  ((= 5 (player-level *player*)) (lesson_six))
	  ((= 6 (player-level *player*)) (lesson_seven))
	  ((= 7 (player-level *player*)) (lesson_eight))
	  ((= 8 (player-level *player*)) (lesson_nine))
	  ((= 9 (player-level *player*)) (lesson_ten))
	  (t (lesson_ten))
    )
)

(defun lesson_one ()
    (cls)
    (princ "You are authorized to use the following commands: ")
    (terpri) (terpri)
    (princ "Teleport: yo voy")
    (fresh-line)
    (princ "Fireball: yo hago un fuego")
    (fresh-line)
    (princ "Eat: yo como")
    (fresh-line)
    (princ "Drink: yo bebo")
    (fresh-line)
    (princ "Heal: yo estoy bien")
    (fresh-line)
    (princ "Use: yo uso")
    (terpri) (terpri)
    (princ "Spacebar to exit.")
    (read_input)
    (draw_world)
    (fresh-line)
)

(defun lesson_two ()
    (cls)
    (princ "Level 1 vocabulary: ")
    (terpri) (terpri)
    (princ "a bonfire: una fogata")
    (terpri) (terpri)
    (princ "Spacebar to exit.")
    (read_input)
    (draw_world)
    (fresh-line)
)

(defun lesson_three ()
    (cls)
    (princ "Level 2 vocabulary: ")
    (terpri) (terpri)
    (princ "the fire: el fuego")
    (fresh-line)
    (princ "the bonfire: la fogata")
    (terpri) (terpri)
    (princ "Spacebar to exit.")
    (read_input)
    (draw_world)
    (fresh-line)
)

(defun lesson_four ()
    (cls)
    (princ "Level 3 vocabulary: ")
    (terpri) (terpri)
    (princ "this fire: este fuego")
    (fresh-line)
    (princ "this bonfire: esta fogata")
    (terpri) (terpri)
    (princ "Spacebar to exit.")
    (read_input)
    (draw_world)
    (fresh-line)
)

(defun lesson_five ()
    (cls)
    (princ "Level 4 vocabulary: ")
    (terpri) (terpri)
    (princ "we (m) / we (f) make: nosotros / nosotras hacemos")
    (fresh-line)
    (princ "we (m) / we (f) go: nosotros / nosotras vamos")
    (fresh-line)
    (princ "we (m) / we (f) eat: nosotros / nosotras comemos")
    (fresh-line)
    (princ "we (m) / we (f) drink: nosotros / nosotras bebemos")
    (fresh-line)
    (princ "we (m) / we (f) are well: nosotros / nosotras estamos bien")
    (fresh-line)
    (princ "we (m) / we (f) use: nosotros / nosotras usamos")
    (terpri) (terpri)
    (princ "Spacebar to exit.")
    (read_input)
    (draw_world)
    (fresh-line)
)

(defun lesson_six ()
    (cls)
    (princ "Level 5 vocabulary: ")
    (terpri) (terpri)
    (princ "that fire: ese fuego")
    (fresh-line)
    (princ "that bonfire: esa fogata")
    (terpri) (terpri)
    (princ "Spacebar to exit.")
    (read_input)
    (draw_world)
    (fresh-line)
)

(defun lesson_seven ()
    (cls)
    (princ "Level 6 vocabulary: ")
    (terpri) (terpri)
    (princ "he / she makes: el / ella hace")
    (fresh-line)
    (princ "he / she goes: el / ella va")
    (fresh-line)
    (princ "he / she eats: el / ella come")
    (fresh-line)
    (princ "he / she drinks: el / ella bebe")
    (fresh-line)
    (princ "he / she is well: el / ella esta bien")
    (fresh-line)
    (princ "he / she uses: el / ella usa")
    (terpri) (terpri)
    (princ "Spacebar to exit.")
    (read_input)
    (draw_world)
    (fresh-line)
)

(defun lesson_eight ()
    (cls)
    (princ "Level 7 vocabulary: ")
    (terpri) (terpri)
    (princ "they (m) / they (f) make: ellos / ellas hacen")
    (fresh-line)
    (princ "they (m) / they (f) go: ellos / ellas van")
    (fresh-line)
    (princ "they (m) / they (f) eat: ellos / ellas comen")
    (fresh-line)
    (princ "they (m) / they (f) drink: ellos / ellas beben")
    (fresh-line)
    (princ "they (m) / they (f) are well: ellos / ellas estan bien")
    (terpri) (terpri)
    (princ "Spacebar to exit.")
    (read_input)
    (draw_world)
    (fresh-line)
)

(defun lesson_nine ()
    (cls)
    (princ "Level 8 vocabulary: ")
    (terpri) (terpri)
    (princ "you (informal) / you (formal) / you (plural) make: tu haces / usted hace / ustedes hacen")
    (fresh-line)
    (princ "you (informal) / you (formal) / you (plural) go: tu vas / usted va / ustedes van")
    (fresh-line)
    (princ "you (informal) / you (formal) / you (plural) eat: tu comes / usted come / ustedes comen")
    (fresh-line)
    (princ "you (informal) / you (formal) / you (plural) drink: tu bebes / usted bebe / ustedes beben")
    (fresh-line)
    (princ "you (informal) / you (formal) / you (plural) are well: tu estas bien / usted esta bien / ustedes estan bien")
    (fresh-line)
    (princ "you (informal) / you (formal) / you (plural) use: tu usas / usted usa / ustedes usan")
    (terpri) (terpri)
    (princ "Spacebar to exit.")
    (read_input)
    (draw_world)
    (fresh-line)
)

(defun lesson_ten ()
    (cls)
    (princ "Level 9 vocabulary: ")
    (terpri) (terpri)
    (princ "the fires: los fuegos")
    (fresh-line)
    (princ "the bonfires: las fogatas")
    (fresh-line)
    (princ "some fires: unos fuegos")
    (fresh-line)
    (princ "some bonfires: unas fogatas")
    (fresh-line)
    (princ "these fires: estos fuegos")
    (fresh-line)
    (princ "those fires: esos fuegos")
    (fresh-line)
    (princ "these bonfires: estas fogatas")
    (fresh-line)
    (princ "those bonfires: esas fogatas")
    (terpri) (terpri)
    (princ "Spacebar to exit.")
    (read_input)
    (draw_world)
    (fresh-line)
)


