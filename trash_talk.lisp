(defmethod trash_talk (m)
    (when (= (random 5) 0) (message (concatenate 'string "The " (creature-type m) " laughs at you!")))
)
