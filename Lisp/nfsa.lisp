;Ferrara	Jacopo	924022

(defstruct automa
stato-iniziale
stato-finale
delta
)


(defun is-regex (RE)
(cond ((atom RE) t)
	((member (first RE) '(c a z o))
	(every #'is-regex (rest RE)))
))


(defun nfsa-compile-regex (RE)
	(if (is-regex RE)
		(let ((iniziale (gensym))
		(finale (gensym)))
	;atomo	
	(cond ((atom RE) 
		(make-automa
		:stato-iniziale iniziale
		:stato-finale finale
		:delta (list(list iniziale RE finale))))
	;concatenazione c
	((eq (first RE) 'c)
	(let ((automi (mapcar #'nfsa-compile-regex (rest RE))))
		(make-automa
		:stato-iniziale (automa-stato-iniziale (first automi))
		:stato-finale (automa-stato-finale (first (last automi)))
		:delta (append(apply #'append(mapcar #'automa-delta automi))
		(list(list (automa-stato-finale (first automi)) nil (automa-stato-iniziale (second automi)))))))
	;and a
	((eq (first RE) 'a)
	(let ((automi (mapcar #'nfsa-compile-regex (rest RE))))
		(make-automa
		:stato-iniziale iniziale
		:stato-finale finale
		:delta (append (apply #'append(mapcar #'automa-delta automi))
		(list(list iniziale nil (automa-stato-iniziale (first automi)))
		(list iniziale nil (automa-stato-iniziale (second automi)))
		(list (automa-stato-finale (first automi)) nil finale)
		(list (automa-stato-finale (second automi)) nil finale)))))
	;chiusura di Kleene z
	((eq (first RE) 'z)
	(let ((automi (mapcar #'nfsa-compile-regex (rest RE))))
		(make-automa
		:stato-iniziale iniziale
		:stato-finale finale
		:delta (append(apply #'append(mapcar #'automa-delta automi))
		(list(list iniziale nil (automa-stato-iniziale (first automi)))
		(list iniziale nil finale)(list (automa-stato-finale (first automi)) nil (automa-stato-iniziale (first automi)))
		(list (automa-stato-finale (first automi)) nil finale))))))
	;or o 
	((eq (first RE) 'o)
	(let ((automi (mapcar #'nfsa-compile-regex (rest RE))))
		(make-automa
		:stato-iniziale iniziale
		:stato-finale finale
		:delta (append(apply #'append(mapcar #'automa-delta automi))
		(list(list iniziale nil (automa-stato-iniziale (first automi)))
		(list (automa-stato-finale (first automi)) nil finale)
		(list iniziale nil finale))))))
	)))
	NIL
	)))


(defun nfsa-recognize (FA input)


)
