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
	(cond ((atom RE) 
		(make-automa
		:stato_iniziale iniziale
		:stato_finale finale
		:delta (list(list iniziale RE finale))))
	((eq (first RE) 'c)
	(let ((automi (mapcar #'nfsa-compile-regex (rest RE))))
		(make-automa
		:stato_iniziale (automa-stato-iniziale ((car) (first automi)))
		:stato_finale (automa-stato-finale ((cdr) (last automi)))
		:delta ((apply #'append (mapcar #'(automa-delta))) append ())	
	))
	((eq (first RE) 'a)
	
	)
	((eq (first RE) 'z)
	
	
	)
	((eq (first RE) 'z)
	
	
	))
	NIL
	)))


(defun nfsa-recognize (FA input)


)
