;Ferrara	Jacopo	924022

(defstruct automa
stato_iniziale
stato_finale
delta
)



(defun is-regex (RE)
(cond ((atom RE) t)
	((member (first RE) '(c a z o))
	(every #'is-regex (rest RE)))
))




(defun nfsa-compile-regex (RE)
	(if (is-regex RE) 
	(setq nuovoAutoma (make-automa: 
	:stato_iniziale ))
	(NIL)
)


(defun nfsa-recognize (FA input)


)
