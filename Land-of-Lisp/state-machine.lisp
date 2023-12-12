(defun start (input-syms
	      &aux (this-input (first syms)))
  (cond ((null input-syms) 'start)
	((equal this-input 'nickel)
	 (format t "~&~A" "Clunk!")
	 (have-5 (rest input-syms)))
	((equal this-input 'dime)
	 (format t "~&~A" "Clink!")
	 (have-10 (rest input-syms)))
	((equal this-input 'coin-return)
	 (format t "~&~A" "Nothing to return.")
	 (start (rest input-syms)))
	(t (error "No arc from ~A with label ~A."
		  'start this-input))))

(defun compile-arc (arc)
  (let ((a (arc-action arc)))
    `((equal this-input ',(arc-label arc))
      (format t "~&~A" ,a)
      (, (node-name (arc-to arc))
	 (rest input-syms)))))

(defun compile-node (node)
  (let ((name (node-name node))
	(arc-clauses
	 (mapcar #'compile-arc
		 (node-outputs node))))
    `(defun ,name (input-syms
		   &aux (this-input
			 (first input-syms)))
       (cond ((null input-syms) ',name)
	     ,@arc-clauses
	     (t (format t
			"~&There is no arc from ~A with label ~S"
			',name this-input))))))

(defmacro compile-machine ()
  `(progn ,@(mapcar #'compile-node *nodes*)))