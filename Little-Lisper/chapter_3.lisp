(defun rember (a lat)
  "Removes first occurrence of atom 'a in list of atoms 'lat"
  (cond
	((null lat) nil)
	((eq a (car lat)) (cdr lat))
	(t (cons (car lat) (rember a (cdr lat))))))

(defun rember2 (a lat)
  "Removes all occurrences of atom 'a in list of atoms 'lat"
; slightly more overhead as recursion takes places
; in two cases
  (cond
	((null lat) nil)
	((eq a (car lat)) (rember2 a (cdr lat)))
	(t (cons (car lat) (rember2 a (cdr lat))))))

(defun firsts (l)
  (cond
	((null l) nil)
	(t (cons (car (car l)) (firsts (cdr l))))))

(defun insertR (new old lat)
  (cond
	((null lat) nil)
	(t (cond
		 ((eq (car lat) old) (cons old (cons new (cdr lat))))
		 (t (cons (car lat) (insertR new old (cdr lat))))))))

(defun insertL (new old lat)
  (cond
	((null lat) nil)
	((eq (car lat) old) (cons new lat))
	(t (cons (car lat) (insertL new old (cdr lat))))))

(defun mysubst (new old lat)
  (cond
	((null lat) nil)
	((eq (car lat) old) (cons new (cdr lat)))
	(t (cons (car lat) (mysubst new old (cdr lat))))))

(defun mysubst2 (new old1 old2 lat)
  (cond
	((null lat) nil)
	((or
	  (eq (car lat) old1)
	  (eq (car lat) old2))
	 (cons new (cdr lat)))
	(t (cons (car lat) (mysubst2 new old1 old2 (cdr lat))))))

