(defparameter l1 '(german chococate cake))
(defparameter l2 '(poppy seed cake))
(defparameter l3 '((linzer) (torte) ()))
(defparameter l4 '((bleu cheese) (and) (red) (wine)))
(defparameter l5 '(() ()))
(defparameter a1 'coffee)
(defparameter a2 'seed)
(defparameter a3 'poppy)

(defun lat? (l)
  (cond
	((null l) t)
	((atom (car l)) (lat? (cdr l))) ;no end-recursion here
	; because we have to go through the full list anyway
	(t nil)))

(defun member? (a lat)
  (cond
	((null lat) nil)
	((eq a (car lat)) t)
	(t (member? a (cdr lat)))))

(defun member2? (a lat)
  (cond
	((null lat) nil)
	(t
	 (or
	  (member? a (cdr lat))
	  (eq a (car lat))))))

(defun lat_if? (l)
  (if
   (null l) t
   (if
	(atom (car l)) (lat_if? (cdr l))
	nil)))

(defun member_if? (a lat)
  (if
   (null lat) nil
   (or
	(eq a (car lat))
	(member_if? a (cdr lat)))))

(defun nonlat? (l)
  (cond
	((null l) t)
	((not (atom (car l))) (nonlat? (cdr l)))
	(t nil)))

(defun member-cake? (l)
  (if
   (member 'cake l) t
   nil))

(defun member-twice? (a lat &optional found)
  (cond
	((null lat) nil)
	(t (cond
		 ((eq a (car lat))
		  (if found t
			  (member-twice? a (cdr lat) t)))
		 (t (member-twice? a (cdr lat) found))))))