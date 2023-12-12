(defparameter vec1 '(1 2))
(defparameter vec2 '(3 2 4))
(defparameter vec3 '(2 1 3))
(defparameter vec4 '(6 2 1))
(defparameter l ())
(defparameter zero 0)
(defparameter one 1)
(defparameter three 3)
(defparameter obj '(x y))

(defun myplus (n m)
  (cond
	((zerop m) n)
	(t (1+ (myplus n (1- m))))))

(defun myminus (n m)
  (cond
	((zerop m) n)
	(t (1- (myminus n (1- m))))))

(defun addvec (vec)
  (cond
	((null vec) 0)
	(t (+ (car vec) (addvec (cdr vec))))))

(defun times (n m)
  (cond
	((zerop m) 0)
	(t (+ n (times n (1- m))))))

(defun vec+ (vec1 vec2)
  (cond
	((null vec1) vec2)
	((null vec2) vec1)
	(t (cons (+ (car vec1) (car vec2)) (vec+ (cdr vec1) (cdr vec2))))))

(defun gt (n m)
  (cond
	((zerop n) nil)
	((zerop m) t)
	(t (gt (1- n) (1- m)))))

(defun lt (n m)
  (cond
	((zerop m) nil)
	((zerop n) t)
	(t (lt (1- n) (1- m)))))

(defun == (n m)
  (cond
	((> n m) nil)
	((< n m) nil)
	(t t)))

(defun ^ (n m)
  (cond
	((zerop m) 1)
	(t (* n (^ n (1- m))))))

(defun my-length (lat)
  (cond
	((null lat) 0)
	(t (1+ (my-length (cdr lat))))))

(defun pick (n lat)
  (cond
	((null lat) nil)
	((zerop (1- n)) (car lat))
	(t (pick (1- n) (cdr lat)))))

(defun rempick (n lat)
  (cond
	((null lat) nil)
	((zerop (1- n)) (rempick (1- n) (cdr lat)))
	(t (cons (car lat) (rempick (1- n) (cdr lat))))))

(defun no-nums (lat)
  (cond
	((null lat) nil)
	((numberp (car lat)) (no-nums (cdr lat)))
	(t (cons (car lat) (no-nums (cdr lat))))))

(defun all-nums (lat)
  (cond
	((null lat) nil)
	((numberp (car lat)) (cons (car lat) (all-nums (cdr lat))))
	(t (all-nums (cdr lat)))))

(defun eqan? (a1 a2)
  (cond
	((numberp a1) 
	 (cond 
	   ((numberp a2) (= a1 a2))
	   (t nil)))
	((numberp a2) nil)
	(t (eq a1 a2))))

(defun duplicate (n obj)
  (cond	((zerop n) nil)
	(t (cons obj (duplicate (1- n) obj)))))

(defun multvec (v)
  (cond
	((null v) 1)
	(t (* (car v) (multvec (cdr v))))))

(defun my-index (a lat &optional (n 0 n-supplied-p))
  (cond
	((null lat) nil)
	((eq (car lat) a) n)
	(t (my-index a (cdr lat) (1+ n)))))

(defun product (vec1 vec2)
  (cond
	((null vec1) vec2)
	((null vec2) vec1)
	(t (cons (* (car vec1) (car vec2)) (product (cdr vec1) (cdr vec2))))))

(defun dot-product (vec1 vec2)
  (cond
	((null vec1) 0)
	(t (+ (* (car vec1) (car vec2)) (dot-product (cdr vec1) (cdr vec2))))))

(defun my/ (n m)
  (cond
	((< n m) 0)
	((>= 0 (- n m)) 1)
	(t (1+ (my/ (- n m) m)))))

(defun reminder (n m)
  (- n (* m (my/ n m))))

(defun lte (n m)
  (cond
	((zerop n) t)
	((zerop m) nil)
	(t (lte (1- n) (1- m)))))

