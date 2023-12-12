(load "lisp-unit.lisp")
(use-package :lisp-unit)

(defparameter x 'comma)
(defparameter y 'dot)
(defparameter a 'kiwis)
(defparameter b 'plums)
(defparameter lat1 '(bananas kiwis))
(defparameter lat2 '(peaches apples bananas))
(defparameter lat3 '(kiwis pears plums bananas cherries))
(defparameter lat4 '(kiwis mangoes kiwis guavas kiwis))
(defparameter l1 '((curry) () (chicken) ()))
(defparameter l2 '((peaches) (and cream)))
(defparameter l3 '((plums) and (ice) and cream))
(defparameter l4 ())

(defun member? (a lat)
  (cond
	((null lat) nil)
	(t (or
		(eq (car lat) a)
		(member? a (cdr lat))))))

(defun rember (a lat)
  (cond
	((null lat) nil)
	(t (cond
		 ((eq (car lat) a) (cdr lat))
		 (t (cons (car lat) (rember a (cdr lat))))))))

(defun multirember (a lat)
  (cond
	((null lat) nil)
	(t (cond
		 ((eq (car lat) a) (multirember a (cdr lat)))
		 (t (cons (car lat) (multirember a (cdr lat))))))))

(defun multiinsertR (new old lat)
  (cond
	((null lat) nil)
	(t (cond
		 ((eq (car lat) old) (cons (car lat) (cons new (multiinsertR new old (cdr lat)))))
		 (t (cons (car lat) (multiinsertR new old (cdr lat))))))))

(defun multiinsertL (new old lat)
  (cond
	((null lat) nil)
	(t (cond
		 ((eq (car lat) old) (cons new (cons old (multiinsertL new old (cdr lat)))))
		 (t (cons (car lat) (multiinsertL new old (cdr lat))))))))

(defun multisubst (new old lat)
  (cond
	((null lat) nil)
	(t (cond
		 ((eq (car lat) old) (cons new (multisubst new old (cdr lat))))
		 (t (cons (car lat) (multisubst new old (cdr lat))))))))

(defun occur (a lat)
  (cond
	((null lat) 0)
	(t (cond
		 ((eq (car lat) a) (+ 1 (occur a (cdr lat))))
		 (t (occur a (cdr lat)))))))

(defun one? (n)
  (cond
	((numberp n) (cond
				   ((== n 1) t)
				   (t nil)))
	(t nil)))

(defun eff_one? (n)
  (eq n 1))

(defun rempick (n lat)
  (cond
	((null lat) nil)
	((one? n) (cdr lat))
	(t (cons (car lat) (rempick (1- n) (cdr lat))))))

(defun multisubst-kiwis (new lat)
  (cond
	((null lat) nil)
	(t (cond
		 ((eq (car lat) 'kiwis) (cons new (multisubst-kiwis new (cdr lat))))
		 (t (cons (car lat) (multisubst-kiwis new (cdr lat))))))))

(define-test 5.1a 
  (assert-equal '(bananas plums) (multisubst-kiwis b lat1)))

(define-test 5.1b
  (assert-equal '(peaches apples bananas) (multisubst-kiwis y lat2)))

(define-test 5.1c 
  (assert-equal '(dot mangoes dot guavas dot) (multisubst-kiwis y lat4)))

(define-test 5.1d
  (assert-equal () (multisubst-kiwis y l4)))

(define-test 5.2a
  (assert-equal '(bananas comma) (multisubst2 x a b lat1)))

(define-test 5.2b
  (assert-equal '(dot pears dot bananas cherries) (multisubst2 y a b lat3)))

(define-test 5.2c
  (assert-equal '(bananas kiwis) (multisubst2 a x y lat1)))

(define-test 5.3a
  (assert-equal '((bananas) (kiwis)) (multidown lat1)))

(define-test 5.3b
  (assert-equal () (multidown l4)))

(define-test 5.4a
  (assert-equal 0 (occurN lat1 l4)))

(define-test 5.4b
  (assert-equal 1 (occurN lat1 lat2)))

(define-test 5.4c
  (assert-equal 2 (occurN lat1 lat3)))

(define-test 5.5a
  (assert-equal () (I lat1 l4)))

(define-test 5.5b
  (assert-equal 'bananas (I lat1 lat2)))

(define-test 5.5c 
  (assert-equal 'kiwis (I lat1 lat3)))

(define-test 5.5d
  (assert-equal () (multiI lat1 l4)))

(define-test 5.5e 
  (assert-equal '(bananas) (multiI lat1 lat2)))

(define-test 5.5f 
  (assert-equal '(kiwis bananas) (multiI lat1 lat3)))

(define-test 5.9a
  (assert-equal () (multiup l4)))

(define-test 5.9b
  (assert-equal '(curry chicken) (multiup l1)))

(define-test 5.9c
  (assert-equal '(peaches (and cream)) (multiup l2)))

(defun multisubst2 (new o1 o2 lat)
  (cond
	((null lat) ())
	(t (cond
		 ((or (eq (car lat) o1)
			  (eq (car lat) o2)) (cons new (multisubst2 new o1 o2 (cdr lat))))
		 (t (cons (car lat) (multisubst2 new o1 o2 (cdr lat))))))))

(defun multidown (lat)
  (cond
	((null lat) nil)
	(t (cons (list (car lat)) (multidown (cdr lat))))))

(defun occurN (alat lat)
  (cond 
	((null alat) 0)
	(t (+ (occur (car alat) lat) (occurN (cdr alat) lat)))))

(defun I (lat1 lat2)
  (cond
	((null lat2) nil)
	(t (cond
		 ((member (car lat2) lat1) (car lat2))
		 (t (I lat1 (cdr lat2)))))))
	
(defun multiI (lat1 lat2)
  (cond
	((null lat2) nil)
	(t (cond
		 ((member (car lat2) lat1) (cons (car lat2) (multiI (cdr lat2) lat1)))
		 (t (multiI (cdr lat2) lat1))))))

(defun count0 (vec)
  (cond
	((null vec) 0)
	(t (cond
		 ((zerop (car vec)) (1+ (count0 (cdr vec))))
		 (t (count0 (cdr vec)))))))

(defun multiup (l)
  (cond
	((null l) nil)
	(t (cond
		 ((= 1 (length (car l))) (cons (car (car l)) (multiup (cdr l))))
		 ((null (car l)) (multiup (cdr l)))
		 (t (cons (car l) (cdr l)))))))

(run-tests)
