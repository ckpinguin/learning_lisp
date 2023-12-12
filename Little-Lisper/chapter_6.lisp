(load "lisp-unit.lisp")
(use-package :lisp-unit)

(defun leftmost (l)
  (cond
	((null l) nil)
	((atom (car l)) (car l))
	(t (leftmost (car l)))))

(defun leftmost2 (l)
  (cond
	((null l) nil)
	((non-atom? (car l)) (leftmost (car l)))
	(t (car l))))

(defun non-atom? (s)
  (my-not (atom s)))

(defun my-not (s)
  (cond
	(s nil)
	(t t)))

(defun rember* (a l)
  (cond
	((null l) nil)
	((non-atom? (car l)) (cons (rember* a (car l)) (rember* a (cdr l))))
	(t (cond
		 ((eq (car l) a) (rember* a (cdr l)))
		 (t (cons (car l) (rember* a (cdr l))))))))

(defun insertR* (new old l)
  (cond
	((null l) nil)
	((non-atom? (car l)) (cons (insertR* new old (car l)) (insertR* new old (cdr l))))
	(t (cond
		 ((eq (car l) old) (cons (car l) (cons new (insertR* new old (cdr l)))))
		 (t (cons (car l) (insertR* new old (cdr l))))))))

(defun occur* (a l)
  (cond
	((null l) 0)
	((non-atom? (car l)) (+ (occur* a (car l)) (occur* a (cdr l))))
	(t (cond
		 ((eq (car l) a) (1+ (occur* a (cdr l))))
		 (t (occur* a (cdr l)))))))

(defun subst* (new old l)
  (cond
	((null l) nil)
	((non-atom? (car l)) (cons (subst* new old (car l)) (subst* new old (cdr l))))
	(t (cond
		 ((eq (car l) old) (cons new (subst* new old (cdr l))))
		 (t (cons (car l) (subst* new old (cdr l))))))))

(defun insertL* (new old l)
  (cond
	((null l) nil)
	((non-atom? (car l))
	 (cons (insertL* new old (car l)) (insertL* new old (cdr l))))
	((eq (car l) old)
	 (cons new (cons (car l) (insertL* new old (cdr l)))))
	(t (cons (car l) (insertL* new old (cdr l))))))

(defun member* (a l)
  (cond
	((null l) nil)
	((non-atom? (car l)) (or 
						  (member* a (car l)) 
						  (member* a (cdr l))))
	(t (or
		 (eq (car l) a)
		 (member* a (cdr l))))))

(defun member2* (a l)
  (cond
	((null l) nil)
	((atom (car l)) (or 
					 (eq (car l) a)
					 (member2* a (cdr l))))
	(t (or
		(member2* a (car l))
		(member2* a (cdr l))))))

(defun eqan? (a1 a2)
  (cond
	((numberp a1) 
	 (cond 
	   ((numberp a2) (= a1 a2))
	   (t nil)))
	((numberp a2) nil)
	(t (eq a1 a2))))

; Great example for and-or-catching patterns!
(defun eqlist? (l1 l2)
  (cond
	((and (null l1) (null l2)) t)
	((or (null l1) (null l2)) nil)
	((and (non-atom? (car l1))
		  (non-atom? (car l2))
		  (and (eqlist? (car l1) (car l2))
			   (eqlist? (cdr l1) (cdr l2)))))
	((or (non-atom? (car l1))
		 (non-atom? (car l2))) nil)
	(t (and
		(eqan? (car l1) (car l2))
		(eqlist? (cdr l1) (cdr l2))))))
	  
; My first naiive try only copes with single atoms 
(defun my-equal? (s1 s2)
  (cond
	((and (atom s1) (atom s2)) (eqan? s1 s2))
	((or (atom s1) (atom s2)) nil)
	(t (or (my-equal? (car s1) (car s2))))))

; This one is better really for any S-expression
(defun equal? (s1 s2)
  (cond
	((and (atom s1) (atom s2)) (eqan? s1 s2))
	((and (non-atom? s1) (non-atom? s2))
	 (eqlist? s1 s2))
	(t nil)))

(defun eqlist2? (l1 l2)
	(cond
	  ((and (null l1) (null l2)) t)
	  ((or (null l1) (null l2)) nil)
	  (t (and
		  (equal? (car l1) (car l2))
		  (eqlist2? (cdr l1) (cdr l2))))))

(defun s-rember (s l)
  (cond
	((null l) nil)
	((equal? (car l) s) (cdr l))
	(t (cons (car l) (s-rember s (cdr l))))))

; Exercises
(defparameter l1 '((fried potatoes) (baked (fried)) tomatoes))
(defparameter l2 '(((chili) chili (chili))))
(defparameter l3 ())
(defparameter lat1 '(chili and hot))
(defparameter lat2 '(baked fried))
(defparameter a 'fried)
(defparameter ln1 '((1 (6 6 ()))))
(defparameter ln2 '((1 2 (3 6)) 1))

(defun down* (l)
  (cond
	((null l) nil)
	((non-atom? (car l)) (cons (down* (car l)) (down* (cdr l))))
	(t (cons (list (car l)) (down* (cdr l))))))

(define-test 6.1a 
  (assert-equal '((((chili)) (chili) ((chili)))) (down* l2)))

(define-test 6.1b
  (assert-equal () (down* l3)))

(define-test 6.1c
  (assert-equal '((chili) (and) (hot))  (down* lat1)))

(defun occurN* (lat l)
  (cond
	((null lat) 0)
	(t (+ (occur* (car lat) l) (occurN* (cdr lat) l)))))

(define-test 6.2a
  (assert-equal 3 (occurN* lat1 l2)))

(define-test 6.2b
  (assert-equal 3 (occurN* lat2 l1)))

(define-test 6.2c
  (assert-equal 0 (occurN* lat1 l3)))

(defun double* (a l)
  (cond
	((null l) nil)
	((non-atom? (car l)) (cons (double* a (car l)) (double* a (cdr l))))
	((eq (car l) a) (cons a (cons (car l) (double* a (cdr l)))))
	(t (cons (car l) (double* a (cdr l))))))

(define-test 6.3a
  (assert-equal '((fried fried potatoes) (baked (fried fried)) tomatoes) (double* a l1)))

(define-test 6.3b
  (assert-equal '(((chili) chili (chili))) (double* a l2)))

(define-test 6.3c
  (assert-equal '(baked fried fried) (double* a lat2)))

(defun list+ (l)
  (cond
	((null l) 0)
	((non-atom? (car l)) (+ (list+ (car l)) (list+ (cdr l))))
	(t (cond
		 ((numberp (car l)) (+ (car l) (list+ (cdr l))))
		 (t (list+ (cdr l)))))))

(define-test 6.6a
  (assert-equal 13 (list+ ln1)))

(define-test 6.6b
  (assert-equal 13 (list+ ln2)))

(define-test 6.6c
  (assert-equal 0 (list+ l3) 0))


; some endrecursions (working on call-stack)
(defun g* (lvec acc)
  (cond
	((null lvec) acc)
	((atom (car lvec))
	 (g* (cdr lvec) (+ (car lvec) acc)))
	(t (g* (car lvec) (g* (cdr lvec) acc)))))

(defun f* (l acc)
  (cond
	((null l) acc)
	((atom (car l))
	 (cond
	   ((member (car l) acc) (f* (cdr l) acc))
	   (t (f* (cdr l) (cons (car l) acc)))))
	(t (f* (car l) (f* (cdr l) acc)))))

(defun occurAcc (a lat acc)
  (cond
	((null lat) acc)
	(t (cond
		 ((eq (car lat) a) (occurAcc a (cdr lat) (1+ acc)))
		 (t (occurAcc a (cdr lat) acc))))))

; try to get end-recursion on general lists?
; doesn't work out with car-cdr-recursion it seems
; Of course not, because this is a multiple-recursion pattern (tree-like)
; so we get the result only of one "end".
(defun occurAcc* (a l acc)
  (cond
	((null l) acc)
	((non-atom? (car l)) (occurAcc* a (car l) acc) (occurAcc* a (cdr l) acc))
	(t (cond
		 ((eq (car l) a) (occurAcc* a (cdr l) (1+ acc)))
		 (t (occurAcc* a (cdr l) acc))))))

(run-tests)