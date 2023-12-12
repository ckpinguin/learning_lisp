;(use-package :lisp-user)
(load "lisp-unit.lisp")
;(ql:quickload :lisp-unit)
(use-package :lisp-unit)
(in-package :lisp-unit)


(defun ^ (n m)
  (cond
	((zerop m) 1)
	(t (* n (^ n (1- m))))))

(defun ineff_numbered? (aexp)
  (cond
	((atom aexp) (numberp aexp))
	((eq (car (cdr aexp)) (quote +))
	 (and
	  (numbered? (car aexp))
	  (numbered? (car (cdr (cdr aexp))))))
	((eq (car (cdr aexp)) (quote *))
	 (and
	  (numbered? (car aexp))
	  (numbered? (car (cdr (cdr aexp))))))
	((eq (car (cdr aexp)) (quote ^))
	 (and
	  (numbered? (car aexp))
	  (numbered? (car (cdr (cdr aexp))))))))

(defun numbered? (aexp)
  (cond
	((atom aexp) (numberp aexp))
	(t (and
		(numbered? (car aexp))
		(numbered? (car (cdr (cdr aexp))))))))

(defun value (aexp)
  (cond
	((numberp aexp) aexp)
	((eq (car (cdr aexp)) (quote +)) (+ (value (car aexp)) (value (car (cdr (cdr aexp))))))
	((eq (car (cdr aexp)) (quote *)) (* (value (car aexp)) (value (car (cdr (cdr aexp))))))
	(t (^ (value (car aexp)) (value (car (cdr (cdr aexp))))))))

(defun 1st-sub-exp (aexp)
  (car aexp))

(defun 2nd-sub-exp (aexp)
  (car (cdr (cdr aexp))))

(defun operator (aexp)
  (car (cdr aexp)))

(defun value2 (aexp)
  (cond
	((numberp aexp) aexp)
	((eq (operator aexp) (quote plus)) (+ (value2 (1st-sub-exp aexp)) (value2 (2nd-sub-exp aexp))))
	((eq (operator aexp) (quote times)) (* (value2 (1st-sub-exp aexp)) (value2 (2nd-sub-exp aexp))))
	(t (^ (value2 (1st-sub-exp aexp)) (value2 (2nd-sub-exp aexp))))))

; Using count ()'s to represent numbers
(defun null? (s)
  (and
   (atom s)
   (eq s (quote ()))))

(defun zero? (n)
  (null? n))

(defun add1 (n)
  (cons (quote ()) n))

(defun sub1 (n)
  (cond
	((zero? (cdr n)) (quote nil))
	(t (cdr n))))

(defun new+ (n m)
  (cond
	((zero? m) n)
	(t (add1 (new+ n (sub1 m))))))

(defun number? (n)
  (cond
	((null? n) t)
	(t (and
		(null? (car n))
		(number? (cdr n))))))

(defparameter aexp1 '(1 + (3 * 4)))
(defparameter aexp2 '((3 ^ 4) + 5))
(defparameter aexp3 '(3 * (4 * (5 * 6))))
(defparameter aexp4 5)
(defparameter l1 ())
(defparameter l2 '(3 + (66 6)))
(defparameter l3 '(x y z u))
(defparameter l4 '((x 1) (y 0)))
(defparameter l5 '((u 1) (v 1)))
(defparameter l6 ())
(defparameter l7 '((x 1) (y 0) (z 0)))
(defparameter l8 '((y 0) (u 0) (v 1)))
(defparameter lexp1 '(AND (OR x y) y))
(defparameter lexp2 '(AND (NOT y) (OR u v)))
(defparameter lexp3 '(OR x y))
(defparameter lexp4 'z)

(defun mk+exp (aexp1 aexp2)
  (cons aexp1 (cons (quote +) (cons aexp2 ()))))

(defun mk*exp (aexp1 aexp2)
  (cons aexp1 (cons (quote *) (cons aexp2 ()))))

(defun mk^exp (aexp1 aexp2)
  (cons aexp1 (cons (quote ^) (cons aexp2 ()))))

(define-test 7.1a
  (assert-equal aexp1 (mk+exp 1 (mk*exp 3 4))))

(define-test 7.1b
  (assert-equal aexp2 (mk+exp (mk^exp 3 4) 5)))

(define-test 7.1c
  (assert-equal aexp3 (mk*exp 3 (mk*exp 4 (mk*exp 5 6)))))

(defun aexp? (s)
  (cond
	((null s) nil)
	(t (numbered? s))))
	
(define-test 7.2a
  (assert-equal t (aexp? aexp1)))

(define-test 7.2b
  (assert-equal t (aexp? aexp2)))

(define-test 7.2c
  (assert-equal nil (aexp? l1)))

(define-test 7.2d
  (assert-equal nil (aexp? l2)))

(defun count-op (aexp)
  (cond
	((atom aexp) 0)
	((or
	  (eq (operator aexp) (quote +))
	  (eq (operator aexp) (quote *))
	  (eq (operator aexp) (quote ^)))
	 (1+ (+ (count-op (1st-sub-exp aexp)) (count-op (2nd-sub-exp aexp)))))
	(t (+ (count-op (1st-sub-exp aexp))) (count-op (2nd-sub-exp aexp)))))

(defun count-nums (aexp)
  (cond
	((atom aexp) 1)
	(t (+ (count-nums (1st-sub-exp aexp)) (count-nums (2nd-sub-exp aexp))))))

(defun count+ (aexp)
  (cond
	((atom aexp) 0)
	((eq (operator aexp) (quote +))
	 (1+ (+ (count+ (1st-sub-exp aexp)) (count+ (2nd-sub-exp aexp)))))
	(t (+ (count+ (1st-sub-exp aexp)) (count+ (2nd-sub-exp aexp))))))

(defun count* (aexp)
  (cond
	((atom aexp) 0)
	((eq (operator aexp) (quote *))
	 (1+ (+ (count* (1st-sub-exp aexp)) (count* (2nd-sub-exp aexp)))))
	(t (+ (count* (1st-sub-exp aexp)) (count* (2nd-sub-exp aexp))))))

(defun count^ (aexp)
  (cond
	((atom aexp) 0)
	((eq (operator aexp) (quote ^))
	 (1+ (+ (count^ (1st-sub-exp aexp)) (count^ (2nd-sub-exp aexp)))))
	(t (+ (count^ (1st-sub-exp aexp)) (count^ (2nd-sub-exp aexp))))))

(define-test 7.3a
  (assert-equal 2 (count-op aexp1)))

(define-test 7.3b
  (assert-equal 3 (count-op aexp3)))

(define-test 7.3c
  (assert-equal 0 (count-op aexp4)))

(define-test 7.3d
  (assert-equal 1 (count+ aexp1)))

(define-test 7.3e
  (assert-equal 1 (count* aexp1)))

(define-test 7.3f
  (assert-equal 0 (count^ aexp1)))

(define-test 7.4a
  (assert-equal 3 (count-nums aexp1)))

(define-test 7.4b
  (assert-equal 4 (count-nums aexp3)))

(define-test 7.4c
  (assert-equal 1 (count-nums aexp4)))

(defun numbered2? (aexp)
  (cond
	((null aexp) t) ; handle nil as subexpr's can be nil
	((atom aexp) (numberp aexp))
	(t (and (numbered2? (1-sub-exp aexp))
			(numbered2? (2-sub-exp aexp))
			(numbered2? (3-sub-exp aexp))
			(numbered2? (4-sub-exp aexp))))))

(defun cnt-aexp (aexp)
  (cond
	((null aexp) 0)
	((not (null (1-sub-exp aexp))) (1+ (cnt-aexp (cdr aexp))))
	(t (+  (cnt-aexp (cdr aexp))))))

(defun 1-sub-exp (aexp)
  (car (cdr aexp)))

(defun 2-sub-exp (aexp)
  (car (cdr (cdr aexp))))

(defun 3-sub-exp (aexp)
  (car (cdr (cdr (cdr aexp)))))

(defun 4-sub-exp (aexp)
  (car (cdr (cdr (cdr (cdr aexp))))))

(defun operator2 (aexp)
  (car aexp))

(defun value3 (aexp)
  (cond
	((null aexp) 1) ; TODO: Handle neutral element for * and + somehow
	((numberp aexp) aexp)
	((eq (operator2 aexp) (quote +)) (+ -1 (value3 (1-sub-exp aexp)) (value3 (2-sub-exp aexp)) (value3 (3-sub-exp aexp)) (value3 (4-sub-exp aexp)))) ; fix neutral element for every addition (not beutiful) 
	((eq (operator2 aexp) (quote *)) (* (value3 (1-sub-exp aexp)) (value3 (2-sub-exp aexp)) (value3 (3-sub-exp aexp)) (value3 (4-sub-exp aexp))))
	((eq (operator2 aexp) (quote ^)) (^ (value3 (1-sub-exp aexp)) (value3 (2-sub-exp aexp))))
	(t (^ (value3 (1-sub-exp aexp)) (value3 (2-sub-exp aexp))))))

(defparameter aexp5 '(+ 3 2 (* 7 8)))
(defparameter aexp6 '(* 3 4 5 6))
(defparameter aexp7 '(^ (+ 3 2 (* 7 8)) (* 3 4 5 6)))

(define-test 7.5a
  (assert-equal 3 (cnt-aexp aexp5)))

(define-test 7.5b
  (assert-equal 4 (cnt-aexp aexp6)))

(define-test 7.5c
  (assert-equal 2 (cnt-aexp aexp7)))

(define-test 7.5d
  (assert-equal 61 (value3 aexp5)))

(define-test 7.5e
  (assert-equal 360 (value3 aexp6)))

(define-test 7.5f
  (assert-equal 5232877930315167619332253963729133520308349598790460708174472978003089581595731266035032732214961900701648872267896076451821190141068363615493445778481369779390801431189161722233516376182641776496271239368761192811575958523155147751363301727232229805266973338553241499532685281610970132040164667514024965025673430165660975420404263997124314214994064083207920731270119204721919697278214025692306791249346320534524958778955283216837089582747791328077913800298931713572956832545515744988511191053466226025282667802449637685860089768377511819784867923379356600849013035443951087509076112845631383104358490171815746671102782192279047934527642573601 (value3 aexp7)))

(defun lexp? (s)
  (cond
	((null s) t)
	((atom (car s)) 
	 (cond
	   ((op? (operator2 s)) (lexp? (cdr s)))
	   ((symbolp (car s)) (lexp? (cdr s)))
	   (t nil)))
	(t (and 
		(lexp? (car s))
		(lexp? (cdr s))))))

(defun op? (a)
  (member a '(AND OR NOT)))

(defun nop? (a)
  (not (member a '(AND OR NOT))))

(defun non-atom? (s)
  (my-not (atom s)))

(defun my-not (s)
  (cond
	(s nil)
	(t t)))

(defun flatten (list)
  (mapcan #'(lambda (e) (if (consp e) (flatten e) (list e))) list))

(defun vars (lexp)
  (cond
	((null lexp) nil)
	((non-atom? (car lexp))
	 (cons (vars (car lexp)) (vars (cdr lexp))))
	(t (cond
		 ((nop? (car lexp)) (cons (car lexp) (vars (cdr lexp))))
		 (t (vars (cdr lexp)))))))

(defun covered? (lexp lat)
  (cond
	((null lexp) t)
	((atom (car lexp)) (and
						(nop? (car lexp))
						(member (car lexp) lat)
						(covered? (cdr lexp) lat)))
	(t (covered? (cdr lexp) lat))))

(defun lookup (var alist)
  (cond
	((null alist) nil)
	((eq (car (car alist)) var) (car (cdr (car alist))))
	(t (lookup var (cdr alist)))))

(defun firsts (alist)
  (cond
	((null alist) nil)
	(t (cons (car (car alist)) (firsts (cdr alist))))))

(defun Mlexp (lexp alist)
  (cond
	((null alist) t)
	(t (and 
		(covered? lexp (firsts alist))
		(cond
		  ((eq (operator2 lexp) 'OR)))))))

(define-test 7.6a
  (assert-true (lexp? lexp1)))

(define-test 7.6b
  (assert-true (lexp? lexp2)))

(define-test 7.6c
  (assert-true (lexp? lexp3)))

(define-test 7.6d
  (assert-false (lexp? aexp1)))

(define-test 7.6e
  (assert-false (lexp? l2)))

(define-test 7.7a
  (assert-true (covered? lexp1 l3)))

(define-test 7.7b
  (assert-false (covered? lexp2 l3)))

(define-test 7.7c
  (assert-true (covered? lexp4 l3)))

(define-test 7.8a
  (assert-equal 0 (lookup 'y l4)))

(define-test 7.8b
  (assert-equal 1 (lookup 'u l5)))

(define-test 7.8c
  (assert-equal nil (lookup 'y l6)))

(define-test 7.9a
  (assert-false (Mlexp lexp1 l7)))

(define-test 7.9b
  (assert-true (Mlexp lexp2 l8)))

(define-test 7.9c
  (assert-false (Mlexp lexp4 l7)))

(run-tests)