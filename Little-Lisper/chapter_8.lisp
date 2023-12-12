(load "lisp-unit.lisp")
(use-package :lisp-unit)

(defun set? (lat)
  (cond
	((null lat) t)
	((member (car lat) (cdr lat)) nil)
	(t (set? (cdr lat)))))

(defun makeset (lat)
  (cond
	((null lat) ())
	((member (car lat) (cdr lat))
	 (makeset (cdr lat)))
	(t (cons (car lat) (makeset (cdr lat))))))

(defun multirember (a lat)
  (cond
	((null lat) nil)
	(t (cond
		 ((eq (car lat) a) (multirember a (cdr lat)))
		 (t (cons (car lat) (multirember a (cdr lat))))))))

(defun makeset2 (lat)
  (cond
	((null lat) ())
	(t (cons (car lat)
			 (makeset (multirember (car lat) (cdr lat)))))))

(defun subset? (set1 set2)
  (cond
	((null set1) t)
	((member (car set1) set2) (subset? (cdr set1) set2))
	(t nil)))

(defun subset-and? (set1 set2)
  (cond
	((null set1) t)
	(t (and
		(member (car set1) set2)
		(subset-and? (cdr set1) set2)))))

(defun eqset? (set1 set2)
  (cond
	((subset? set1 set2) (subset? set2 set1))
	(t nil)))

(defun eqset-and? (set1 set2)
  (and
   (subset? set1 set2)
   (subset? set2 set1)))

(defun intersect? (set1 set2)
  (cond
	((null set1) nil)
	((member (car set1) set2) t)
	(t (intersect? (cdr set1) set2))))

; something strange happens here, we don't get boolean value, we
; get a list with the first matching atom of the sets!
; As I found out in chapter 9 (using recursive lambdas for or/and
; to implement intersection/subset generically) this is because
; or evaluates either to nil or to whatever the first non-nil
; expression evaluates to (non-nil is considered 't logically). 
(defun intersect2? (set1 set2)
  (cond
	((null set1) nil)
	(t (or
		(member (car set1) set2)
		(intersect2? (cdr set1) set2)))))

(defun intersect (set1 set2)
  (cond
	((null set1) ())
	((member (car set1) set2) (cons (car set1) (intersect (cdr set1) set2)))
	(t (intersect (cdr set1) set2))))

(defun intersect2 (set1 set2)
  (cond
	((null set1) ())
	((not (member (car set1) set2)) (intersect (cdr set1) set2))
	(t (cons (car set1) (intersect (cdr set1) set2)))))

(defun my-union (set1 set2)
  (cond
	((null set1) set2)
	((member (car set1) set2) (my-union (cdr set1) set2))
	(t (cons (car set1) (my-union (cdr set1) set2)))))

(defun my-complement (set1 set2)
  (cond
	((null set1) ())
	((member (car set1) set2) (my-complement (cdr set1) set2))
	(t (cons (car set1) (my-complement (cdr set1) set2)))))

(defun intersectall (l-set)
  (cond
	((null (cdr l-set)) (car l-set))
	(t (intersect (car l-set) (intersectall (cdr l-set))))))

(defun my-first (p)
  (car p))

(defun my-second (p)
  (car (cdr p)))

(defun build-pair (a1 a2)
  (cons a1 (cons a2 ())))

(defun eqpair? (p1 p2)
  (and
   (eq (first p1) (first p2))
   (eq (second p1) (second p2))))

(defun my-third (l)
  (car (cdr (cdr l))))

(defun member* (a l)
  (cond
	((null l) nil)
	((non-atom? (car l)) (or 
						  (member* a (car l)) 
						  (member* a (cdr l))))
	(t (or
		 (eq (car l) a)
		 (member* a (cdr l))))))

(defun firsts (l)
  (cond
	((null l) nil)
	(t (cons (car (car l)) (firsts (cdr l))))))

(defun seconds (l)
  (cond
	((null l) nil)
	(t (cons (car (cdr (car l))) (seconds (cdr l))))))

(defun try-fun? (rel)
  (cond
	((null rel) t)
	((member* (my-first (car rel)) (cdr rel)) nil)
	(t (fun? (cdr rel)))))

(defun fun? (rel)
  (cond
	((null rel) t)
	((member (my-first (car rel)) (firsts (cdr rel))) nil)
	(t (fun2? (cdr rel)))))

(defun fun3? (rel)
	(set? (firsts rel)))

(defun revrel (rel)
  (cond
	((null rel) ())
	(t (cons
		(build-pair
		 (my-second (car rel)) 
		 (my-first (car rel)))
		(revrel (cdr rel))))))

; as definition says we get a fun as argument, 
; the first funcall is superfluous. This just shows my
; first thoughts...
(defun my-fullfun? (fun)
  (and
   (fun? fun)
   (fun? (revrel fun))))

(defun fullfun? (fun)
  (set? (seconds fun)))

(defun one-to-one (fun)
  (fun? (revrel fun)))

(defparameter r1 '((a b) (a a) (b b)))
(defparameter r2 '((c c)))
(defparameter r3 '((a c) (b c)))
(defparameter r4 '((a b) (b a)))
(defparameter f1 '((a 1) (b 2) (c 2) (d 1)))
(defparameter f2 ())
(defparameter f3 '((a 2) (b 1)))
(defparameter f4 '((1 $) (3 *)))
(defparameter d1 '(a b))
(defparameter d2 '(c d))
(defparameter x 'a)

(defun domset (rel)
  (cond
	((null rel) nil)
	(t (union (makeset (firsts rel))
			  (makeset (seconds rel))))))

(define-test 8.1a
  (assert-true (eqset? '(a b) (domset r1))))

(define-test 8.1b
  (assert-true (eqset? '(c) (domset r2))))

(define-test 8.1c
  (assert-true (eqset? '(a b c) (domset r3))))

(defun idrel (s)
  (cond
	((null s) nil)
	(t (cons (build-pair (car s) (car s)) (idrel (cdr s))))))

(define-test 8.1d
  (assert-equal '((a a) (b b)) (idrel d1)))

(define-test 8.1e
  (assert-equal '((c c) (d d)) (idrel d2)))

(define-test 8.1f
  (assert-equal () (idrel f2)))

(defun reflexive? (rel)
  (eqset?
   (makeset (firsts rel))
   (makeset (seconds rel))))

(define-test 8.2a
  (assert-true (reflexive? r1)))

(define-test 8.2b
  (assert-true (reflexive? r2)))

(define-test 8.2c
  (assert-false (reflexive? r3)))

(defun member-rel (p rel)
  (cond
	((null rel) nil)
	((eqpair? (car rel) p) t)
	(t (member-rel p (cdr rel)))))

(defun intersect-rel (rel1 rel2)
  (cond
	((null rel1) ())
	((member-rel (car rel1) rel2) (cons (car rel1) (intersect (cdr rel1) rel2)))
	(t (intersect (cdr rel1) rel2))))

(defun symmetric? (rel)
  (eqset-rel? rel (revrel rel)))

(defun antisymmetric? (rel)
  (subset-rel? (intersect-rel rel (revrel rel)) (idrel (domset rel))))

(defun eqset-rel? (rel1 rel2)
  (cond
	((subset-rel? rel1 rel2) (subset-rel? rel1 rel2))
	(t nil)))

(defun subset-rel? (rel1 rel2)
  (cond
	((null rel1) t)
	((member-rel (car rel1) rel2) (subset-rel? (cdr rel1) rel2))
	(t nil)))

; this function (from the book) cannot work
; as we have intersect only for (flat) sets, not for
; relations
(defun asymmetric? (rel)
  (null? (intersect-rel rel (revrel rel))))

(define-test 8.3a
  (assert-false (symmetric? r1)))

(define-test 8.3b
  (assert-true (symmetric? r2)))

(define-test 8.3c
  (assert-true (symmetric? f2)))

(define-test 8.3d
  (assert-true (antisymmetric? r1)))

(define-test 8.3e
  (assert-true (antisymmetric? r2)))

(define-test 8.3f
  (assert-false (antisymmetric? r4)))

(defun Fapply (f a)
  (cond
	((null f) nil)
	((eq (first (car f)) a) (second (car f)))
	(t (Fapply (cdr f) a))))

(define-test 8.4a
  (assert-equal 1 (Fapply f1 x)))

(define-test 8.4b
  (assert-equal nil (Fapply f2 x)))

(define-test 8.4c
  (assert-equal 2 (Fapply f3 x)))

(defun Fcomp (f1 f2)
	(cond
	  ((null f2) nil)
	  ((member* (second (car f2)) (firsts f1))
	   (cons (build-pair (first (car f2)) (Fapply f1 (second (car f2)))) (Fcomp f1 (cdr f2))))
	  (t (Fcomp f1 (cdr f2)))))

(define-test 8.5a
  (assert-equal () (Fcomp f1 f4)))

(define-test 8.5b
  (assert-equal () (Fcomp f1 f3)))

(define-test 8.5c
  (assert-equal '((a $) (d $)) (Fcomp f4 f1)))

(define-test 8.5d
  (assert-equal '((b $)) (Fcomp f4 f3)))

(defun Rapply (rel x)
  (cond
	((null rel) nil)
	((eq (first (car rel)) x)
	 (cons (second (car rel)) (Rapply (cdr rel) x)))
	(t (Rapply (cdr rel) x))))

(define-test 8.6a
  (assert-equal '(1) (Rapply f1 x)))

(define-test 8.6b
  (assert-equal '(b a) (Rapply r1 x)))

(define-test 8.6c
  (assert-equal () (Rapply f2 x)))

(defun Rin (x set)
  (cond
	((null set) nil)
	(t (cons (build-pair x (car set)) (Rin x (cdr set))))))

(define-test 8.7a
  (assert-equal '((a a) (a b)) (Rin x d1)))

(define-test 8.7b
  (assert-equal '((a c) (a d)) (Rin x d2)))

(define-test 8.7c
  (assert-equal () (Rin x f2)))
	
(defun Rcomp (rel1 rel2)
  (cond
	((null rel1) ())
	(t (my-union
		(Rin
		 (first (car rel1))
		 (Rapply rel2 (second (car rel1))))
		(Rcomp (cdr rel1) rel2)))))

(define-test 8.8a
  (assert-equal '((A C) (A C) (B C)) (Rcomp r1 r3)))

(define-test 8.8b
  (assert-equal '((A 2) (A 1) (B 2)) (Rcomp r1 f1)))

(define-test 8.8c
  (assert-equal '((A B) (A B) (A A) (B B)) (Rcomp r1 r1)))

(defun transitive? (rel)
  (subset-rel? (Rcomp rel rel) rel))

(define-test 8.9a
  (assert-true (transitive? r1)))

(define-test 8.9b
  (assert-true (transitive? r3)))

(define-test 8.9c
  (assert-true (transitive? f1)))

(defun quasi-order? (rel)
  (and
   (reflexive? rel)
   (transitive? rel)))

(defun partial-order? (rel)
  (and
   (quasi-order? rel)
   (antisymmetric? rel)))

(defun equivalence? (rel)
  (and 
   (quasi-order? rel)
   (symmetric? rel)))



(run-tests)

