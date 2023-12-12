(load "lisp-unit.lisp")
(use-package :lisp-unit)


(defun rember-f_try1 (test? a l)
  (cond
	((null l) nil)
	((funcall test? (car l) a) (cdr l))
	(t (cons (car l) (rember-f_try1 test? a (cdr l))))))

(defun eq?-c (a)
	  (lambda (x)
		(eq x a)))

(defparameter eq?-salad (eq?-c 'salad))

(defun rember-f (test?)
  (lambda (a l)
	(cond
	  ((null l) nil)
	  ((funcall test? (car l) a) (cdr l))
	  (t (cons (car l)
			   (funcall (rember-f test?) a (cdr l)))))))

(defparameter rember-eq? (rember-f #'eq))

(defun insertL-f (test?)
  (lambda (new old lat)
	(cond
	  ((null lat) nil)
	  ((funcall test? (car lat) old) 
	   (cons new (cons old (cdr lat))))
	  (t (cons (car lat)
			   (funcall (insertL-f test?) new old (cdr lat)))))))

(defun insertR-f (test?)
  (lambda (new old lat)
	(cond
	  ((null lat) nil)
	  ((funcall test? (car lat) old)
	   (cons old (cons new (cdr lat))))
	  (t (cons (car lat)
			   (funcall (insertR-f test?) new old (cdr lat)))))))


(defun insert-g_try1 (test? where)
  (lambda (new old lat)
	(cond
	  ((null lat) nil)
	  ((funcall test? (car lat) old)
	   (cond
		 ((eq 'left where) (cons new lat))
		 (t (cons old (cons new (cdr lat))))))
	  (t (cons (car lat)
			   (funcall (insert-g_try1 test? where) new old (cdr lat)))))))

(defun seqL (new old l)
  (cons new (cons old l)))

(defun seqR (new old l)
  (cons old (cons new l)))

(defun insert-g (test? conser)
  (lambda (new old lat)
	(cond
	  ((null lat) nil)
	  ((funcall test? (car lat) old)
	   (funcall conser new old (cdr lat)))
	  (t (cons (car lat) 
			   (funcall (insert-g test? conser) new old (cdr lat)))))))

(defparameter insertL (insert-g #'eq #'seqL))
(defparameter insertR (insert-g #'eq #'seqR))

(defparameter insertL2
  (insert-g #'eq (lambda (new old l)
				   (cons new (cons old l)))))

(defun my-subst (new old l)
  (cond
	((null l) ())
	((eq (car l) old)
	 (cons new (cdr l)))
	(t (cons (car l) 
			 (my-subst new old (cdr l))))))

(defparameter subst2
  (insert-g #'eq (lambda (new old l)
				   (cons new l))))

(defun seqS (new old l)
  (cons new l))

(defparameter subst3
  (insert-g #'eq #'seqS))

(defun ^ (n m)
  (cond
	((zerop m) 1)
	(t (* n (^ n (1- m))))))

(defun operator (aexp)
  (car aexp))

(defun 1st-sub-exp (aexp)
  (car (cdr aexp)))

(defun 2nd-sub-exp (aexp)
  (car (cdr (cdr aexp))))

(defun 3rd-sub-exp (aexp)
  (car (cdr (cdr (cdr aexp)))))

(defun 4th-sub-exp (aexp)
  (car (cdr (cdr (cdr (cdr aexp))))))

(defun atom-to-function (x)
  (cond
	((eq x (quote +)) #'+)
	((eq x (quote *)) #'*)
	((eq x (quote ^)) #'^)))

(defun value (aexp)
  (cond
	((numberp aexp) aexp)
	(t (funcall (atom-to-function (operator aexp))
				(value (1st-sub-exp aexp))
				(value (2nd-sub-exp aexp))))))

(defun set-f?_try1 (logical? const)
  (lambda (set1 set2)
	(cond
	  ((null set1) const)
	  (t (funcall logical? 
				  (member (car set1) set2)
				  (funcall (set-f?_try1 logical? const)) (cdr set1) set2)))))

; We can use literal parameters, because or/and are macros
(defparameter intersect2? (set-f?_try1 'or nil))
(defparameter subset2? (set-f?_try1 'and t))

; Alternative: fake functions for the and/or macros
(defun and-prime (x set1 set2)
  (and x (funcall subset3? (cdr set1) set2)))

; BUT BEWARE! This is not loose coupling, when
; we use a not-yet defined lambda in a recursive
; helper-function for that same lambda (just my five cents).
(defun or-prime (x set1 set2)
  (or x (funcall intersect3? (cdr set1) set2)))

(defun set-f? (logical? const)
  (lambda (set1 set2)
	(cond
	  ((null set1) const)
	  (t (funcall logical? (member (car set1) set2)
				  set1
				  set2)))))

(defparameter intersect3? 
  (set-f? #'or-prime nil))
(defparameter subset3? 
  (set-f? #'and-prime t))

(defun multirember (a l)
  (cond
	((null l) nil)
	((eq (car l) a) (multirember a (cdr l)))
	(t (cons (car l) (multirember a (cdr l))))))

(defun Mrember-curry_try1 (l)
  (multirember (quote curry) l))

(defun Mrember-curry_try2 (l)
  (cond
	((null l) nil)
	((eq (car l) (quote curry)) (Mrember-curry (cdr l)))
	(t (cons (car l) (Mrember-curry (cdr l))))))

(defun curry-maker (future)
  (lambda (l)
	(cond
	  ((null l) ())
	  ((eq (car l) (quote curry))
	   (funcall (curry-maker future) (cdr l)))
	  (t (cons (car l) (funcall (curry-maker future) (cdr l)))))))

(defparameter Mrember-curry_try3 (curry-maker 0))

(defun function-maker_try1 (future)
  (lambda (l)
	(cond
	  ((null l) ())
	  ((eq (car l) (quote curry))
	   (funcall (funcall future future) (cdr l)))
	  (t (cons (car l) 
			   (funcall (funcall future future) (cdr l)))))))

(defun function-maker_wrapped1 (future)
  (lambda (l)
	(cond
	  ((null l) ())
	  ((eq (car l) (quote curry)) 
	   (funcall (lambda (arg)
				  (funcall (funcall future future) arg))
				(cdr l)))
	  (t (cons (car l)
			   (funcall (lambda (arg)
						  (funcall (funcall future future) arg))
						(cdr l)))))))

(defun function-maker_wrapped2 (future)
  ((lambda (recfun)
	 (lambda (l)
	   (cond
		 ((null l) nil)
		 ((eq (car l) (quote curry))
		  (funcall recfun (cdr l)))
		 (t (cons (car l)
				  (funcall recfun (cdr l)))))))
   (lambda (arg)
	 (funcall (funcall future future) arg))))

(defun function-maker (future)
  (M (lambda (arg)
	   (funcall (funcall future future) arg))))

(defun M (recfun)
  (lambda (l)
	(cond
	  ((null l) nil)
	  ((eq (car l) (quote curry))
	   (funcall recfun (cdr l)))
	  (t (cons (car l)
			   (funcall recfun (cdr l)))))))

(defun my_Y (M)
  ((lambda (future)
	 (funcall M (lambda (arg)
		  (funcall (funcall future future) arg))))
   (lambda (future)
	 (funcall M (lambda (arg)
		  (funcall (funcall future future) arg))))))

(defparameter Mrember-curry (my_Y #'M))

; Y-combinator from wikipedia:
(defun Y (f)
  ((lambda (x)
     (funcall f (lambda (v)
                  (funcall (funcall x x) v))))
   ; this is the "x" argument (itself a lambda)
   (lambda (x)
     (funcall f (lambda (v)
                  (funcall (funcall x x) v))))))

(defun L (recfun)
  (lambda (l)
	(cond
	  ((null l) 0)
	  (t (1+ (funcall recfun (cdr l)))))))

(defparameter my-length_try1 (Y #'L))

(defun my-length ()
  (Y
   (lambda (recfun)
	 (lambda (l)
	   (cond
		 ((null l) 0)
		 (t (1+ (funcall recfun (cdr l)))))))))

(defun my-length_no_Y ()
  ((lambda (M)
	 ((lambda (future)
		(funcall M (lambda (arg)
			 (funcall (funcall future future) arg))))
	  (lambda (future)
		(funcall M (lambda (arg)
			 (funcall (funcall future future) arg))))))
   (lambda (recfun)
	 (lambda (l)
	   (cond
		 ((null l) 0)
		 (t (1+ (funcall recfun (cdr l)))))))))

(defun my-map (fun l)
  (cond
	((null l) ())
	(t (cons (funcall fun (car l)) (my-map fun (cdr l))))))

(defun firsts (l)
  (my-map #'car l))

(defun seconds (l)
  (my-map (lambda (x) (car (cdr x))) l))

(defun assq-sf (a l sk fk)
  (cond
	((null l) (funcall fk a))
	((eq (first (car l)) a) (funcall sk (car l)))
	(t (assq-sf a (cdr l) sk fk))))

(defun build-pair (a1 a2)
  (cons a1 (cons a2 ())))

(defparameter a 'apple)
(defparameter b1 ())
(defparameter b2 '((apple 1) (plum 2)))
(defparameter b3 '((peach 3)))
(defparameter sk (lambda (p)
				   (build-pair (first p) (1+ (second p)))))
(defparameter fk (lambda (name)
				   (cons name
						 (quote (not-in-list)))))

(define-test 9.2a
  (assert-equal '(apple not-in-list) (assq-sf a b1 sk fk)))

(define-test 9.2b
  (assert-equal '(apple 2) (assq-sf a b2 sk fk)))

(define-test 9.2c
  (assert-equal '(apple not-in-list) (assq-sf a b3 sk fk)))

(defun Y2 (M)
  (funcall (lambda (future)
	 (funcall M (lambda (arg1 arg2)
		  (funcall (funcall future future) arg1 arg2))))
   (lambda (future)
	 (funcall M (lambda (arg1 arg2)
		  (funcall (funcall future future) arg1 arg2))))))

(defun rempickY2 ()
  (Y2 
   (lambda (recfun)
	 (lambda (n lat)
	   (cond
		 ((null lat) nil)
		 ((zerop (1- n)) (funcall recfun (1- n) (cdr lat)))
		 (t (cons (car lat) (funcall recfun (1- n) (cdr lat)))))))))

(define-test 9.3a
  (assert-equal '(one three) (funcall (rempickY2) 2 '(one two three))))

(defun pickY2 ()
  (Y2
   (lambda (recfun)
	 (lambda (n lat)
	   (cond
		 ((null lat) nil)
		 ((zerop (1- n)) (car lat))
		 (t (funcall recfun (1- n) (cdr lat))))))))

(define-test 9.3b
  (assert-equal 'two (funcall (pickY2) 2 '(one two three))))

(defun member-Y (a l)
  (funcall (Y (lambda (recfun) ; the whole stuff is arg. to Y
				; this is recfun (anon. function with 1 arg.):
				(lambda (l) 
				  (cond
					((null l) nil)
					(t (or
						(eq (car l) a)
						(funcall recfun (cdr l))))))))
		   l)) ; l ist arg. to what Y returns (i.e. the above lambda
										; that has been "made recursive")

(define-test 9.4a
  (assert-true (member-Y 'a '(b c a))))

(defun rember-Y (a lat)
  (funcall (Y2 (lambda (recfun)
				(lambda (a lat)
				  (cond
					((null lat) nil)
					((eq a (car lat)) (cdr lat))
					(t (cons (car lat) (funcall recfun a (cdr lat))))))))
		   a lat))

(define-test 9.4b
  (assert-equal '(b c) (rember-Y 'a '(b c a))))

; Continuation instead of stack variable
(defun multisubst-k (new old lat k)
  (cond
	((null lat) (funcall k nil)) ; here is the problem located (probably)
	((eq (car lat) old)
	 (multisubst-k new old (cdr lat)
				   (lambda (d)
					 (funcall k (cons new d)))))
	(t (multisubst-k new old (cdr lat)
					 (lambda (d)
					   (funcall k (cons (car lat) d)))))))

(defparameter k (lambda (x) x))
(defparameter new 'y)
(defparameter old 'x)
(defparameter lat '(u v x x y z x))

; We get a stack overflow (it works only the first time...sigh...
;(define-test 9.5a
;  (assert-equal '(u v y y y z y) (multisubst-k new old lat k)))

; Even this simple function overflows the stack, so the problem
; is maybe Clisp-special?
(defun length-k (l k)
  (cond
	((null l) (funcall k 0))
	(t (length-k (cdr l) 
					 (lambda (d)
					   (funcall k (1+ d)))))))

; Thunks stuff
(defun or-func (or1 or2)
  (or
   (funcall or1)
   (funcall or2)))

(defparameter l1 'a)

(define-test 9.8a
  (assert-true (or-func (lambda () (null l1)) (lambda () (atom l1)))))

(defun set-f2? (logical? const)
  (lambda (set1 set2)
	(cond
	  ((null set1) const)
	  (t (funcall logical? (lambda () (member (car set1) set2))
				  (lambda () set1)
				  )))))

(defparameter intersect4? 
  (set-f2? #'or-func nil))
(defparameter subset4? 
  (set-f2? #'or-func t))


; Streams stuff
(defparameter first$ #'first) ; didn't find yet a better way to alias a function

(defun second$ (str)
  (funcall (second str)))

(defparameter s (build-pair 1 (lambda () 2)))

; Believe it or not, we just built lazy evaluation with streams here:
(defun str-maker (next n)
  (build-pair n (lambda () (str-maker next (funcall next n)))))

; Stream of all integers (access them with second$)
(defparameter int (str-maker #'1+ 0))

; Stream of all even integers
(defparameter even (str-maker (lambda (n) (+ 2 n)) 0))
; Like this, we could also make a stream of all prime numbers, 
; evaluated the lazy way (an efficient algorithm provided).

(defun frontier (str n)
  (cond
	((zerop n) nil)
	(t (cons (first str) (frontier (second$ str) (1- n))))))

(defparameter odd (str-maker (lambda (n) (+ 2 n)) 1))

(defun Q (str n)
  (cond
	((zerop (rem (first str) n))
	 (Q (second$ str) n)) ; no pair-building when divisable by an other integer
	(t (build-pair (first str)
				   (lambda () (Q (second$ str) n))))))

(defun P (str)
  (build-pair (first str) (lambda () (P (Q str (first str))))))

; And here we go for the prime numbers (using naiive division probing)
; => don't use it to get the highest known prime-number of the world
(define-test 9.10a
  (assert-equal '(2 3 5 7 11 13 17 19 23 29) (frontier (P (second$ (second$ int))) 10))) ; starting with 2 of course


(run-tests)