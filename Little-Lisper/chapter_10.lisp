(load "lisp-unit.lisp")
(use-package :lisp-unit)

(defun build (s l)
  (cons s (cons l ())))

(defparameter e1 (build '(appetizer entree beverage) '(pate boeuf vin)))
(defparameter e2 (build '(beverage dessert) '((food is) (number one with us))))
(defparameter t1 (cons e1 (cons e2 nil)))

; Building an interpreter

(defun lookup-in-entry (name entry entry-f)
  (lookup-in-entry-help
   name
   (first entry)
   (second entry)
   entry-f))

(defun lookup-in-entry-help (name names values entry-f)
  (cond
	((null names) (funcall entry-f name))
	((eq (car names) name) (car values))
	(t (lookup-in-entry-help
		name
		(cdr names)
		(cdr values)
		entry-f))))

(defun lookup-in-table (name table table-f)
  (cond
	((null table) (funcall table-f name))
	(t (lookup-in-entry
		name
		(car table) 
		(lambda (name)
		  (lookup-in-table
		   name
		   (cdr table)
		   table-f))))))

; actions are functions
(defun expression-to-action (e)
  (cond
	((atom e) (atom-to-action e))
	(t (list-to-action e))))

(defun atom-to-action (e)
  (cond
	((numberp e) #'*self-evaluating)
	(t #'*identifier)))

(defun list-to-action (e)
  (cond
	((atom (car e))
	 (cond
	   ((eq (car e) (quote quote)) #'*quote)
	   ((eq (car e) (quote lambda)) #'*lambda)
	   ((eq (car e) (quote cond)) #'*cond)
	   (t #'*application)))
	(t #'*application)))

; value _is_ the core of the interpreter
(defun value (e)
  (meaning e (quote ()))) ; (quote ()) is the empty table

(defun meaning (e table)
  (funcall (expression-to-action e) e table))

; Here come the definitions of the action functions
(defun *self-evaluating (e table)
  e)

(defun *identifier (e table)
  (lookup-in-table e table #'initial-table))

(defun initial-table (name)
  (cond
	((eq name (quote t)) t)
	((eq name (quote nil)) nil)
	(t (build
		(quote primitive)
		name))))

(defun initial-table2 ()
  ((lambda (add1)
	 (lambda (name)
	   (cond
		 ((eq name (quote t)) t)
		 ((eq name (quote nil)) nil)
		 ((eq name (quote add1)) add1)
		 (t (build (quote primitive) name)))))
   (build (quote primitive) add1)))
  
(defun *quote (e table)
  (text-of-quotation e))

(defun *lambda (e table)
  (build 
   (quote non-primitive)
   (cons table (cdr e))))

(defun *lambda2 (e table)
  (build
   (quote non-primitive)
   (lambda (vals)
	 (meaning (body-of e)
			  (extend-table
			   (new-entry (formals-of e) vals)
			   table)))))

(defun *cond (e table)
  (evcon (cond-lines e) table))

(defun cond-lines (e)
  (cdr e))

(defun text-of-quotation (e)
  (second e))

(defun table-of (e)
  (first e))

(defun formals-of (e)
  (second e))

(defun body-of (e)
  (third e))

(defun evcon (lines table)
  (cond
	((meaning (question-of (car lines)) table)
	 (meaning (answer-of (car lines)) table))
	(t (evcon (cdr lines) table))))

(defun question-of (e)
  (first e))

(defun answer-of (e)
  (second e))

(defparameter e1 '(cond (coffee klatsch) (t party)))
(defparameter table1 '(((coffee) (t)) ((klatsch party) (5 (6)))))

(define-test a1
  (assert-equal 5 (*cond e1 table1)))

(defun evlis (args table)
  (cond
	((null args) (quote nil))
	(t (cons (meaning (car args) table) 
			 (evlis (cdr args) table)))))

(defun *application (e table)
  (my-apply
   (meaning (function-of e) table)
   (evlis (arguments-of e) table)))

(defun function-of (e)
  (car e))

(defun arguments-of (e)
  (cdr e))

(defun primitive? (l)
  (eq (first l) (quote primitive)))

(defun non-primitive? (l)
  (eq (first l) (quote non-primitive)))

(defun my-apply (fun vals)
  (cond
	((primitive? fun)
	 (apply-primitive (second fun) vals))
	((non-primitive? fun)
	 (apply-closure
	  (second fun) vals))))

(defun apply-primitive (name vals)
  (cond
	((eq name (quote car))
	 (car (first vals)))
	((eq name (quote cdr))
	 (cdr (first vals)))
	((eq name (quote cons))
	 (cons (first vals) (second vals)))
	((eq name (quote eq?))
	 (eq (first vals) (second vals)))
	((eq name (quote atom?))
	 (atom (first vals)))
	((eq name (quote not))
	 (not (first vals)))
	((eq name (quote null?))
	 (null (first vals)))
	((eq name (quote number?))
	 (numberp (first vals)))
	((eq name (quote zero?))
	 (zerop (first vals)))
	((eq name (quote add1))
	 (1+ (first vals)))
	((eq name (quote sub1))
	 (1- (first vals)))))

; This is the part that extends the table
(defun apply-closure (closure vals)
  (meaning (body-of closure)
		   (extend-table
			(new-entry
			 (formals-of closure) vals)
			(table-of closure))))

(defun pick (n lat)
  (cond
	((null lat) nil)
	((zerop (1- n)) (car lat))
	(t (pick (1- n) (cdr lat)))))

(defun index (a lat &optional (n 0 n-supplied-p))
  (cond
	((null lat) nil)
	((eq (car lat) a) n)
	(t (index a (cdr lat) (1+ n)))))

(defun extend-table (entry table)
  (cons entry table))

(defun extend-table2 (entry table)
  (lambda (name)
	(cond
	  ((member name (first entry))
	   (pick (index name (first entry))
			 (second entry)))
	  (t (funcall table name)))))

(defun new-entry (s l)
  (build s l))

(defun my-cons (u v)
  (lambda (b)
	(cond
	  (b u)
	  (t v))))

(defparameter e1 '((lambda (x)
					 (cond
					   ((atom? x) (quote done))
					   ((null? x) (quote almost))
					   (t (quote never))))
				   (quote (cons nil ()))))
(defparameter e2 '(((lambda (x y)
					  (lambda (u)
						(cond
						  (u x)
						  (t y))))
					1 ())
				   nil))
(defparameter e3 '((lambda (x)
					((lambda (x)
					   (add1 x))
					 (add1 4)))
				  6))
(defparameter e4 '(3 (quote a) (quote b)))
(defparameter e5 '(lambda (lat) (cons (quote lat) lat)))
(defparameter e6 '(lambda (lat (lyst)) a (quote b)))

(defun *lambda? (e)
  ())

; We could replace all cond's by this if-form
; (to be used for e1 and e2)
(defun *if (e table)
  (if (meaning (test-pt e) table)
	  (meaning (then-pt e) table)
	  (meaning (else-pt e) table)))


(run-tests)