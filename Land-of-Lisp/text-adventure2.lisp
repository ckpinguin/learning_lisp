;;My first text-adventure in lisp

;; *nodes* => list of (nodename (description))
(defparameter *nodes*
'((living-room (You are in the living room.
   a wizard is snoring loudly on the couch.))
  (garden (You are in a beautiful garden.
   there is a well in front of you.))
  (attic (you are in the attic.
   there is a giant welding torch in the corner.))))

;; *edges* => list of (nodename (target direction gateway))
(defparameter *edges*
'((living-room (garden west door)
               (attic upstairs ladder))
  (garden (living-room east door))
  (attic (living-room downstairs ladder))))

;; *objects* => list of objects
(defparameter *objects*
'(whiskey bucket frog chain))

;; *object-locations* => list of (objectname location)
;; This is a relation
;; This list gets amended by functions (appendings)
(defparameter *object-locations* '((whiskey living-room)
                                   (bucket living-room)
           			   (chain garden)
				   (frog garden)))                

;; *location* => current location
;; This variable's value gets changed by functions
(defparameter *location* 'living-room)

;; *allowed-commands* => list of commands allowed
(defparameter *allowed-commands* '(look walk pickup inventory))


;; Functions start here

(defun describe-location (location nodes)
  (cadr (assoc location nodes)))

(defun describe-path (edge)
  `(there is a ,(caddr edge) going ,(cadr edge) from here.))

(defun describe-paths (location edges)
  (apply #'append (mapcar #'describe-path (cdr (assoc location edges)))))

(defun objects-at (location objects object-locations)
  (flet ((at-loc-p (obj) ;; local predicate function
    (eq (cadr (assoc obj object-locations)) location )))
  (remove-if-not #'at-loc-p objects))) ;; call our local function

(defun describe-objects (location objects object-locations)
  (flet ((describe-obj (object)
    `(you see a ,object on the floor.)))
  (apply #'append (mapcar #'describe-obj (objects-at location objects
    object-locations)))))

(defun look ()
  (append (describe-location *location* *nodes*)
	  (describe-paths *location* *edges*)
          (describe-objects *location* *objects* *object-locations*)))
          
(defun walk (direction)
  (let ((next (find direction ;; local variable 'next'
                (cdr (assoc *location* *edges*)) :key #'cadr)))
        (if next (progn (setf *location* (car next)) ;; check if next is set
                 (look))
        '(you cannot go that way.))))

(defun pickup (object)
  (cond ((member object (objects-at *location* *objects* *object-locations*))
         (push (list object 'body) *object-locations*)
	 `(you are now carrying the ,object))
        (t '(you cannot pick up that.))))

(defun inventory ()
  (cons 'items- (objects-at 'body *objects* *object-locations*)))

;; most simple repl ;-)
;;(defun game-repl ()
;;  (loop (print (eval (read)))))

(defun game-repl ()
  (let ((cmd (game-read)))
    (unless (eq (car cmd) 'quit)
      (game-print (game-eval cmd))
      (game-repl))))

(defun game-read ()
  (let ((cmd (read-from-string (concatenate 'string "("
	                       (read-line) ")"))))
  (flet ((quote-it (x) (list 'quote x)))
  (cons (car cmd) (mapcar #'quote-it (cdr cmd))))))

(defun game-eval (strexpr)
  (if (member (car strexpr) *allowed-commands*)
    (eval strexpr)
  '(i do not know that command.)))

(defun tweak-text (lst caps lit)
  (when lst (let ((item (car lst)) (rest (cdr lst)))
    (cond ((eq item #\space) (cons item (tweak-text rest caps lit)))
          ((member item '(#\! #\? #\.)) (cons item (tweak-text rest t lit)))
          ((eq item #\") (tweak-text rest caps (not lit)))
	  (lit (cons item (tweak-text rest nil lit)))
	  ((or caps lit) (cons (char-upcase item) (tweak-text rest nil lit)))
          (t (cons (char-downcase item) (tweak-text rest nil nil)))))))

(defun game-print (lst)
  (princ (coerce (tweak-text (coerce (string-trim "() "
    (prin1-to-string lst)) 'list) t nil) 'string))
  (fresh-line))


;; adventure DSL part
(defun have (object)
  (member object (cdr (inventory))))

(defmacro game-action (command subj obj place &body body)
  `(progn (defun ,command (subject object)
	    (if (and (eq *location* ',place)
		     (eq subject ',subj)
		     (eq object ',obj)
		     (have ',subj))
		,@body
		'(i cant ,command like that.)))
	  (pushnew ',command *allowed-commands*)))

(defparameter *chain-welded* nil)

(game-action weld chain bucket attic
  (if (and (have 'bucket) (not *chain-welded*))
      (progn (setf *chain-welded* 't)
	     '(the chain is now securely welded to the bucket.))
      '(you do not have a bucket.)))

(defparameter *bucket-filled* nil)

(game-action dunk bucket well garden
  (if *chain-welded*
      (progn (setf *bucket-filled* 't)
	     '(the bucket is now full of water))
      '(the water level is too low to reach.)))

(game-action splash bucket wizard living-room
  (cond ((not *bucket-filled*) '(the bucket has nothing in it.))
	((have 'frog) '(the wizard awakens and seens that you stole his frog.
			he is so upset he banishes you to the 
			netherworlds- you lose! the end.))
	(t '(the wizard awakens from his slumber and greets you warmly.
	     he hands you the magic low-carb donut- you win! the end.))))

