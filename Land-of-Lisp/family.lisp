(setf family
      '((colin nil nil)
	(deirdre nil nil)
	(arthur nil nil)
	(kate nil nil)
	(frank nil nil)
	(linda nil nil)
	(suzanne colin deirdre)
	(bruce arthur kate)
	(charles arthur kate)
	(david arthur kate)
	(ellen arthur kate)
	(george frank linda)
	(hillary frank linda)
	(andre nil nil)
	(tamara bruce suzanne)
	(vincent bruce suzanne)
	(wanda nil nil)
	(ivan george ellen)
	(julie george ellen)
	(marie george ellen)
	(nigel andre hillary)
	(frederick nil tamara)
	(zelda vincent wanda)
	(joshua ivan wanda)
	(quentin nil nil)
	(robert quentin julie)
	(olivia nigel marie)
	(peter nigel marie)
	(erica nil nil)
	(yvette robert zelda)
	(diane peter erica)))

(defun father  (x) (second (assoc x family)))

(defun mother  (x) (third  (assoc x family)))

(defun parents (x) 
  (union (and (father x) (list (father x)))
	 (and (mother x) (list (mother x)))))

(defun children (parent)
  (and parent
       (mapcar #'first 
	       (remove-if-not
		#'(lambda (entry)
		    (member parent (rest entry)))
		family))))

(defun siblings (x)
  (set-difference (union (children (father x))
			 (children (mother x)))
		  (list x)))

(defun mapunion (fn x)
  (reduce #'union (mapcar fn x)))

(defun grandparents (x)
  (mapunion #'parents (parents x)))

(defun cousins (x)
  (mapunion #'children
	    (mapunion #'siblings (parents x))))

(defun descended-from (p1 p2)
  (cond ((null p1) nil)
	((member p2 (parents p1)) t)
        (t (or (descended-from
		(father p1) p2)
	       (descended-from
		(mother p1) p2)))))

(defun ancestors (x)
  (cond ((null x) nil)
	(t (union
	    (parents x)
	    (union (ancestors (father x))
		   (ancestors (mother x)))))))

(defun generation-gap (p1 p2)
  (g-gap-helper p1 p2 0))

(defun g-gap-helper (x y n)
  (cond ((null x) nil)
	((equal x y) n)
	(t (or (g-gap-helper
		(father x) y (1+ n))
	       (g-gap-helper
		(mother x) y (1+ n))))))
