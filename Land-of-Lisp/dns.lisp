(defun complement-base (base)
  (second 
   (assoc base '((a t) (t a) (g c) (c g)))))

(defun complement-strand (strand)
  (do ((s strand (rest s))
       (result nil
	       (cons (complement-base (first s))
		     result)))
      ((null s) (reverse result))))

(defun make-double (strand)
  (do ((s strand (rest s))
       (result nil
	       (cons (list (first s)
			   (complement-base (first s)))
		     result)))
      ((null s) (reverse result))))

(defun count-bases (dna)
  (let ((acnt 0) (tcnt 0) (gcnt 0) (ccnt 0))
    (labels ((count-one-base (base)
	       (cond ((equal base 'a) (incf acnt))
		     ((equal base 't) (incf tcnt))
		     ((equal base 'g) (incf gcnt))
		     ((equal base 'c) (incf ccnt)))))
      (dolist (element dna)
	(cond ((atom element) (count-one-base element))
	      (t (count-one-base (first element))
		 (count-one-base (second element)))))
      (list (list 'a acnt)
	    (list 't tcnt)
	    (list 'g gcnt)
	    (list 'c ccnt)))))

(defun prefixp (strand1 strand2)
  (do ((s1 strand1 (rest s1))
       (s2 strand2 (rest s2)))
      ((null s1) t)
    (unless (equal (first s1) (first s2))
      (return nil))))

(defun appearsp (strand1 strand2)
  (do ((s2 strand2 (rest s2)))
      ((null s2) nil)
    (if (prefixp strand1 s2)
	(return t))))

(defun coverp (strand1 strand2)
  (do* ((len1 (length strand1))
	(s2 strand2 (nthcdr len1 s2)))
       ((null s2) t)
    (unless (prefixp strand1 s2)
      (return nil))))

(defun prefix (n strand)
  (do ((i 0 (+ i 1))
       (res nil (cons (nth i strand) res)))
      ((equal i n) (reverse res))))

(defun kernel (strand)
  (do ((i 1 (+ i 1)))
      ((coverp (prefix i strand) strand)
       (prefix i strand))))

(defun draw-dna (strand)
  (let ((n (length strand)))
    (draw-string n "-----")
    (draw-string n "  !  ")
    (draw-bases strand)
    (draw-string n "  .  ")
    (draw-string n "  .  ")
    (draw-bases (complement-strand strand))
    (draw-string n "  !  ")
    (draw-string n "-----")))

(defun draw-string (cnt string)
  (format t "~&")
  (dotimes (i cnt)
    (format t "~A" string)))

(defun draw-bases (strand)
  (format t "~&")
  (dolist (base strand)
    (format t "  ~A  " base)))