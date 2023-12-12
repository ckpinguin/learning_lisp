(defun space-over (n)
  (cond ((< n 0) (format t "Error!"))
	((zerop n) nil)
	(t (format t " ")
	   (space-over (- n 1)))))

(defun plot-one-point (plotting-string y-val)
  (space-over y-val)
  (format t "~A~%" plotting-string))
 
(defun plot-points (plotting-string y-list)
  (mapcar #'(lambda (y)
	      (plot-one-point plotting-string y))
	  y-list))

(defun generate (m n)
  (cond ((equal m n) (list n))
	(t (cons m (generate (+ m 1) n)))))

(defun make-graph ()
  (let* ((func
	  (prompt-for "Function to graph? "))
	 (start
	  (prompt-for "Starting x value? "))
	 (end (prompt-for "Ending x value? "))
	 (plotting-string
	  (prompt-for "Plotting string? ")))
    (plot-points plotting-string
		 (mapcar func (generate start end)))
    t))

(defun prompt-for (prompt-string)
  (format t "~A" prompt-string)
  (read))

(defun square (x)
  (* x x))