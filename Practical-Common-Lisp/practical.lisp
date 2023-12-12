(in-package :ch.codehome.practical)

(defmacro with-gensyms ((&rest names) &body body)
  `(let ,(loop for n in names collect `(,n (make-symbol ,(string n))))
     ,@body))

(defmacro once-only ((&rest names) &body body)
  (let ((gensyms (loop for n in names collect (gensym))))
    `(let (,@(loop for g in gensyms collect `(,g (gensym))))
	   `(let (,,@(loop for g in gensyms for n in names collect ``(,,g ,,n)))
		  ,(let (,@(loop for n in names for g in gensyms collect `(,n ,g)))
			 ,@body)))))

(defun nshuffle-vector (vector)
  ; Fisher / Yates (& Knuth) algorithm for shuffling a vector
  (loop for idx downfrom (1- (length vector)) to 1
	   for other = (random (1+ idx))
	   do (unless (= idx other)
			(rotatef (aref vector idx) (aref vector other))))
  vector)

(defun shuffle-vector (vector)
  (nshuffle-vector (copy-seq vector)))

(defun start-of-file (file max-chars)
  (with-open-file (in file)
	(let* ((length (min (file-length in) max-chars))
		   (text (make-string length))
		   (read (read-sequence text in)))
	  (if (< read length)
		  (subseq text 0 read)
		  text))))

(defun spliceable (value)
  (if value (list value)))

(defmacro ppme (form &environment env)
  (progn
    (write (macroexpand-1 form env)
           :length nil
           :level nil
           :circle nil
           :pretty t
           :gensym nil
           :right-margin 83
           :case :downcase)
    nil))
