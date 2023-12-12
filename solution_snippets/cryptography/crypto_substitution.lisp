(defparameter *crypto-text*
  '("zj ze kljjls jf slapzi ezvlij pib kl jufwxuj p hffv jupi jf"
    "enlpo pib slafml pvv bfwkj"))

(defparameter *encipher-table* (make-hash-table))
(defparameter *decipher-table* (make-hash-table))

(defun make-substitution (code clear)
  (setf (gethash clear *encipher-table*) code)
  (setf (gethash code *decipher-table*) clear))

(defun undo-substitution (code clear)
  (setf (gethash clear *encipher-table*) nil)
  (setf (gethash code *decipher-table*) nil))

(defun clear ()
  (clrhash *encipher-table*)
  (clrhash *decipher-table*))

(defun decipher-string (string)
  (do* ((len (length string))
	(new-string (make-string len
				 :initial-element #\Space))
	(i 0 (1+ i)))
       ((equal i len) new-string)
    (let* ((char (aref string i))
	   (new-char
	    (gethash char *decipher-table*)))
      (when new-char
	(setf (aref new-string i) new-char)))))

(defun show-line (line)
  (format t "~%~A~%~A~%"
	  line
	  (decipher-string line)))

(defun show-text ()
  (format t "~&---------------")
  (dolist (line *crypto-text*)
    (show-line line))
  (format t "~&---------------"))

(defun get-first-char (x)
  (char-downcase
   (char (format nil "~A" x) 0)))

(defun read-letter ()
  (let ((obj (read)))
    (if (member obj '(end undo))
	obj
	(get-first-char obj))))

(defun sub-letter (code)
  (when (gethash code *decipher-table*)
    (format t "~&'~A' has already been" code)
    (format t " deciphered as '~A'!"
	    (gethash code *decipher-table*))
    (return-from sub-letter nil))
  (format t "What does '~A' decipher to? " code)
  (let ((clear (read-letter)))
    (cond ((not (characterp clear))
	   (format t "~&Invalid response."))
	  ((gethash clear *encipher-table*)
	   (format t "But '~A' already"
		   (gethash clear *encipher-table*))
	   (format t " deciphers as '~A'!"
		   clear))
	  (t (make-substitution code clear)))))

(defun undo-letter ()
  (format t "~&Undo which letter? ")
  (let* ((code (read-letter))
	 (clear (gethash code
			 *decipher-table*)))
    (cond ((not (characterp code))
	   (format t "~&Invalid input."))
	  (clear (undo-substitution code clear))
	  (t (format t
		     "~&But '~A' wasn't deciphered!"
		     code)))))

(defun solve ()
  (do ((resp nil))
      ((equal resp 'end))
    (show-text)
    (format t "~&Substitute which letter? ")
    (setf resp (read-letter))
    (cond ((characterp resp) (sub-letter resp))
	  ((equal resp 'undo) (undo-letter))
	  ((equal resp 'end) nil)
	  (t (format t "~&Invalid input.")))))
