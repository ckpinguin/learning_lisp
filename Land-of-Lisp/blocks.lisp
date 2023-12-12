(setq database '((b1 shape cube)
                 (b1 color blue)
                 (b1 size big)
                 (b1 supports nil)
                 (b1 left-of nil)
                 (b2 shape brick)
                 (b2 color red)
                 (b2 size small)
                 (b2 supports b1)
                 (b2 left-of b3)
                 (b3 shape cone)
                 (b3 color green)
                 (b3 size tiny)
                 (b3 supports b2)
                 (b3 left-of nil)
                 (b4 shape cube)
                 (b4 color red)
                 (b4 size small)
                 (b4 supports b2)
                 (b4 left-of b3)))

(defun match-element (e q)
  (or (equal e q)
      (eq q '?)))

(defun match-triple (x pat)
  (every #'match-element
         x
         pat))

(defun fetch (pat)
  (remove-if-not
    #'(lambda (x) (match-triple x pat))
    database))

(defun color-pattern (block)
  (list block 'color '?))

(defun supporters (block)
  (mapcar #'first
          (fetch (list '? 'supports block))))

(defun supp-by-cube (block)
  (member 'cube
    (mapcar 
      #'(lambda (b) (third (first (fetch
                        (list b 'shape '?)))))
      (supporters block))))

(defun desc1 (block)
  (fetch (list block '? '?)))

(defun desc2 (block)
  (mapcar #'rest (desc1 block)))

(defun description (block)
  (reduce #'append (desc2 block)))

