(defparameter *loc* 'garden)

(setq rooms
  '((living-room 
      (north garden)
      (up upstairs)
      (down downstairs)
      (east toilet))
    (upstairs 
      (down living-room)
      (up sleeping-room))
    (downstairs
      (up living-room)
      (down cellar))
    (cellar
      (up downstairs))
    (garden
      (south living-room)
      (east outside))
    (sleeping-room
      (downstairs living-room)
      (west outside)
      (south closet))) )

(defun choices (room)
  (rest (assoc room rooms)))

(defun look (direction room)
  (car (rest (assoc direction (choices room)))))

(defun set-loc (place)
  "Moves Robbie to PLACE by setting the
   global variable *loc*"
   (setf *loc* place))

(defun how-many-choices ()
  (length (choices *loc*)))

(defun upstairsp (place)
  (member place '(sleeping-room closet)) ) 

(defun downstairsp (place)
  (member place '(cellar)))

(defun onstairsp (place)
  (member place '(downstairs upstairs)))

(defun where ()
  (let ((up-or-down (cond ((upstairsp *loc*) 'upstairs)
                          ((downstairsp *loc*) 'downstairs)
                           (t 'on-the-ground))))
    (list 'Robbie 'is up-or-down 'in 'the *loc*)))

(defun move (direction)
  (let ((way (look direction loc) ))
    (and way
         (set-loc way)) ))

