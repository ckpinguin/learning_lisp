(defun throw-die ()
  (+ 1 (random 6)) )

(defun throw-dice ()
   (list (throw-die) (throw-die)))

(defun snake-eyes-p (throw)
  (equal throw '(1 1)))

(defun boxcars-p (throw)
  (equal throw '(6 6)))

(defun throw-value (throw)
  (+ (first throw) (second throw)))

(defun instant-win-p (throw)
  (member (throw-value throw) '(7 11)))

(defun instant-loss-p (throw)
  (member (throw-value throw) '(2 3 12)))

(defun say-throw (throw)
  (cond ((snake-eyes-p throw) 'snake-eyes)
        ((boxcars-p throw) 'boxcars)
        (t (throw-value throw))))

;; The game called craps
(defun craps ()
  (let ((throw (throw-dice)))
    (append (list 'throw (first throw)
                  'and (second throw)
                  '-- (say-throw throw) '--)
            (cond ((instant-win-p throw) '(you win!))
                  ((instant-loss-p throw) '(you lose!))
                  (t (list 'your 'point 'is (throw-value throw)))))))

(defun try-for-point (point)
  (let* ((throw (throw-dice))
         (val (throw-value throw)))
    (append (list 'throw (first throw)
                  'and (second throw)
                  '-- (say-throw throw) '--)
            (cond ((eq val point) '(you win!))
                  ((eq val 7) '(you lose!))
                  (t '(throw again?))))))
