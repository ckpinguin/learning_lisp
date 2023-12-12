(defparameter *hist-array* nil)
(defparameter *total-points* 0)


(defun new-histogram (bins)
  (setf *total-points* 0)
  (setf *hist-array*
	(make-array bins :initial-element 0))
  t)

(defun record-value (v)
  (incf *total-points*)
  (if (and (>= v 0)
	   (< v (length *hist-array*)))
      (incf (aref *hist-array* v))
      (error "Value ~S out of bounds." v)))

(defun print-hist-line (i)
  (let ((val (aref *hist-array* i)))
    (format t "~&~2D [~3D] " i val)
    (dotimes (j val)
      (format t "*"))))

(defun print-histogram ()
  (dotimes (i (length *hist-array*))
    (print-hist-line i))
  (format t "~&    ~3D total" *total-points*))

