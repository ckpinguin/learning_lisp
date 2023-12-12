(defstruct node
  name
  question
  yes-case
  no-case)

(defparameter *node-list* nil)

(defun init ()
  (setf *node-list* nil)
  'initialized)

(defun add-node (name question yes-case no-case)
  (push (make-node :name name
		   :question question
		   :yes-case yes-case
		   :no-case no-case)
	*node-list*)
  name)

(defun find-node (x)
  (find-if #'(lambda (node)
	       (equal (node-name node) x))
	   *node-list*))

(defun process-node (name)
  (let ((nd (find-node name)))
    (if nd
	(if (yes-or-no-p "~&~A"
			 (node-question nd))
	    (node-yes-case nd)
	    (node-no-case nd))
	(format t
		"~&Node ~S not yet defined." name))))

(defun run()
  (do ((current-node 'start
		     (process-node current-node)))
      ((null current-node) nil)
    (cond ((stringp current-node)
	   (format t "~&~A" current-node)
	   (return nil)))))

(defun interactive-add ()
  (let* ((name (prompt-for "Node name? "))
	 (question (prompt-for "Question? "))
	 (yes-action (prompt-for "If Yes? "))
	 (no-action (prompt-for "If No? ")))
    (add-node name question yes-action no-action)))

(defun prompt-for (str)
  (format t "~&~A" str)
  (read))


(add-node 'start
	  "Does the engine turn over?"
	  'engine-turns-over
	  'engine-wont-turn-over)
(add-node 'engine-turns-over
	  "Will the engine run for any period of time?"
	  'engine-will-run-briefly
	  'engine-wont-run)
(add-node 'engine-wont-run
	  "Is there gas in the tank?"
	  'gas-in-tank
	  "Fill the tank and try starting the engine again.")
(add-node 'engine-wont-turn-over
	  "Do you hear any sound when you turn the key?"
	  'sound-when-turn-key
	  'no-sound-when-turn-key)
(add-node 'no-sound-when-turn-key
	  "Is the battery voltage low?"
	  "Replace the battery"
	  'battery-voltage-ok)
(add-node 'battery-voltage-ok
	  "Are the battery cables dirty or loose?"
	  "Clean the cables and tighten the connections."
	  'battery-cables-good)
(add-node 'engine-will-run-briefly
	  "Does the engine stall when cold bit not when warm? "
	  'stalls-only-when-cold
	  'stalls-even-when-warm)
(add-node 'stalls-only-when-cold
	  "Is the cold idle speed at least 700 rpm? "
	  'cold-idle-speed-normal
	  "Adjust the cold idle speed.")