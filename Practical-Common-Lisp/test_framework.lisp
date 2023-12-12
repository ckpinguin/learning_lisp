; dynamic variable, that each test function can bind to, so
; we have a way to know, where exacty a test is failing
(defvar *test-name* nil)

(defun report-result (result form)
  "Report the results of a single test case. Called by 'check'."
  (format t "~:[FAIL~;pass~] ... ~a: ~a~%" result *test-name* form)
  result)

(defmacro check (&body forms)
  "Run each expression in 'forms' as a test case."
  `(combine-results
	 ,@(loop for f in forms collect `(report-result ,f ',f))))

(defmacro combine-results (&body forms)
  "Combine the results as booleans of evaluating 'forms' in order,
   but without the short-circuit behaviour of AND."
  (with-gensyms (result)
	`(let ((,result t))
	   ,@(loop for f in forms collect `(unless ,f (setf ,result nil)))
	   ,result)))

(defmacro deftest (name parameters &body body)
  "Define a test function. Within a test function we can call
   other test functions or use 'check' to run individual test
   cases."
  `(defun ,name ,parameters
	 (let ((*test-name* (append *test-name* (list ',name))))
	   ,@body)))

(defmacro with-gensyms ((&rest names) &body body)
  `(let ,(loop for n in names collect `(,n (gensym)))
  ,@body))


(deftest test-+ ()
  (check
	(= (+ 21 3) 24)
	(= (+ -2 -1 3) 0)
	(= (+ 1 1) 3)))

(deftest test-* ()
  (check
	(= (* 2 3) 6)
	(= (* 1 4) 4)))

(deftest test-arithmetic ()
  (combine-results
	(test-+)
	(test-*)))

(deftest test-math ()
  (combine-results
	(test-arithmetic)))