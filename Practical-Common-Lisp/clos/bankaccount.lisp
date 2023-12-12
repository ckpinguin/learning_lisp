(defvar *account-numbers* 0)
(defparameter *minimum-balance* 0)

(defclass bank-account ()
  ((customer-name
	:initarg :customer-name
	:initform (error "Must supply a customer name.")
	:accessor customer-name
	:documentation "The customer's name")
   (balance
	:initarg :balance
	:initform 0
	:reader balance
	:documentation "Current account balance")
   (account-number
	:initform (incf *account-numbers*)
	:reader account-number
	:documentation "Account number, unique within the bank.")
   (account-type
	:reader account-type
	:documentation "Type of account, on of :gold, :silver or :bronze.")))

; Constructor / Initializer
; Specializing for an opening-bonus
(defmethod initialize-instance :after ((account bank-account)
									   &key opening-bonus-percentage)
  (when opening-bonus-percentage
	(incf (slot-value account 'balance)
		  (* (slot-value account 'balance) (/ opening-bonus-percentage 100))))

  (let ((balance (slot-value account 'balance)))
	(setf (slot-value account 'account-type)
		  (cond
			((>= balance 100000) :gold)
			((>= balance 50000) :silver)
			(t :bronze)))))

; Generally one should use with-accessors, but balance is read-only, so
; we resort to the somewhat more crude with-slots
(defmethod assess-low-balance-penalty ((account bank-account))
  (with-slots (balance) account
	(when (< balance *minimum-balance*)
	  (decf balance (* balance 0.01)))))

(defparameter *acct1* 
  (make-instance 'bank-account :customer-name "Chrigiii" :balance 70000 :opening-bonus-percentage 4))
