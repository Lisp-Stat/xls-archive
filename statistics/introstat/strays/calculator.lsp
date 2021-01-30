(defproto ti-30-calculator
  '(dialog-window stack display status-window drg inv mem should-clear
		  count-x sum-x sum-y sum-x-sq sum-y-sq sum-xy need-y))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; strchr
;
; str - string
; c - character
; returns - index of first occurence of character c in string str
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmeth ti-30-calculator :strchr (str c)
    (dotimes (i (length str) nil)
	     (cond ((eq (elt str i) c) (return i))
		   (T nil))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; is-number
;
; x - string
; returns - true if x is a valid lisp number
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmeth ti-30-calculator :is-number (x)
  (cond ((send self :strchr x #\() nil)
	((send self :strchr x #\)) nil)
	(t (numberp (read-from-string x)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; is-unary
;
; x - string
; returns - true if x is a unary operator
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmeth ti-30-calculator :is-unary (x)
  (dolist (i '("!" "log" "ln" "sin" "asin" "cos" "acos" "tan" "atan" "1/x"
	       "x^2" "sqrt" "10^x" "e^x")
	     nil)
	  (cond ((string= x i) (return t)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; is-binary
;
; x - string
; returns - true if x is a binary operator
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmeth ti-30-calculator :is-binary (x)
  (dolist (i '("+" "-" "*" "/" "^") nil)
	   (cond ((string= x i) (return t)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; is-operator
;
; x - string
; returns - true if x is a unary or binary operator
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmeth ti-30-calculator :is-operator (x)
  (or (send self :is-unary x) (send self :is-binary x)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; op-precedence
;
; x - string
; returns - the precedence (0=lowest,4=highest) of the operator x
;           nil if x is not an operator
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmeth ti-30-calculator :op-precedence (x)
  (cond	((string= x "+") 1)
	((string= x "-") 1)
	((string= x "*") 2)
	((string= x "/") 2)
	((string= x "^") 3)
	(T nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; stack-top-index
;
; returns - the index of the top-most number on the stack
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmeth ti-30-calculator :stack-top-index ()
  (dotimes (i (length (send self :slot-value 'stack)) nil)
	   (cond ((send self :is-number (elt (send self :slot-value 'stack) i))
		  (return i)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; stack-top
;
; returns - the topmost number on the stack
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmeth ti-30-calculator :stack-top ()
  (elt (send self :slot-value 'stack) (send self :stack-top-index)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; push-number
;
; x - number string
; side-effect - if the top member of the stack is a number, it is replaced by x
;               if the top member of the stack is an operator (not number),
;                   the x is pushed on the stack
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmeth ti-30-calculator :push-number (x)
  (let ((stack (send self :slot-value 'stack)))
    (cond ((send self :is-number (car stack))
	   (send self :slot-value 'stack (cons x (cdr stack))))
	  (T (send self :slot-value 'stack (cons x stack))))
    (send self :display-stack-top)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; push-unary-op
;
; x - string
; side-effects - if the top of the stack is an operator, the topmost number
;                    of the stack is pushed, and the stack evaluated
;                if the top of the stack is a number, the stack is evaluated
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmeth ti-30-calculator :push-unary-op (x)
  (let ((stack (send self :slot-value 'stack)) (the-op x) the-val result)
    (send self :slot-value 'should-clear t)
    (cond ((eq (send self :slot-value 'inv) t)
	   (setf the-op (cond ((string= x "sin")   "asin")
			      ((string= x "cos")   "acos")
			      ((string= x "tan")   "atan")
			      ((string= x "x^2")   "sqrt")
			      ((string= x "sqrt")  "x^2")
			      ((string= x "log10") "10^x")
			      ((string= x "log")   "e^x")))
	   (send self :slot-value 'inv nil)))
    (cond ((send self :is-operator (car stack))
	   (send self :slot-value 'stack (cons (send self :stack-top) stack))))
    (setf the-val (read-from-string (car (send self :slot-value 'stack))))
    (setf result
	  (format nil "~a"
		  (cond ((string= the-op "!")   (send self :factorial the-val))
			((string= the-op "1/x") (/ 1 the-val))
			((string= the-op "x^2") (^ the-val 2))
			((string= the-op "sqrt")(sqrt the-val))
			((string= the-op "ln")  (log the-val))
			((string= the-op "log") (log the-val 10))
			((string= the-op "10^x")(^ 10 the-val))
			((string= the-op "e^x") (exp the-val))
			((or (string= the-op "sin")
			     (string= the-op "cos")
			     (string= the-op "tan"))
                         (apply (read-from-string the-op)
				(list
				 (case (send self :slot-value 'drg)
				       ((DEG) (* PI (/ the-val 180)))
				       ((RAD) the-val)
				       ((GRAD) (* (/ PI 200) the-val 100))))))
			((or (string= the-op "asin")
			     (string= the-op "acos")
			     (string= the-op "atan"))
			 (case (send self :slot-value 'drg)
			       ((DEG)
				(* (/ 180 PI)
				   (apply (read-from-string the-op)
					  (list the-val))))
			       ((RAD)
				(apply (read-from-string the-op)
				       (list the-val)))
			       ((GRAD)
				(* (/ 200 PI) (apply (read-from-string the-op)
						     (list the-val))))))
			(t
			 (format *standard-output*
				 "Error: unimplemented op")))))
    (setf stack (cons result (cdr stack)))
    (send self :slot-value 'stack stack)
    (send self :display-stack-top)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; push-binary-op
;
; x - string
; side-effects - if the top of the stack is an operator, x replaces that
;                    operator
;                if the top of the stack is a number, x is pushed onto the
;                    stack
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmeth ti-30-calculator :push-binary-op (x)
  (let ((stack (send self :slot-value 'stack)) (the-op x))
    (send self :slot-value 'should-clear t)
    (cond ((send self :is-operator (car stack))
	   (send self :slot-value 'stack (cons the-op (cdr stack)))
	   (send self :evaluate-stack))
	  (T
	   (send self :slot-value 'stack (cons the-op stack))
	   (send self :evaluate-stack)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; push-parenthesis
;
; x - string, either "(" or ")"
; test cases:
;     2 * 3 ( 4 + 1 )   -->   2 * ( 34 + 1 )   -->   2 * 35   -->   70
;     2 * ( 3 + 4 + )   -->   2 * ( 3 + 4 +
;     ( 2   -->   2
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmeth ti-30-calculator :push-parenthesis (x)
  (let ((stack (send self :slot-value 'stack))
	(stack-top (car (send self :slot-value 'stack))))
    (cond ((string= x "(")
	   (cond ((string= "(" stack-top)
		  (send self :slot-value cons (x stack)))
		 ((send self :is-operator stack-top)
		  (send self :slot-value 'should-clear t)
		  (send self :slot-value 'stack (cons x stack)))
		 ((send self :is-number stack-top)
		  (cond ((send self :slot-value 'should-clear)
			 (send self :slot-value 'stack
			       (cons (car stack) (cons x (cdr stack)))))
			(t
			 (send self :slot-value 'stack
			       (cons (car stack) (cons x (cdr stack)))))))
		 (t (format *standard-output* "Error: unknown stack-top"))))
	  ((string= x ")")
	   (cond ((member "(" stack :test #'string=)
		  (cond ((send self :is-number stack-top)
			 (send self :slot-value 'stack (cons ")" stack))
			 (send self :evaluate-stack))))))))
  (send self :display-stack-top))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; display-stack-top 
;
; side-effect - finds the topmost number on the stack, and displays it's value
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmeth ti-30-calculator :display-stack-top ()
  (send (send self :slot-value 'status-window) :text
	      (format nil "~a ~a ~a"
		      (case (send self :slot-value 'inv)
			    (nil "   ")
			    (t   "INV"))
		      (case (send self :slot-value 'drg)
			    (DEG  "DEG       ")
			    (RAD  "   RAD    ")
			    (GRAD "      GRAD"))
		      (case (send self :slot-value 'need-y)
			    (nil "      ")
			    (t   "NEED-Y"))))
  (do ((stack (send self :slot-value 'stack) (cdr stack)))
      ((eq stack nil)
       (format *standard-output* "display-stack-top: empty stack?"))
      (cond ((send self :is-number (car stack))
	     (send (send self :slot-value 'display) :text (car stack))
	     (return)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; correct-error
;
; side-effect - removes the topmost elements of the stack up-to and
;               including the first topmost number
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmeth ti-30-calculator :correct-error ()
  (do ((stack (send self :slot-value 'stack) (cdr stack)))
      ((send self :is-number (car stack))
       (send self :slot-value 'stack
	     (cond ((eq (cdr stack) nil) '("0"))
		   (t (cdr stack)))))
      (cond ((eq nil (send self :stack-top))
	     (send self :slot-value 'stack '("0"))))
      (send self :display-stack-top)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; all-clear
;
; side-effect - resets the stack to it's initial state
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmeth ti-30-calculator :all-clear ()
  (send self :slot-value 'stack '("0"))
  (send self :slot-value 'count-x 0)
  (send self :slot-value 'count-x 0)
  (send self :slot-value 'sum-x 0)
  (send self :slot-value 'sum-y 0)
  (send self :slot-value 'sum-x-sq 0)
  (send self :slot-value 'sum-y-sq 0)
  (send self :slot-value 'sum-xy 0)
  (send self :slot-value 'need-y nil)
  (send self :display-stack-top))

(defmeth ti-30-calculator :toggle-drg ()
  (send self :slot-value 'drg (case (send self :slot-value 'drg)
				    ((DEG) 'RAD)
				    ((RAD) 'GRAD)
				    ((GRAD) 'DEG)))
  (send self :display-stack-top))

(defmeth ti-30-calculator :store ()
  (send self :slot-value 'mem (send self :stack-top))
  (send self :slot-value 'should-clear t))

(defmeth ti-30-calculator :recall ()
  (send self :push-number (send self :slot-value 'mem))
  (send self :slot-value 'should-clear t))

(defmeth ti-30-calculator :sum ()
  (send self :slot-value 'mem
	(format nil "~a" (+ (read-from-string (send self :slot-value 'mem))
			    (read-from-string (send self :stack-top)))))
  (send self :slot-value 'should-clear t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; exchange
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmeth ti-30-calculator :exchange ()
  (let ((tmp (send self :slot-value 'mem)))
    (send self :slot-value 'mem (send self :stack-top))
    (send self :push-number tmp)
    (send self :display-stack-top))
  (send self :slot-value 'should-clear t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; factorial
;
; x - an integer >= 0
; returns - x!=x(x-1)(x-2)...1
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmeth ti-30-calculator :factorial (x)
  (do ((i 2 (+ i 1)) (sum 1)) ((> i x) sum)
      (setf sum (* sum i))))

(defmeth ti-30-calculator :evaluate-stack ()
  (let ((stack (send self :slot-value 'stack)) tmp)
    (do ((op1 (first stack) (first stack))
	 (num1 (second stack) (second stack))
	 (op2 (third stack) (third stack))
	 (num2 (fourth stack) (fourth stack)))
	(nil nil)
	(cond ((string= op1 ")")
	       (cond ((string= op2 "(")
		      (setf stack (cons num1 (cdddr stack)))
		      (send self :slot-value 'stack stack)
		      (send self :display-stack-top)
		      (return))
		     (T
		      (setf num1 (format nil "~a"
					 (eval (read-from-string
						(format nil "( ~a ~a ~a )"
							op2 num2 num1)))))
		      (setf stack (cons op1 (cons num1 (cddddr stack))))
		      (send self :slot-value 'stack stack)
		      (send self :display-stack-top))))
	      ((string= op2 "(")
	       (return))
	      ((numberp (read-from-string op1)) (return))
	      ((string= op1 "=")
	       (cond ((>= (length stack) 4)
		      (setf num1 (format nil "~a"
					 (eval (read-from-string
						(format nil "( ~a ~a ~a )"
							op2 num2 num1))))) 
		      (setf stack (cons op1 (cons num1 (cddddr stack))))
		      (send self :slot-value 'stack stack)
		      (send self :display-stack-top))
		     ((= (length stack) 2)
		      (setf stack (list num1))
		      (send self :slot-value 'stack stack)
		      (send self :display-stack-top))
		     (T
		      (format *standard-output* "Error: stack=~a" stack)
		      (return))))
	      ((= (length stack) 1) (return))
	      ((= (length stack) 2) (return))
	      ((= (length stack) 3) (return))
	      ((<= (send self :op-precedence op1)
		   (send self :op-precedence op2))
	       (setf num1 (format nil "~a"
				  (eval (read-from-string
					 (format nil "( ~a ~a ~a )"
						 op2 num2 num1))))) 
	       (setf stack (cons op1 (cons num1 (cddddr stack))))
	       (send self :slot-value 'stack stack)
	       (send self :display-stack-top))
	      (T (send self :slot-value 'stack stack)
		 (return))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; toggle-sign
;
; side effect - if a number has no exponent, toggle the sign of the mantissa
;               if a number has an exponent, toggle the sign of the exponent
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmeth ti-30-calculator :toggle-sign ()
  (let ((stack (copy-list (send self :slot-value 'stack)))
	(top (send self :stack-top))
	(i (send self :strchr (send self :stack-top) #\E))
	(new-sign ""))
    (cond (i
	   (cond ((eq (elt top (+ i 1)) #\+)
		  (setf new-sign "-"))
		 ((eq (elt top (+ i 1)) #\-)
		  (setf new-sign "+")))
	   (setf top (format nil "~aE~a~a"
			     (subseq top 0 i)
			     new-sign
			     (subseq top (+ i 2) (length top)))))
	  (t
	   (cond ((eq (elt top 0) #\-)
		  (setf top (format nil "~a" (subseq top 1))))
		 (t
		  (setf top (format nil "-~a" top))))))
    (setf (elt (send self :slot-value 'stack)
	       (send self :stack-top-index))
	  top)
    (send self :display-stack-top)))
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; toggle-inv
;
; logically negate the inverse slot
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmeth ti-30-calculator :toggle-inv ()
  (send self :slot-value 'inv (not (send self :slot-value 'inv)))
  (send self :display-stack-top))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; enter-char
;
; c - character: 0-9 or .
;
; side-effect - if should-clear is true
;                   if the stack-top is a number,
;                       if c is a digit, the stack-top is replaced by c
;                       if c is a decimal point, the stack-top is replaced
;                           by 0.
;                   if the stack-top is an operator,
;                       if c is a digit, c is pushed on the stack
;                       if c is a decimal point, 0. is pushed on the stack
;               if should-clear is nil,
;                   if the stack-top is a number
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmeth ti-30-calculator :enter-char (c)
  (let ((stack (send self :slot-value 'stack))
	(stack-top (car (send self :slot-value 'stack)))
	new-val)
    (cond ((send self :slot-value 'should-clear)
	   (send self :slot-value 'should-clear nil)
	   (cond ((eq c #\.) (setf new-val "0."))
		 (t (setf new-val (string c))))
	   (cond ((string= stack-top "(")
		  (send self :slot-value 'stack (cons new-val stack)))
		 ((send self :is-number stack-top)
		  (send self :slot-value 'stack (cons new-val (cdr stack))))
		 ((send self :is-operator stack-top)
		  (send self :slot-value 'stack (cons new-val stack)))
		 (t
		  (format *standard-output*
			  "Error: enter-char: invalid stack-top"))))
	  (t
	   (cond ((send self :is-number stack-top)
		  (cond ((equal c #\.)
			 (cond ((not (send self :strchr stack-top #\.))
				(setf new-val (format nil "~a." stack-top)))
			       (t (return nil))))
			((string= stack-top "0")
			 (setf new-val (string c)))
			(t
			 (setf new-val
			       (format nil "~a~a" stack-top (string c)))))
		  (send self :slot-value 'stack (cons new-val (cdr stack))))
		 (t (send self :slot-value 'stack (cons (string c) stack))))))
    (send self :display-stack-top)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; exponent
;
; side-effect - if the top-most number is already in exponential notiation,
;                   do nothing
;               else append an "E+" to the end to the top-most number
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmeth ti-30-calculator :exponent ()
  (let ((stack (send self :slot-value 'stack))
	(new-val (car (send self :slot-value 'stack))))
    (cond ((send self :op-precedence new-val)
	   (setf new-val "0E+")
	   (send self :slot-value 'stack (cons new-val stack)))
	  ((send self :strchr new-val #\E)
	   (return nil))
	  (T
	   (setf new-val (format nil "~aE+" (car stack)))
	   (send self :slot-value 'stack (cons new-val (cdr stack)))))
    (send self :display-stack-top)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; count-x
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmeth ti-30-calculator :enter-x ()
  (let ((the-num (read-from-string (send self :stack-top))))
    (send self :slot-value 'count-x 
	  (+ (send self :slot-value 'count-x) 1))
    (send self :slot-value 'sum-x (+ (send self :slot-value 'sum-x) the-num))
    (send self :slot-value 'sum-x-sq
	  (+ (send self :slot-value 'sum-x-sq) (* the-num the-num)))
    (send self :slot-value 'should-clear t)
    (send self :slot-value 'need-y the-num)
    (send self :display-stack-top)))

(defmeth ti-30-calculator :enter-y ()
  (cond ((send self :slot-value 'need-y)
	 (let ((the-num (read-from-string (send self :stack-top))))
	   (send self :slot-value 'sum-y
		 (+ (send self :slot-value 'sum-y) the-num))
	   (send self :slot-value 'sum-y-sq
		 (+ (send self :slot-value 'sum-y-sq) (* the-num the-num)))
	   (send self :slot-value 'sum-xy
		 (+ (send self :slot-value 'sum-xy)
		    (* the-num (send self :slot-value 'need-y))))
	   (send self :slot-value 'should-clear t)
	   (send self :slot-value 'need-y nil)
	   (send self :display-stack-top)))))

(defmeth ti-30-calculator :count-x ()
  (send self :push-number (format nil "~a" (send self :slot-value 'count-x))))

(defmeth ti-30-calculator :sum-x ()
  (send self :push-number (format nil "~a" (send self :slot-value 'sum-x))))

(defmeth ti-30-calculator :sum-y ()
  (send self :push-number (format nil "~a" (send self :slot-value 'sum-y))))

(defmeth ti-30-calculator :sum-x-sq ()
  (send self :push-number (format nil "~a" (send self :slot-value 'sum-x-sq))))

(defmeth ti-30-calculator :sum-y-sq ()
  (send self :push-number (format nil "~a" (send self :slot-value 'sum-y-sq))))

(defmeth ti-30-calculator :sum-xy ()
  (send self :push-number (format nil "~a" (send self :slot-value 'sum-xy))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmeth ti-30-calculator :isnew ()
  (send self :slot-value 'display
	(send text-item-proto :new "0" :text-length 50))
  (send self :slot-value 'status-window
	(send text-item-proto :new "" :text-length 50))
  (send self :slot-value 'stack '("0"))
  (send self :slot-value 'mem "0")
  (send self :slot-value 'drg 'DEG)
  (send self :slot-value 'inv nil)
  (send self :slot-value 'should-clear t)
  (send self :slot-value 'count-x 0)
  (send self :slot-value 'sum-x 0)
  (send self :slot-value 'sum-y 0)
  (send self :slot-value 'sum-x-sq 0)
  (send self :slot-value 'sum-y-sq 0)
  (send self :slot-value 'sum-xy 0)
  (send self :slot-value 'need-y nil)
  (send self :slot-value 'dialog-window
	(send dialog-proto :new
	      (list (list (send button-item-proto :new "Close"
				:action #'(lambda () (exit))))
		    (list (send self :slot-value 'display))
		    (list (send self :slot-value 'status-window))
		    (list (send button-item-proto :new "X"
				:action #'(lambda ()
					    (send self :enter-x)))
			  (send button-item-proto :new "1/x"
				:action #'(lambda ()
					    (send self :push-unary-op "1/x")))
			  (send button-item-proto :new "x^2"
				:action #'(lambda ()
					    (send self :push-unary-op "x^2")))
			  (send button-item-proto :new "SQRT"
				:action #'(lambda ()
					    (send self :push-unary-op "sqrt")))
			  (send button-item-proto :new "CE/C"
				:action #'(lambda ()
					    (send self :correct-error)))
			  (send button-item-proto :new "AC"
				:action #'(lambda ()
					    (send self :all-clear))))
		    (list (send button-item-proto :new "Y"
				:action #'(lambda ()
					    (send self :enter-y)))
			  (send button-item-proto :new "INV"
				:action #'(lambda ()
					    (send self :toggle-inv)))
			  (send button-item-proto :new "sin"
				:action #'(lambda ()
					    (send self :push-unary-op "sin")))
			  (send button-item-proto :new "cos"
				:action #'(lambda ()
					    (send self :push-unary-op "cos")))
			  (send button-item-proto :new "tan"
				:action #'(lambda ()
					    (send self :push-unary-op "tan")))
			  (send button-item-proto :new "DRG"
				:action #'(lambda ()
					    (send self :toggle-drg))))
		    (list (send button-item-proto :new "N(X)"
				:action #'(lambda ()
					    (send self :count-x)))
			  (send button-item-proto :new "e"
				:action #'(lambda ()
					    (send self :push-number
						  (format nil "~a" (exp 1)))))
			  (send button-item-proto :new "EE"
				:action #'(lambda ()
					    (send self :exponent)))
			  (send button-item-proto :new "log"
				:action #'(lambda ()
					    (send self :push-unary-op "log")))
			  (send button-item-proto :new "ln"
				:action #'(lambda ()
					    (send self :push-unary-op "ln")))
			  (send button-item-proto :new "y^x"
				:action #'(lambda ()
					    (send self :push-binary-op "^"))))
		    (list (send button-item-proto :new "SUM(X)"
				:action #'(lambda ()
					    (send self :sum-x)))
			  (send button-item-proto :new "PI"
				:action #'(lambda ()
					    (send self :push-number 
						  (format nil "~a" PI))))
			  (send button-item-proto :new "x!"
				:action #'(lambda ()
					    (send self :push-unary-op "!")))
			  (send button-item-proto :new "("
				:action #'(lambda ()
					    (send self :push-parenthesis "(")))
			  (send button-item-proto :new ")"
				:action #'(lambda ()
					    (send self :push-parenthesis ")")))
			  (send button-item-proto :new "/"
				:action #'(lambda ()
					    (send self :push-binary-op "/"))))
		    (list (send button-item-proto :new "SUM(Y)"
				:action #'(lambda ()
					    (send self :sum-y)))
			  (send button-item-proto :new "STO"
				:action #'(lambda () (send self :store)))
			  (send button-item-proto :new "7"
				:action #'(lambda ()
					    (send self :enter-char #\7)))
			  (send button-item-proto :new "8"
				:action #'(lambda ()
					    (send self :enter-char #\8)))
			  (send button-item-proto :new "9"
				:action #'(lambda ()
					    (send self :enter-char #\9)))
			  (send button-item-proto :new "*"
				:action #'(lambda ()
					    (send self :push-binary-op "*"))))
		    (list (send button-item-proto :new "SUM(X^2)"
				:action #'(lambda ()
					    (send self :sum-x-sq)))
			  (send button-item-proto :new "RCL"
				:action #'(lambda () (send self :recall)))
			  (send button-item-proto :new "4"
				:action #'(lambda ()
					    (send self :enter-char #\4)))
			  (send button-item-proto :new "5"
				:action #'(lambda ()
					    (send self :enter-char #\5)))
			  (send button-item-proto :new "6"
				:action #'(lambda ()
					    (send self :enter-char #\6)))
			  (send button-item-proto :new "-"
				:action #'(lambda ()
					    (send self :push-binary-op "-"))))
		    (list (send button-item-proto :new "SUM(Y^2)"
				:action #'(lambda ()
					    (send self :sum-y-sq)))
			  (send button-item-proto :new "SUM"
				:action #'(lambda () (send self :sum)))
			  (send button-item-proto :new "1"
				:action #'(lambda ()
					    (send self :enter-char #\1)))
			  (send button-item-proto :new "2"
				:action #'(lambda ()
					    (send self :enter-char #\2)))
			  (send button-item-proto :new "3"
				:action #'(lambda ()
					    (send self :enter-char #\3)))
			  (send button-item-proto :new "+"
				:action #'(lambda ()
					    (send self :push-binary-op "+"))))
		    (list (send button-item-proto :new "SUM(XY)"
				:action #'(lambda ()
					    (send self :sum-xy)))
			  (send button-item-proto :new "EXC"
				:action #'(lambda () (send self :exchange)))
			  (send button-item-proto :new "0"
				:action #'(lambda ()
					    (send self :enter-char #\0)))
			  (send button-item-proto :new "."
				:action #'(lambda ()
					    (send self :enter-char #\.)))
			  (send button-item-proto :new "+/-"
				:action	#'(lambda ()
					    (send self :toggle-sign)))
			  (send button-item-proto :new "="
				:action #'(lambda ()
					    (send self :push-binary-op "=")))))
	      :go-away nil))
  (send self :display-stack-top))

(send ti-30-calculator :new)
