;;; -*- Mode: LISP; Syntax: Common-lisp; Package: User; Base: 10 -*-

(in-package :User)

;;;===========================================================================
;;;===========================================================================
;;; Some simple tracing/metering utilities. 
;;;
;;; With-Function-Call-Count: Takes a list of function names and a body of code
;;; ========================  and returns two values: (A) the normal return
;;;                           value of the body of code and (B) a list of the
;;;                           number of times the functions were called during
;;;                           the execution of the Body.
;;;                           > (With-Function-Call-Count (F1 F2 F3)
;;;                               (Foo))
;;;                           Foo-Result   ; return result of (Foo)
;;;                           (X Y Z)      ; F1 was called X times, F2 Y
;;;                                        ; times and F3 Z times while
;;;                                        ; executing (Foo).
;;;
;;; Time-Form: Takes a single form and optionally an integer N (default 20). It
;;; =========  runs the form N times and prints out the average time. Useful
;;;            for timing very small, fast functions when the time-step of
;;;            the builtin TIME function is too coarse.
;;;            > (Time-Form (Foo))     ; Call (Foo) 20 times + print avg time
;;;            > (Time-Form (Bar) 100) ; Call (Bar) 100 times + print avg time
;;;
;;; Breakon: Takes a function name and changes the function to enter BREAK on
;;; =======  entry to that function. Lets you do a backtrace and other
;;;          options your debugger gives you.
;;;          > (Breakon 'F1)
;;;          > (Foo)           ; assume F1 gets called when Foo executes
;;;          Break: `Breakon' specified for function F1.
;;;          <Debugger Options>
;;;
;;; Unbreakon: Returns a function to state it was in before Breakon was called.
;;; =========
;;;
;;; Make-Timer (and Run-Timer and Timer-Results):
;;; ============================================
;;;   This is used in the rare instances when you need to time something that
;;;   is near to or faster than then internal clock, but where you can't run
;;;   it multiple times in a row, as with Time-Form. An example is a function
;;;   where certain parameters need to get reset (or a table cleared) after
;;;   each execution, but you don't want to include the time of resetting the
;;;   parameters (or clearing the table) in your benchmark. For instance,
;;;   suppose Foo is the function you want to time, and Reset-Foo is the
;;;   procedure that clears out the table or resets the parameters Foo
;;;   needs. In such a case, you would do the following, assuming '> ' is
;;;   the Lisp prompt:
;;;   
;;;   > (setq Timer-1 (Make-Timer (Foo)))
;;;   > (loop repeat <Num Trials> do
;;;      (Run-Timer Timer-1)
;;;      (Reset-Foo) )
;;;   > (Timer-Results Timer-1)
;;;   (Foo) was executed <Num Trials> times, taking an average of xxx seconds.
;;;
;;; 
;;; 1993-95 Marty Hall. marty_hall@jhuapl.edu
;;; The Johns Hopkins University Applied Physics Lab
;;;
;;; adapted to Xlisp by Jan de Leeuw (deleeuw@stat.ucla.edu)
;;; 
;;; This code may be freely copied and used without restriction.
;;;===========================================================================
;;;===========================================================================



;;;===========================================================================
;;; Takes a list of function names and a body of code, and returns a list of
;;; the number of times the functions were called during the execution of the
;;; Body. See the doc string for more details. The UNWIND-PROTECT is to make
;;; sure the call-count gets reset even if the Body of code crashes. Also,
;;; this works for either regular or generic functions, but there is no way
;;; to specify that only one particular method of a generic function gets
;;; counted. Also (to risk stating the obvious), this will not work for
;;; counting macros or INLINEd functions. 3/93 Marty Hall.

(defmacro With-Function-Call-Count (Function-Name-List &body Body)
  "Takes a list of function names and a body of code, and returns two values:
   (A) the normal return value of the body of code and (B) a list of the
   number of times the functions were called during the execution of the Body.
   Eg:

   (With-Function-Call-Count (Speed Latitude Longitude)
     (Make-Top-Level-Display)
     (Make-MAD-Display))

   returns (867 651 651) as the secondary value, indicating SPEED was called
   867 times, and LATITUDE and LONGITUDE 651 times each during the execution
   of the top-level and MAD displays."
  (let ((Call-Count-Variable (gensym "CALL-COUNT-"))
	(Return-Value (gensym "RETURN-VALUE-")))
    `(let (,Call-Count-Variable ,Return-Value)
       (unwind-protect
	   (progn
	     (mapc #'Make-Function-Countable ',Function-Name-List)
	     (setq ,Return-Value (progn ,@Body))
	     (setq ,Call-Count-Variable
		   (mapcar #'(lambda (Function-Name)
			       (get Function-Name :Call-Count))
			   ',Function-Name-List))
	     (values ,Return-Value ,Call-Count-Variable) )
	 (mapc #'Make-Function-Uncountable ',Function-Name-List) ) )
))

;;;===========================================================================
;;; Changes a function from its normal version to one that counts how often it
;;; is called. Should only be used temporarily. Also note this won't work for
;;; recursive routines without the addition of Make-Function-Countable,
;;; because the internal calls go to the non-counting version.
;;; This is normally called from With-Function-Call-Count, not directly.
;;; 3/93 Marty Hall

(defun Countable-Function (Function-Name)
  "Takes a function NAME and returns a function OBJECT that does what #'NAME
   did, except also keeps track of the number of times it has been called"
  (let ((Function (symbol-function Function-Name)))
    (setf (get Function-Name :Call-Count) 0)
    (setf (get Function-Name :Non-Counting-Function) Function)
    #'(lambda (&rest Args)
	(incf (get Function-Name :Call-Count))
	(apply Function Args)) ))

;;;===========================================================================
;;; Makes function countable. Generally accessed via With-Function-Call-Count.
;;; 3/93 Marty Hall.

(defun Make-Function-Countable (Function-Name)
  "Given a function name changes it into equivalent version that counts
   function calls"
  (setf (symbol-function Function-Name)
	(Countable-Function Function-Name)) )

;;;===========================================================================
;;; Undoes the above.  3/93 Marty Hall

(defun Make-Function-Uncountable (Function-Name)
  "Returns the function to its original (non-counting) state"
  (let ((Original (get Function-Name :Non-Counting-Function)))
    (cond      
      (Original
       (setf (symbol-function Function-Name) Original)
       (remf (symbol-plist Function-Name) :Non-Counting-Function)
       (remf (symbol-plist Function-Name) :Call-Count)
       Original)
      (t
       (format nil "~%Function ~S wasn't countable to begin with: unchanged."
	           Function-Name)))
))

;;;===========================================================================
;;; The Lucid clock has resolution of only 0.01 seconds. So for more accurate
;;; timing of relatively small forms, you should execute it multiple times
;;; and then divide the resultant time by the number of repetitions to get
;;; the average time. However, more repetitions also increases the likelihook
;;; of a gc during execution. 7/94 Marty Hall.

(defmacro Time-Form (Form &optional (Repetitions 20))
  "Runs FORM N times, printing avg execution time and returning FORM's value"
  (declare (optimize speed (safety 1) (compilation-speed 0)))
  `(let* ((Start (get-internal-run-time))
          (Value (dotimes (i ,Repetitions) ,Form))
	  (Stop (get-internal-run-time)))
    (format t "~%Time to do ~S is ~0,4F sec."
     ',Form
     (float (/ (- Stop Start)
	       (* ,Repetitions internal-time-units-per-second))))
    Value))

;;;===========================================================================
;;; Often useful to find when you want to find out why/where a certain
;;; function is being called. Ie you know FOO is being called, but want to see
;;; who is calling it. Put BREAKOn on FOO then do a backtrace. To risk
;;; stating the obvious, this will not work for macros or INLINEd functions.
;;; Idea from si:breakon on Symbolics. 9/93 Marty Hall

(defun Breakon (Function-Name)
  "Given a function name changes it into an `equivalent' version that BREAKs
  upon entry, letting you do a backtrace, etc."
  (setf (symbol-function Function-Name)
	(Function-with-Break Function-Name)) )

;;;===========================================================================
;;; Internal routine that returns the new function that does the BREAK.
;;; 9/93 Marty Hall

(defun Function-with-Break (Function-Name)
  "Takes a function NAME and returns a function OBJECT that does what #'NAME
   did, except that it enters BREAK at the beginning (allowing a backtrace or
   examination of the local variables). Use BREAKON instead of calling this
   directly."
  (let ((Function (symbol-function Function-Name)))
    (setf (get Function-Name :Non-Breaking-Function) Function)
    #'(lambda (&rest Args)
	(break "`Breakon' specified for function ~S." Function-Name)
	(apply Function Args)) ))

;;;===========================================================================
;;; Undoes the above.  9/93 Marty Hall

(defun Unbreakon (Function-Name)
  "Returns the function to its original (non-breaking) state"
  (let ((Original (get Function-Name :Non-Breaking-Function)))
    (cond      
      (Original
       (setf (symbol-function Function-Name) Original)
       (remf (symbol-plist Function-Name) :Non-Breaking-Function)
       Original)
      (t
       (format nil "~%BREAKON wasn't set for function ~S: unchanged."
	       Function-Name)))
))

;;;===========================================================================
;;; Rarely used -- generally use Time-Form instead for timing fast
;;; functions (near to the resolution of the internal clock), and the
;;; builtin TIME function othertimes. 1/95 Marty Hall.

(defmacro Make-Timer (Form)
  "This is used in the rare instances when you need to time something that
   is near to or faster than then internal clock, but where you can't run
   it multiple times in a row, as with Time-Form. An example is a function
   where certain parameters need to get reset (or a table cleared) after
   each execution, but you don't want to include the time of resetting the
   parameters (or clearing the table) in your benchmark. For instance,
   suppose Foo is the function you want to time, and Reset-Foo is the
   procedure that clears out the table or resets the parameters Foo
   needs. In such a case, you would do the following, assuming '> ' is
   the Lisp prompt:
   
   > (setq Timer-1 (Make-Timer (Foo)))
   > (loop repeat <Num Trials> do
      (Run-Timer Timer-1)
      (Reset-Foo) )
   > (Timer-Results Timer-1)
   (Foo) was executed <Num Trials> times, taking an average of xxx seconds."
  `(let ((Elapsed-Time 0)
	 (Execution-Count 0))
     (cons
       #'(lambda ()
	   (declare (optimize speed (safety 1) (compilation-speed 0)))
	   (let* ((Time (get-internal-run-time))
		  (Return-Value ,Form))
	     (incf Elapsed-Time (- (get-internal-run-time) Time))
	     (incf Execution-Count)
	     Return-Value))
       #'(lambda (&optional (Stream t))
	   (if
	     (= 0 Execution-Count)
	     (format Stream "~&Sorry: ~S has never been executed. ~
                             Use (Run-Timer <Timer>).~%"
		     ',Form)
	     (format Stream "~&~S was executed ~D times, taking an average ~
                             of ~0,4F seconds.~%"
		     ',Form
		     Execution-Count
		     (float
		      (/ Elapsed-Time
			 (* Execution-Count internal-time-units-per-second)))))
	     (values)) )))

;;;===========================================================================
;;; 1/95 Marty Hall

(defun Run-Timer (Timer)
  "Takes a timer created via Make-Timer and executes it, keeping track
   of cumulative execution time and number of executions. Use
   (Timer-Results <Timer>) to get a report of these values."
  (declare (optimize speed (safety 1) (compilation-speed 0)))
  (funcall (car Timer)))

;;;===========================================================================
;;; 1/95 Marty Hall.

(defun Timer-Results (Timer &optional (Stream t))
  "Takes a timer created via Make-Timer and reports how many times the
   timer has been executed (via Run-Timer) and average execution time."
  (funcall (cdr Timer) Stream))
	  

;;;===========================================================================
;;; Defines a list of user-level symbols and exports them. The right thing to
;;; do is probably to put these in a separate package (not :User), and then
;;; import this list into whatever package you are using.

(eval-when (compile load eval)
  (defvar *Simple-Metering-Function-Names*
    '(With-Function-Call-Count Time-Form Breakon Unbreakon
      Make-Timer Run-Timer Timer-Results Without-GC With-Metering)
    "The names of the user-level functions in the Simple-Metering package.
     This list can be passed to IMPORT/EXPORT calls."))

(export *Simple-Metering-Function-Names*)

;;;===========================================================================

