;;; -*- Mode:Lisp -*-
;;; ***************************************************************************
;;; Common Lisp implementation of "Quickprop", a variation on back-propagation.

;;; For a description of the Quickprop algorithm, see "Faster-Learning
;;; Variations on Back-Propagation: An Empirical Study" by Scott E. Fahlman
;;; in Proceedings of the 1988 Connectionist Models Summer School,
;;; Morgan-Kaufmann, 1988.

;;; This code was written and placed in the public domain by Scott E.
;;; Fahlman.  I would like to be hear about anyone using this code and what
;;; kind of results are achieved.  I can be contacted on Arpanet as
;;; "fahlman@cs.cmu.edu" or by physical mail:

;;; Scott E. Fahlman,
;;; School of Computer Science
;;; Carnegie-Mellon University
;;; Pittsburgh, PA 15213
;;; ***************************************************************************
;;; EDIT HISTORY:
;;;
;;; 12/19/91:
;;; Changed archaic usage (short-float <foo>) to (coerce <foo> 'short-float)
;;; in two places.
;;; ***************************************************************************

;;; This proclamation buys a certain amount of overall speed at the expense
;;; of runtime checking.  Comment it out when debugging new, bug-infested code.

(proclaim '(optimize (speed 3) (space 0) (safety 0)))

;;; Portability note: This file is mostly portable Common Lisp.  A few CMU
;;; extensions are used:

;;; SYSTEM:SERVER with no argument reads one pending external event (from
;;; X, for example) and passes it off to a handler function.  If no event
;;; is pending, it proceeds immediately.  With an argument of N, it waits N
;;; seconds for an event to arrive, then proceeds.  SYSTEM:SERVE-ALL is
;;; similar, but does not return until all pending events have been served.
;;; These calls can be removed if your Lisp handles external events in some
;;; other way, or not at all.

;;; The EXTENSIONS:*IGNORE-FLOATING-POINT-UNDERFLOW* switch, if non-null,
;;; says that floating point underflows should quietly return zero rather
;;; than signalling an error.  If your Lisp does not have such a switch,
;;; you will either have to define an error handler for floating underflows
;;; or check for tiny values at various critical points of the code.

;;; (setq extensions:*ignore-floating-point-underflow* t)

;;; Compensate for the clumsy Common Lisp declaration system.
;;; INCF-SF, *SF, etc. are like INCF, *, etc., but they declare
;;; their operands and results to be short-floats.  The code gets unreadable
;;; quickly if you do this by hand.

(defmacro incf-sf (place &optional (increment 1.0))
  `(the short-float (incf (the short-float ,place)
			  (the short-float ,increment))))

(defmacro decf-sf (place &optional (increment 1.0))
  `(the short-float (decf (the short-float ,place)
			  (the short-float ,increment))))

(defmacro *sf (&rest args)
  `(the short-float
	(* ,@(mapcar #'(lambda (x) (list 'the 'short-float x)) args))))

(defmacro +sf (&rest args)
  `(the short-float
	(+ ,@(mapcar #'(lambda (x) (list 'the 'short-float x)) args))))

(defmacro -sf (&rest args)
  `(the short-float
	(- ,@(mapcar #'(lambda (x) (list 'the 'short-float x)) args))))

(defmacro /sf (&rest args)
  `(the short-float
	(/ ,@(mapcar #'(lambda (x) (list 'the 'short-float x)) args))))


;;;; Assorted Parameters.

;;; Thse parameters and switches control the learning algorithm.

(defvar *weight-range* 2.0
  "Initial weights in the network get random values between plus and
  minus *weight-range*.")
(proclaim '(short-float *weight-range*))

(defvar *sigmoid-prime-offset* 0.1
  "Add this to the sigmoid prime value to eliminate the flat spots where
  derivative goes to zero.")
(proclaim '(short-float *sigmoid-prime-offset*))

(defvar *epsilon* 1.0
  "Controls the amount of linear gradient descent to use.")
(proclaim '(short-float *epsilon*))

(defvar *mu* 1.75
  "Do not let quadratic method take a step greater than this value times
  the previous step.  If this is too large, the learning becomes chaotic.")
(proclaim '(short-float *mu*))

(defvar *decay* -0.0001
  "This factor times the current weight is added to the slope at the
  start of each epoch.  Keeps weights from growing too big.")
(proclaim '(short-float *decay*))

(defvar *hyper-err* t
  "If non-nil, use hyperbolic arctan error function.")

(defvar *split-epsilon* t
  "If non-nil, divide epsilon by unit fan-in before using it.")

(defvar *symmetric* nil
  "If non-nil, use sigmoid activation function ranging from -0.5 to +0.5.
  If nil, range is 0.0 to 1.0.")

;;; These variables and switches control the simulation and display.

(defparameter *epoch* 0
  "Count of the number of times the entire training set has been presented.")
(proclaim '(fixnum *epoch*))

(defvar *restart* t
  "If set, restart whenever the maximum epoch limit is exceeded.
  Else, just quit.")

(defparameter *graphics* nil
  "If nil, skip all routine display updating.")

(defvar *single-pass* nil
  "When on, pause after forward/backward cycle.")

(defvar *single-epoch* nil
  "When on, pause after each training epoch.")

(defparameter *step* nil
  "Turned briefly to T in order to continue after a pause.")

(defvar *layout* nil
  "The layout structure for displaying the current network.")

(defvar *debug-displays* nil
  "If set when creating displays, create the debugging displays as well.")

;;; The real values to be used for logical one and zero values on input
;;; and output.  The routines that build various networks (e.g. BUILD-ENCODER)
;;; look at these.

(defvar *input-zero-value* 0.0
  "Value representing logical zero on inputs.")

(defvar *input-one-value* 1.0
  "Value representing logical one on inputs.")

(defvar *output-zero-value* 0.0
  "Value representing logical zero on outputs.")

(defvar *output-one-value* 1.0
  "Value representing logical one on outputs.")

;;; The sets of training inputs and outputs are stored in parallel vectors.
;;; Each set is a vector of short-float values.

(defvar *training-inputs* nil
  "Vector of input patterns for training the net.")

(defvar *training-outputs* nil
  "Vector of output patterns for training the net.")

;;; For some benchmarks, there is a separate set of values used for testing
;;; the network's ability to generalize.  These values are not used during
;;; training.

(defvar *test-inputs* nil
  "Vector of input patterns for testing the net.")

(defvar *test-outputs* nil
  "Vector of output patterns for testing the net.")


;;;; Fundamental data structures.

;;; Unit outputs and weights are short flonums.

;;; Instead of representing each unit by a structure, we represent the
;;; unit by a fixnum.  This is used to index into various vectors that hold
;;; per-unit information, such as the output state of each unit.

;;; The set of connections COMING INTO each unit is represented by a vector
;;; that is stored with the unit.  Per-connection info is stored in similar
;;; vectors.  The only constraint on network topology is that a unit's
;;; index must be greater than the index of any unit from which it receives
;;; an input.  Regular layers are not required.

;;; Unit 0 is always at a maximum-on value, and has a connection to every
;;; other unit.  The weight on this connection acts as a threshold.
;;; Next come some input units, then some hidden units, and finally some
;;; output units.

;;; The following parameters must be set up by the network-building routines.

(defvar *nunits* 0 "Total number of units in the network.")
(proclaim '(fixnum *nunits*))

(defvar *ninputs* 0 "Number of input units.")
(proclaim '(fixnum *ninputs*))

(defvar *first-hidden* 0 "Index of first hidden unit.")
(proclaim '(fixnum *first-hidden*))

(defvar *nhidden* 0 "Number of hidden units.")
(proclaim '(fixnum *nhidden*))

(defvar *first-output* 0 "Index of first output unit.")
(proclaim '(fixnum *first-output*))

(defvar *noutputs* 0 "Number of output units.")
(proclaim '(fixnum *noutputs*))

(defvar *outputs* nil
  "Vector holding the final output value of each unit.")
(proclaim '(simple-vector *outputs*))

(defvar *error-sums* nil
  "Vector holding the total error activation for each unit.")
(proclaim '(simple-vector *error-sums*))

(defvar *errors* nil
  "Vector holding the final error value for each unit.")
(proclaim '(simple-vector *errors*))

(defvar *nconnections* nil
  "Vector holding the number of incoming connections for each unit.")
(proclaim '(simple-vector *nconnections*))

(defvar *connections* nil
  "Vector that holds a connection vector for each unit I.
  Each entry in the connection vector holds a unit index J,
  indicating that this connection is from J to I.")
(proclaim '(simple-vector *connections*))

(defvar *weights* nil
  "Vector of vectors, with each entry giving the weight associated
  with connection IJ.")
(proclaim '(simple-vector *weights*))

(defvar *delta-weights* nil
  "Vector of vectors, with each entry giving the change between the previous
  weight and the current one.")
(proclaim '(simple-vector *delta-weights*))

(defvar *slopes* nil
  "Vector of vectors, with each entry giving the accumulated slope value
  at the current position.")
(proclaim '(simple-vector *slopes*))

(defvar *prev-slopes*
  "Vector of vectors, with each entry giving the slope value for the previous
  position.")
(proclaim '(simple-vector *prev-slopes*))


;;;; Network-building utilities.

(defun build-data-structures (ninputs nhidden noutputs)
  "Create the network data structures, given the number of input, hidden
  and output units."
  (setq *nunits* (+ 1 ninputs nhidden noutputs)
	*ninputs* ninputs
	*first-hidden* (+ 1 ninputs)
	*nhidden* nhidden
	*first-output* (+ 1 ninputs nhidden)
	*noutputs* noutputs
	*outputs* (make-array *nunits* :element-type 'short-float
			      :initial-element 0.0)
	*error-sums* (make-array *nunits* :element-type 'short-float
				  :initial-element 0.0)
	*errors* (make-array *nunits* :element-type 'short-float
			     :initial-element 0.0)
	*nconnections* (make-array *nunits* :element-type 'fixnum
				   :initial-element 0)
	*connections* (make-array *nunits* :element-type 'simple-vector)
	*weights* (make-array *nunits* :element-type 'simple-vector)
	*delta-weights* (make-array *nunits* :element-type 'simple-vector)
	*slopes* (make-array *nunits* :element-type 'simple-vector)
	*prev-slopes* (make-array *nunits* :element-type 'simple-vector))
  (setf (svref *outputs* 0) *input-one-value*))

(defun random-weight (range)
  "Select a random weight, an integer uniformly distributed over the
  interval from minus RANGE to plus RANGE, inclusive."
  (- (random (* 2.0 range)) range))

(defun connect-layers (start1 end1 start2 end2
			      &optional (random-range 0))
  "Build connections from every unit in range 1 to every unit in the range 2.
  Also add a connection from unit 0 to every unit in range 2.
  For each connection, select a random initial weight between RANDOM-RANGE
  and its negative."
  (setq *epoch* 0)
  (let ((n (1+ (- end1 start1))))
    (do ((i start2 (1+ i)))
	((>= i end2))
      (let ((c (make-array n :element-type 'fixnum))
	    (w (make-array n :element-type 'short-float))
	    (d (make-array n :element-type 'short-float))
	    (cs (make-array n :element-type 'short-float))
	    (ps (make-array n :element-type 'short-float)))
	(setf (svref *nconnections* i) n)
	(setf (svref *connections* i) c)
	(setf (svref *weights* i) w)
	(setf (svref *delta-weights* i) d)
	(setf (svref *slopes* i) cs)
	(setf (svref *prev-slopes* i) ps)
	(setf (svref c 0) 0)
	(setf (svref w 0) (random-weight random-range))
	(setf (svref d 0) 0.0)
	(setf (svref cs 0) 0.0)
	(setf (svref ps 0) 0.0)
	(do ((j start1 (1+ j))
	     (k 1 (1+ k)))
	    ((>= j end1))
	  (setf (svref c k) j)
	  (setf (svref w k) (random-weight random-range))
	  (setf (svref d k) 0.0)
	  (setf (svref cs k) 0.0)
	  (setf (svref ps k) 0.0))))))

(defun init-weights (&optional (random-range *weight-range*))
  "For each connection, select a random initial weight between RANDOM-RANGE
  and its negative.  Clear delta and previous delta values."
  (dotimes (i *nunits*)
    (declare (fixnum i))
    (let ((w (svref *weights* i))
	  (d (svref *delta-weights* i))
	  (cs (svref *slopes* i))
	  (ps (svref *prev-slopes* i)))
      (dotimes (j (svref *nconnections* i))
	(declare (fixnum j))
	(setf (svref w j) (random-weight random-range))
	(setf (svref d j) 0.0)
	(setf (svref cs j) 0.0)
	(setf (svref ps j) 0.0)))))

(defun clear-slopes ()
  "Save the current slope values as prev-slopes, and clear all the slopes."
  (do ((i *first-hidden* (1+ i)))
      ((= i *nunits*))
    (declare (fixnum i))
    (let ((cs (svref *slopes* i))
	  (ps (svref *prev-slopes* i))
	  (w (svref *weights* i)))
      (dotimes (j (svref *nconnections* i))
	(declare (fixnum j))
	(setf (svref ps j) (svref cs j))
	(setf (svref cs j) (* *decay* (svref w j)))))))


;;;; Learning machinery.

;;; Some key utilities.

;;; Sigmoid and sigmoid prime live in the tightest inner loops, so we make
;;; them macros to save a lot of function calls.

(defmacro sigmoid (activation)
  "The basic sigmoid computation.  Maps sum of input activation into
  a unit output value in the range from 0.0 to 1.0."
  `(cond ((< ,activation -15.0) 0.0)
	 ((> ,activation 15.0) 1.0)
	 (t (/sf (+sf 1.0 (exp (-sf ,activation)))))))

(defmacro sigmoid-prime (output)
  "Compute the derivative of the output with respect to activation at
  the current output value.  Add a small constant to keep the derivative
  from going to zero when error is close to 1.0."
  `(+sf *sigmoid-prime-offset* (*sf ,output (-sf 1.0 ,output))))

(defvar *total-error* 0.0
  "Accumulate the total output error for one epoch.")
(proclaim '(short-float *total-error*))

(defvar *score-threshold* .4
  "To count as correct, a bit's output must be this close to the desired value.")
(proclaim '(short-float *score-threshold*))

(defvar *total-error-bits* 0
  "Count number of bits in epoch that are wrong by more than
  *SCORE-THRESHOLD*")
(proclaim '(fixnum *total-error-bits*))

(defmacro errfun (desired actual)
  "Compute the error for one output unit.
  If *hyper-err* is on, use hyperbolic arctan error function.
  Record the squared error."
  `(let* ((dif (-sf ,desired ,actual)))
     (declare (short-float dif))
     (incf-sf *total-error* (* dif dif))
     (unless (< (abs dif) *score-threshold*)
       (incf *total-error-bits*))
     (cond ((not *hyper-err*)
	    (if (< -0.1 dif 0.1) 0.0 dif))
	   ((< dif -.9999999) -17.0)
	   ((> dif  .9999999)  17.0)
	   (t (log (/sf (+sf 1.0 dif) (-sf 1.0 dif)))))))

;;; The inner loops...

(defun forward-pass (input)
  "Input is a vector of values that become the outputs of the input units.
  Then propagate the values forward through the network."
  ;; Set up all the inputs.
  (dotimes (i *ninputs*)
    (setf (svref *outputs* (1+ i)) (svref input i)))
  ;; For each unit J, add up the incoming activation from all units I,
  ;; Then run it through the sigmoid to produce an output.
  (do ((j *first-hidden* (1+ j))
       (symmetric-offset (if *symmetric* -0.5 0.0)))
      ((= j *nunits*))
    (declare (fixnum j))
    (let ((c (svref *connections* j))
	  (w (svref *weights* j))
	  (sum 0.0))
      (declare (short-float sum))
      (dotimes (i (svref *nconnections* j))
	(declare (fixnum i))
	(incf-sf sum
		 (*sf (svref *outputs* (svref c i))
		      (svref w i))))
      (setf (svref *outputs* j)
	    (+sf symmetric-offset (sigmoid sum))))))

(defun backward-pass (goal)
  "Goal is a vector of desired values for the output units.  Propagate the
  error back through the network, accumulating weight deltas."
  ;; Compare outputs to goal and determine error values.
  (do ((i *first-output* (1+ i))
       (n 0 (1+ n)))
      ((>= i *nunits*))
    (declare (fixnum i n))
    (setf (svref *error-sums* i)
	  (errfun (svref goal n) (svref *outputs* i))))
  ;; Zero the error sums for non-output units.
  (dotimes (i *first-output*)
    (declare (fixnum i))
    (setf (svref *error-sums* i) 0.0))
  ;; Now propagate error back through net.  When this loop reaches unit J,
  ;; all error from later units has been collected.  Do the sigmoid-prime
  ;; calcuation, and pass error back to earlier weights and units.
  (do ((j (1- *nunits*) (1- j))
       (symmetric-offset (if *symmetric* 0.5 0.0)))
      ((< j *first-hidden*))
    (declare (fixnum j))
    (let* ((c (svref *connections* j))
	   (w (svref *weights* j))
	   (cs (svref *slopes* j))
	   (nc (svref *nconnections* j))
	   (o (svref *outputs* j))
	   (err-j (setf (svref *errors* j)
			(*sf (sigmoid-prime (+sf symmetric-offset o))
			     (svref *error-sums* j)))))
      (declare (short-float err-j))
      (dotimes (i nc)
	(declare (fixnum i))
	(let ((i-index (svref c i)))
	  (declare (fixnum i-index))
	  (incf-sf (svref *error-sums* i-index)
		   (*sf err-j (svref w i)))
	  (incf-sf (svref cs i)
		   (*sf err-j
			(svref *outputs* i-index))))))))

(defun update-weights ()
  "Update all the weights in the network as a function of each weight's current
  slope. previous slope, and the distance of the last move."
  (let ((shrink-factor (/sf *mu* (+sf 1.0 *mu*))))
    (declare (short-float shrink-factor))
    (do ((j *first-hidden* (1+ j)))
	((= j *nunits*))
      (declare (fixnum j))
      (let ((w (svref *weights* j))
	    (nc (svref *nconnections* j))
	    (d (svref *delta-weights* j))
	    (cs (svref *slopes* j))
	    (ps (svref *prev-slopes* j)))
	(declare (fixnum nc))
	(dotimes (i nc)
	  (declare (fixnum i))
	  (let* ((ps-i (svref ps i))
		 (cs-i (svref cs i))
		 (d-i (svref d i))
		 (next-step 0.0))
	    (declare (short-float next-step ps-i cs-i d-i)) 
	    (cond
	     ;; If last step was positive...
	     ((plusp d-i)
	      ;; Add in epsilon if current slope is positive.
	      (when (plusp cs-i)
		(incf next-step (if *split-epsilon*
				(/sf (*sf *epsilon* cs-i) nc)
				(*sf *epsilon* cs-i)))) 
	      (cond
	       ;; If current slope is close to or larger than prev slope...
	       ((> cs-i (*sf shrink-factor ps-i))
		;; Take maximum size positive step.
		(incf-sf next-step (*sf *mu* d-i)))
	       ;; Else, use quadratic estimate.
	       (t (incf-sf next-step (*sf (/sf cs-i (-sf ps-i cs-i)) d-i)))))
	     ;; If last step was significantly negative...
	     ((minusp d-i)
	      ;; Add in epsilon if current slope is negative.
	      (when (minusp cs-i)
		(incf next-step (if *split-epsilon*
				(/sf (*sf *epsilon* cs-i) nc)
				(*sf *epsilon* cs-i))))
	      (cond
	       ;; If current slope is close to or more neg than prev slope...
	       ((< cs-i (*sf shrink-factor ps-i))
		;; Take maximum size negative step.
		(incf-sf next-step (*sf *mu* d-i)))
	       ;; Else, use quadratic estimate.
	       (t (incf-sf next-step (*sf (/sf cs-i (-sf ps-i cs-i)) d-i)))))
	     (t (incf next-step (if *split-epsilon*
				    (/sf (*sf *epsilon* cs-i) nc)
				    (*sf *epsilon* cs-i)))))
	    (setf (svref d i) next-step)
	    (incf-sf (svref w i) next-step)))))))

(defun train-one-epoch ()
  "Perform forward and back propagation once for each set of weights in the
  training vectors, collecting deltas.  Then burn in the weights."
  (clear-slopes)
  (dotimes (i (length *training-inputs*))
;;    (system:serve-all)
    (forward-pass (svref *training-inputs* i))
    (backward-pass (svref *training-outputs* i))
    (when *single-pass*
      (loop
	(when (or (not *single-pass*) *step*)
	  (setq *step* nil)
	  (return nil))
;;	(system:server 1)
	))
    (when *graphics* (update-pass-displays)))
  (update-weights)
  (incf *epoch*)
  (when *graphics* (update-epoch-displays))
  (when (and *single-epoch* (not *single-pass*))
    (loop
      (when (or (not *single-epoch*) *step*)
	(setq *step* nil)
	(return nil))
;;      (system:server 1)
	)))

(defun train-test (times max &optional (report nil))
  "Train the network until there are 0 bits wrong, then print a message.
  If any given test reaches MAX epochs, restart or abort, depending on
  *RESTART* swtich.  Repeat all this for the specified number of TIMES."
  (let ((total-epochs 0)
	(total-restarts 0)
	(esquared 0)
	(maxepochs 0)
	(minepochs max)
	(newmax nil))
    (dotimes (i times)
      (setq *epoch* 0)
      (init-weights)
      (setq newmax max)
      (loop
	(when (>= *epoch* newmax)
	  (if *restart*
	      (progn
		(incf newmax max)
		(init-weights)	
		(format t "Trial ~3D:  Restart after ~D epochs.~%" i *epoch*)
		(incf total-restarts))
	      (progn
		(format t "Trial ~3D:  Abort after ~D epochs.~%" i *epoch*)
		(incf total-restarts)
		(incf total-epochs newmax)
		(incf esquared (* newmax newmax))
		(setq maxepochs newmax)
		(return nil))))
	(setq *total-error* 0.0)
	(setq *total-error-bits* 0)
	(train-one-epoch)
	(when (and report (zerop (mod (1- *epoch*) report)))
	  (format t "Trained ~D epochs, ~D bits wrong, error = ~S.~%"
		  (1- *epoch*) *total-error-bits* *total-error*))
	(when (zerop *total-error-bits*)
	  (decf *epoch*)
	  (incf total-epochs *epoch*)
	  (incf esquared (* *epoch* *epoch*))
	  (setq maxepochs (max *epoch* maxepochs))
	  (setq minepochs (min *epoch* minepochs))
	  (format t "Trial ~3D:  Learned after ~3D epochs.  Running Avg: ~,2F~%"
		  i *epoch* (/ (coerce total-epochs 'short-float) (1+ i)))
	  (return nil))))
    (format t "Eps ~,2F~A, Mu ~,2F, WtRng ~,1F, Decay ~7F, SigOff ~,2F, Hyper ~S, Sym ~S~%"
	    *epsilon* (if *split-epsilon* "*" "") *mu* *weight-range* *decay*
	    *sigmoid-prime-offset* *hyper-err* *symmetric*)
    (when (> times 1)
      (format t "ReStrt ~D, Max ~D, Min ~D, Avg ~,2F, SD ~,2F.~%"
	      total-restarts maxepochs minepochs
	      (/ (coerce total-epochs 'short-float) times)
	      (sqrt (/ (- (* times esquared) (* total-epochs total-epochs))
		       (* times (1- times))))))))


;;;; Setup modification utilities.

;;; In order to convert from the normal assymmetric activation function to
;;; a symmetric one, several values have to be altered and the network has
;;; to be rebuilt.  Use these functions so that you don't forget any of
;;; these things and get spurious results.

(defun make-symmetric ()
  "Convert the network to use a symmetric activation function ranging
  from -0.5 to +0.5 instead of 0.0 to 1.0."
  (when *symmetric*
    (sysbeep)
    (return-from make-symmetric nil))
  (setq *symmetric* t)
  (decf *input-zero-value* 0.5)
  (decf *input-one-value* 0.5)
  (decf *output-zero-value* 0.5)
  (decf *output-one-value* 0.5)
  "Remember to rebuild the current network.")

(defun make-asymmetric ()
  "Convert the network to use an asymmetric activation function ranging
  from 0.0 to 1.0 instead of -0.5 to +0.5."
  (unless *symmetric*
    (sysbeep)
    (return-from make-asymmetric nil))
  (setq *symmetric* nil)
  (incf *input-zero-value* 0.5)
  (incf *input-one-value* 0.5)
  (incf *output-zero-value* 0.5)
  (incf *output-one-value* 0.5)
  "Remember to rebuild the current network.")

;;; Use this to complement all the input and output values for the current
;;; training and testing patterns.  Flip each value around the midpoint
;;; between logical one and logical zero.

(defun complement-patterns ()
  "For all the training and testing patterns, exchange logical one values
  and logical zero values.  Other values reflect around the midpoint."
  (let ((ival (+ *input-zero-value* *input-one-value*))
	(oval (+ *output-zero-value* *output-one-value*)))
    (dotimes (i (length *training-inputs*))
      (dotimes (j *ninputs*)
	(setf (svref (svref *training-inputs* i) j)
	      (- ival (svref (svref *training-inputs* i) j)))))
    (dotimes (i (length *training-outputs*))
      (dotimes (j *noutputs*)
	(setf (svref (svref *training-outputs* i) j)
	      (- oval (svref (svref *training-outputs* i) j)))))
    (dotimes (i (length *test-inputs*))
      (dotimes (j *ninputs*)
	(setf (svref (svref *test-inputs* i) j)
	      (- ival (svref (svref *test-inputs* i) j)))))
    (dotimes (i (length *test-outputs*))
      (dotimes (j *noutputs*)
	(setf (svref (svref *test-outputs* i) j)
	      (- oval (svref (svref *test-outputs* i) j)))))))

;;;; Example

;;; The code to build an X-Y-X encoder looks like this.
;;; Display code has been omitted.

;;; (defun build-encoder (x y)
;;;   "Build an ecoder with X input units, X output units, and Y units in the
;;;   layer connecting them."
;;;   (build-data-structures x y x)
;;;   (connect-layers 1 (+ x 1) (+ x 1) (+ x y 1) *weight-range*)
;;;   (connect-layers (+ x 1) (+ x y 1) (+ x y 1) (+ x y x 1) *weight-range*)
;;;   (setq *training-inputs* (make-array x))
;;;   (setq *training-outputs* (make-array x))
;;;   (dotimes (i x)
;;;     (let ((v (make-array x :initial-element *input-zero-value*)))
;;;       (setf (svref v i) *input-one-value*)
;;;       (setf (svref *training-inputs* i) v))
;;;     (let ((v (make-array x :initial-element *output-zero-value*)))
;;;       (setf (svref v i) *output-one-value*)
;;;       (setf (svref *training-outputs* i) v))))

;;; To run this, do something like (train-test 10 200).
