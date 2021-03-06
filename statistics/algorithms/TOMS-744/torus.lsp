;;; torus.lisp
;;; This set of functions stochastically adjusts the values of the given
;;; variables in an attempt to either minimize or maximize any mathematical
;;; function or computer model that returns a single numerical value.

(in-package "USER")

(defvar *bump* 1/2); size of bump, number is fraction of range

(defvar *counter1* 4); simulates the number of parallel processors

(defvar *counter2* 40); maximum number of iterations of pass2

(defvar *exit* .000001); change must be less than value to exit

(defvar *scalar1* 1); multiplies single-dimen iterations

(defvar *scalar2* 1); multiplies snake iterations

(defvar *shrink-hit* 1.5); rate the torus collapses after a hit

(defvar *shrink-trial* 1.5); rate the torus collapses between trials

(defvar *torus* 4000);  range of torus




(defmacro control (integer-float lower upper cutoff
			   fname minmax &optional start-set)
  "Returns the maximum or minimum and the associated variable values.
  Parameters (integer-float lower upper cutoff fname minmax
  &optional start-set).  Integer-float refers to a list containing the words
  integer, float, or double depending on how each variable is to be treated,
  lower [upper] to a list of lower [upper] bounds of the variables in the same
  order as the variables are defined in the function to be maximized or
  minimized, cutoff is a list of smallest change of interest for each
  variable, fname is a the name of the function to be maximized or
  minimized, minmax is a word, either min or max, and start-set is the
  optional list of starting values of the variables. For example,
  (control (double double) (-2000 -2000)
	   (2000 2000) (.0000001 .0000001)
	   rosenbrock2a min (1001 1001))
  would be the appropriate format for calling the control macro to find the
  minimum of the function rosenbrock2a."

  `(control-internal ',integer-float ',lower ',upper
 		     ',cutoff ',fname ',minmax ',start-set))



(defun control-internal (integer-float lower upper cutoff
				 fname minmax &optional start-set)
  "This function uses the same variables as the control macro except that they
  need to be quoted (e. g., 'min instead of min)  Start-Values, ranges, and
  the iterate variable are calculated before pass2 is called to find the
  solution.  An example of a direct call would be:
  (control-internal '(double double) '(-2000 -2000)
		   '(2000 2000) '(.0000001 .0000001)
		   'rosenbrock2a 'min '(1001 1001))
  This example is the counterpart of the control call listed in the
  control macro."


  (let* ((start-value (cond (start-set start-set); use start-set if provided
			    ;;calculate middle value on each variable if a
			    ;;start-set is not provided
			    (t (mapcar #'(lambda (x5 y5 z5)
					   (mode (/ (+ x5 y5) 2) z5))
				       lower upper integer-float))))

	 (numvar (length lower)); number of variables

	 (mintorus (mapcar #'(lambda (x y); minum size of outside of torus
			       (cond ((equal x 'integer) (* 2 y))
				     ((equal x 'float) (* 32 y))
				     (t (* 64 y))))
			   integer-float cutoff))

	 (range (mapcar #'(lambda (y z) (- y z))
			upper lower)); list of variable ranges

	 (cutoff-alt (mapcar #'(lambda (x y); minimum cutoff values for pass1
				 (cond ((< (/ x *torus*) y) y)
				       (t (/ x *torus*)))) range cutoff))

	 (iterate-single
	  (round (* *scalar1* 10))); iterations per variable single-alt

	 (iterate-multi
	  (round (* 10 *scalar2* (expt numvar 2)))); iterations multi-dimen

	 (single-dimen-count (* *counter1* iterate-single))
		       ; number of function calls single-dimen-alt

	 (multi-dimen-count (* *counter1* iterate-multi))
	               ; function calls multi-dimen

	 (pass1 (do ((hold nil; first pass to find start-point and startset
			   (cons (multi-dimen
				    integer-float
				   lower upper cutoff-alt fname
				   minmax start-value range
				   iterate-multi)
				   hold))
		     (count 0 (1+ count)))
		    ((= count *counter1*)
		     ;;(print-all 1 hold)
		     hold)))

	 (start-point (apply minmax (mapcar #'car pass1)))
	              ; initial best-value for pass2

	 (startset (mapcar #'(lambda (x y) (mode x y)); starting set for pass2
			    (cadr (assoc start-point pass1))
			    integer-float)); initial set

	 (pass2 (do* ((within-trial 1; new trial call single-dimen
				    (cond ((and (zerop flagz)
						(> within-trial 1))
					   within-trial);success - try again
					  ((= within-trial 3) 1); new trial
					  (t (1+ within-trial)))); call snake

		      ;; a within-trial-count value of 2 or more reflects
		      ;; snake-fit executed on previous trial
		      (within-trial-count 0
					  (cond ((> within-trial 1)
						 (1+ within-trial-count))
						(t 0)))

		      ;;To prevent a trap with too many small improvements
		      ;;The inclusion of glagz-count is very conservative
		      (flagz-count 0
				   (cond ((zerop flagz); improvement
					  (1+ flagz-count))
					 (t 0)))

		      ;;The number of complete trials
		      (count 0
			     (cond ((= within-trial 1) (1+ count)); new trial
				   (t count)))

		      ;;When a trial is completed the bumped variable changes
		      (nextv 0 (rem count numvar)); select variable to bump

                      ;; used to control the direction of single-dimen algorithm
		      ;; change direction on every pass
		      (flagdir 0 (cond ((zerop flagdir) 1)
				       (t 0)))

		      ;; set minimum size of delta for next iteration
		      (cut cutoff-alt
			   (cond ((and (> within-trial-count 1); after snake
				       (zerop flagz)); hit
				  (mapcar #'(lambda (x y)
					      (cond ((< (/ x *shrink-hit*) y)
						     y)
						    (t (/ x *shrink-hit*))))
					  cut cutoff))
				 ((= within-trial 1); new trial
				  (mapcar #'(lambda (x y)
					      (cond ((< (/ x *shrink-trial*)
							y) y)
						    (t (/ x *shrink-trial*))))
					  cut cutoff))
				 (t cut)))

		      ;;range of each variable for next iteration
		      (new-range range
			    (cond ((and (> within-trial-count 1); after snake
					(zerop flagz)); hit
				   (mapcar #'(lambda (x y)
					       (cond ((< (/ x *shrink-hit*)
							 y) y)
						     (t (/ x *shrink-hit*))))
					   new-range mintorus))
				  ((= within-trial 1); around
				   (mapcar #'(lambda (x y)
					       (cond ((< (/ x *shrink-trial*)
							 y) y)
						     (t (/ x *shrink-trial*))))
					   new-range mintorus))
				  (t new-range)))

		      ;;set bump direction if multi-dimen finds a better set
		      ;;otherwise bump up if within-trial = 2
		      ;;change the direction if within-trial = 3
		      ;;only needs to be done for the variable bumped
		      (down-up nil
			(cond ((and (> within-trial-count 1); after snake
				    (zerop flagz)); come back with a winner
			       (cond ((< (nth nextv (cadr hold))
					 (nth nextv lastset)) nil); nil is down
				     (t t))); true is up
			      ((= within-trial 2) t); bump up
			      (down-up nil); if up change to down
			      (t t))); if down - up on within-trial 3

                      ;; only one variable bumped
		      ;; magnitude bump = (* range *bump*)
		      (bump nil
			    (cond (down-up (* (nth nextv new-range) *bump*)); up
				  (t (* (nth nextv new-range)
					-1 *bump*)))); down

		      ;;a list containing zeros and bump value
	              (bumpset (do ((countb 1 (1+ countb))
				    (hld '(0) (cons 0 hld)))
				   ((= countb numvar) hld))
			       (do* ((countb 0 (1+ countb))
				     (hld (cond ((= nextv countb)
						 (list bump))
						(t '(0)))
					  (cond ((= nextv countb)
						 (cons bump hld))
						(t (cons 0 hld)))))
				    ((= countb (1- numvar)) (reverse hld))))

		      ;; best value so far
		      (lasthold start-point
				(cond ((> flagz 0) lasthold)
				      (t (car hold))))

		      ;;best set
		      (lastset startset
			       (cond ((> flagz 0) lastset)
				     (t (mapcar #'(lambda (x y)
						    (mode x y))
						(cadr hold) integer-float))))

		      ;;set used for next iteration
		      ;;if within-trial = 1 or bumped out of range, lastset
		      ;;bumped added if within range, subtracted otherwise
		      (nextset startset
			       (cond ((= within-trial 1) lastset)
				     ((and
					(> (nth nextv upper)
					   (+ (nth nextv lastset)
					      bump));in range on top
					(< (nth nextv lower)
					   (+ (nth nextv lastset)
					      bump)));in range bottom
				      (mapcar #'(lambda (x y)
						  (mode x y))
					      (mapcar #'+ lastset
						      bumpset)
					      integer-float))
				     ((and
					(> (nth nextv upper)
					   (- (nth nextv lastset)
					      bump));in range on top
					(< (nth nextv lower)
					   (- (nth nextv lastset)
					      bump)));in range bottom
				      (mapcar #'(lambda (x y)
						  (mode x y))
					      (mapcar #'- lastset
						      bumpset)
					      integer-float))
				     (t lastset)))

		      ;;index of the number of function calls
		      (iterate-num multi-dimen-count
				   (+ iterate-num single-dimen-count
				      (cond ((= within-trial 1)
					     0); single-dimen only
					    (t multi-dimen-count))))

		      ;;calls either multi-dimen or single-alt
		      ;;single-alt, in turn, calls single-dimen-alt
		      (hold (single-alt  integer-float
				       lower upper cut fname
				       minmax startset new-range
				       iterate-single *counter1*
				       count nextv)
			    (cond ((> within-trial 1); multi-dimen
				   (do ((hold1 nil
					(cons (multi-dimen
						 integer-float
						lower upper cut
						fname minmax nextset
						new-range
						iterate-multi)
					      hold1))
					(count1 0 (1+ count1)))
				       ((= count1 *counter1*)
					;;(print-all 1 hold1)
					(let ((temphold
						(assoc (apply minmax
							       (mapcar #'car
								       hold1))
						       hold1)))
					  (single-alt  integer-float
						     lower upper cut fname
						     minmax
						     (mapcar #'(lambda (x y)
								 (mode x y))
							     (cadr temphold)
							     integer-float)
						     new-range iterate-single
						     *counter1* flagdir
						     nextv)))))
				  (t (single-alt  integer-float
						lower upper cut fname minmax
						nextset new-range iterate-single
						*counter1* flagdir nextv))))

		      ;;returns 0 if better value found, otherwise 1+ flagz
		      (flagz (cond ((= (funcall minmax lasthold
					  (car hold)) lasthold) 1)
				   (t 0))
			     (cond ((= (funcall minmax lasthold
					  (car hold)) lasthold) (1+ flagz))
				   (t 0))))
		     ((or (= count *counter2*)
			  (= flagz-count 24); too many consecutive successes
			  (= flagz 36); too many consecutive failures
			  (and (< (abs (- lasthold (car hold))) *exit*)
			       (= flagz 0)))
		      (cond ((zerop flagz) (cons iterate-num hold))
			    (t (cons iterate-num (list lasthold lastset))))))))
    pass2))



;;;simulates parallel processing by calling single-dimen-alt the number of
;;;times specified by *counter1*
(defun single-alt (integer-float lower upper cutoff fname minmax
			   start-value halfrange iterate-var
			   totcount odd-even bumped-variable)
  (let* ((pass (do ((temp (list (single-dimen-alt  integer-float
					    lower upper cutoff fname
					    minmax start-value halfrange
					    iterate-var
					    odd-even bumped-variable))
			  (cons (single-dimen-alt  integer-float
					    lower upper cutoff fname
					    minmax start-value halfrange
					    iterate-var
					    odd-even bumped-variable)
				temp))
		    (count1 1 (1+ count1)))
		   ((= count1 totcount) temp)))
	 (pass-minmax (apply minmax (mapcar #'car pass))))
    (assoc pass-minmax pass)))


;;; calls single-dimen starting with either 1+ or 1- bumped variable
;;; then cycles through all variables maintaining direction
(defun single-dimen-alt (integer-float lower upper cutoff fname minmax
			    start-value halfrange iterate-var
			    odd-even bumped-variable)
  (let* ((len (length lower)))
    (cond ((zerop odd-even)
	   (do* ((count2 1 (1+ count2))
		 (count3 (mod (1+ bumped-variable) len)
			 (mod (+ count2 bumped-variable) len))
		 (answer (single-dimen  integer-float
				 lower upper cutoff fname
				 minmax start-value halfrange
				 iterate-var count3)
			 (single-dimen  integer-float
				 lower upper cutoff fname
				 minmax (cadr answer) halfrange
				 iterate-var count3)))
		((= count2 len) answer)))
	  (t (do* ((count2 1 (1+ count2))
		   (count3 (mod (1- bumped-variable) len)
			 (mod (- bumped-variable count2) len))
		   (answer (single-dimen  integer-float
				   lower upper cutoff fname
				   minmax start-value halfrange
				   iterate-var count3)
			   (single-dimen  integer-float
				   lower upper cutoff fname
				   minmax (cadr answer) halfrange
				   iterate-var count3)))
		  ((= len count2) answer))))))




;;; mode function can be modified to include more data types
(defun mode (i-f integer-or-float)
  "The function returns either a rounded integer, a single-floating point
  number, or a double-floating point number.  Parameters (number
  integer-or-float) where integer-or-float is either the word 'integer, the
  word 'float, or the word 'double."
  (cond ((equal integer-or-float 'integer) (round i-f))
	((equal integer-or-float 'float) (float i-f))
        (t (float i-f 1.0d0))))


(defun single-dimen (integer-float lower upper cutoff fname minmax
			      start-value halfrange iterate-var variab)
  "This is a Monte Carlo algorithm for changing the value of one variables.
  Annealing principles are used in calculating the maximum range of the
  variables on each iteration.  Fast cooling is implemented as the range is
  logarithmically reduced.  Randomness is introduced into the fit
  by multiplying the maximum range of the fit variable by (random 1.0) to
  compute the delta value for each iteration.  The best fitting score and
  related variable set are returned by the function.  Parameters (list-of-
  variable-names list-of-integer-float list-of-lower-bounds list-of-upper-
  bounds list-of-cutoff-values list-of-function-name-and-variables the-word-
  min-or-max list-of-starting-values list-of-halfranges iterate-variable)."

  (let ((log-constant (log iterate-var)); for bounding between 0 and 1
	(len1 (length lower))); for constructing variable list, see var below

    (do* ((iterate 1
		   (cond (var-flag iterate); new variable set same as last
			 (t (1+ iterate)))); increment iterate counter

	  (iterate-delta 1 (- 1 (/ (log iterate) log-constant))); weights range

	  ;; maximum delta value decremented across iterations
	  (delta nil (let ((x-temp (* iterate-delta
				      (nth variab halfrange) (random 1.0))))
		       (cond ((and (equal (nth variab integer-float)
					  'integer)
				   (< x-temp 1)) 1); minimum value for integer
			     ((>= x-temp (nth variab cutoff));larger than cutoff
			      (mode x-temp (nth variab integer-float))); ok
			     (t (setq x-temp (* 16 (nth variab cutoff)
						(random 1.0)))
				(cond ((>= x-temp (nth variab cutoff))
				       (mode x-temp (nth variab
							 integer-float))); ok
				      (t (mode (nth variab cutoff)
					       (nth variab
						    integer-float))))))));cutoff

	  ;; random 1s and 0s to determine if delta values
	  ;; are to be added or subtracted
	  (newvalue nil (cond ((zerop (random 2))
			       (cond ((> (- (nth variab var-old) delta)
					 (nth variab lower))
				      (- (nth variab var-old) delta))
				     ((< (+ (nth variab var-old) delta)
					   (nth variab upper))
				      (+ (nth variab var-old) delta))
				     (t (nth variab var-old))))
			      (t (cond ((< (+ (nth variab var-old) delta)
					   (nth variab upper))
					(+ (nth variab var-old) delta))
				       ((> (- (nth variab var-old) delta)
					   (nth variab lower))
					(- (nth variab var-old) delta))
				       (t (nth variab var-old))))))

	  ;; construct a new variable set for next iteration
	  (var start-value
		 (do* ((cnt 0 (1+ cnt))
		       (hld (list (cond ((= cnt variab) newvalue)
					(t (nth cnt var-old))))
			    (cons (cond ((= cnt variab) newvalue)
					(t (nth cnt var-old)))
				  hld)))
		      ((= cnt (1- len1)) (reverse hld))))

	  ;; var-flag set to T when var values are the same as var-last
	  (var-flag nil (equal var var-old))

	  ;; value holds the value returned by fname
	  (value (apply #'funcall (cons fname start-value))
		 (cond (var-flag value)
		       (t (apply #'funcall (cons fname var)))))

	  ;; value-flag set to T when better fit is obtained
	  (value-flag nil (= (funcall minmax value-old value) value))

	  ;; value-old holds the best value
	  (value-old value (cond (value-flag value)
				 (t value-old)))

	  ;; var-old holds the best fitting variable set
	  (var-old start-value (cond (value-flag var)
				       (t var-old))))

	 ;; exit test, iterations used up
	 ((= iterate-var iterate) (list value-old var-old)))))


(defun multi-dimen (integer-float lower upper cutoff fname minmax
				 start-value halfrange iterate-var)
  "This is a Monte Carlo algorithm for changing the values of all the
  variables.  Annealing principles are used in calculating the maximum range
  of the variables on each iteration.  Fast cooling is implemented as the
  range is logarithmically reduced.  Randomness is introduced into the fit
  by multiplying the maximum range of the fit variable by (random 1.0) to
  compute the delta value for each iteration.  The best fitting score and 
  related variable set are returned by the function.  Parameters (list-of-
  variable-names list-of-integer-float list-of-lower-bounds list-of-upper-
  bounds list-of-cutoff-values list-of-function-name-and-variables the-word-
  min-or-max list-of-starting-values list-of-halfranges iterate-variable)."

  (let ((number-var (1- (length lower))); for constructing ranlist
	(log-constant (log iterate-var))); for bounding between 0 and 1

    (do* ((iterate 1
		   (cond (var-flag iterate); new variable set same as last
			 (t (1+ iterate)))); increment iterate counter

	  (iterate-delta 1 (- 1 (/ (log iterate) log-constant))); weights range


	  ;; maximum delta value decremented across iterations
	  (delta nil (mapcar #'(lambda (x y z)
				 (let ((x-temp (* iterate-delta
						  x (random 1.0))))
				   (cond ((and (equal z 'integer)
					       (< x-temp 1)) 1); minimum value
					 ((> x-temp y) (mode x-temp z)); ok
					 (t (setq x-temp (* 4 y (random 1.0)))
					    (cond ((> x-temp y)
						   (mode x-temp z)); ok
						  (t (mode y z))))))); cutoff
			     halfrange cutoff integer-float))

	  ;; ranlist consists of random 1s and 0s to deter if delta values
	  ;; are to be added or subtracted
	  (ranlist nil (do ((c1 number-var (1- c1))
			    (ranlst (cons (random 2) '())
				    (cons (random 2) ranlst)))
			   ((zerop c1) ranlst)))

	  ;; v is a random number, 0 or 1.
	  ;; w is a variable value from last iteration
	  ;; x is a lower bound
	  ;; y is an upper bound
	  ;; z is the delta value
	  (var start-value
		 (mapcar #'(lambda (v w x y z)
			     (cond ((zerop v)
				    (cond ((> (- w z) x) (- w z))
					  (t w)))
				   (t (cond ((< (+ w z) y)
					     (+ w z))
					    (t w)))))
			 ranlist var-old lower upper delta))

	  ;; var-flag set to T when var values are the same as var-last
	  (var-flag nil (equal var var-old))

	  ;; value holds the value returned by fname
	  (value (apply #'funcall (cons fname start-value))
		 (cond (var-flag value)
		       (t (apply #'funcall (cons fname var)))))

	  ;; value-flag set to T when better fit is obtained
	  (value-flag nil (= (funcall minmax value-old value) value))

	  ;; value-old holds the best value
	  (value-old value (cond (value-flag value)
				 (t value-old)))

	  ;; var-old holds the best fitting variable set
	  (var-old start-value (cond (value-flag var)
				       (t var-old))))

	 ;; exit test, iterations used up
	 ((= iterate-var iterate) (list value-old var-old)))))

(defun rosenbrock2-6 ()
  "This function calls the control-internal macro to initiate the fit
   process.  It does not require any parameters and is included as an
   example."
  (control-internal '(double double) '(-2000 -2000)
		    '(2000 2000) '(.0000001 .0000001)
		    'rosenbrock2a 'min '(1 1443)))

(defun rosenbrock2a (x1 x2)
  "This function computes the Rosenbrock function with two variables. The
   parameters are the values of the two variables.  It is called by the
   function rosenbrock2-6 and is included as an example."
    (let* ((y1 (- x2 (* x1 x1)))
	          (y2 (1- x1)))
          (+ (* 100 y1 y1) (* y2 y2))))
