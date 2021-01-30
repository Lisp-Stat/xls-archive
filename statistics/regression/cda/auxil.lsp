;;;; Auxiliary functions for regmodel.lsp, rat.lsp and simul.lsp
;;;; written by Julian Faraway, Department of Statistics,
;;;; University of Michigan.
;;
;; a list of possible transformations
(defvar *fun-list* '((0.5 sqrt)
		     (0 log)
		     (-0.5 invsqrt)
		     (-1 recip)
		     (2 square)
		     (-2 invsquare)
		     (-1.5 inv32)
		     (1.5 f32)))
;; The derivative of the inverses of some functions
(defvar *deriv-inv* '((IDENTITY (lambda (x) 1))
		      (LOG EXP)
		      (SQUARE (lambda (x) (* 0.5 (invsqrt x))))
		      (F32 (lambda (x) (* (/ 2 3) (^ x (/ -1 3)))))
		      (SQRT (lambda (x) (* 2 x)))
		      (INV32 (lambda (x) (* (/ -2  3) (^ x (/ -5 3)))))
		      (INVSQUARE (lambda (x) (* -0.5 (^ x -1.5))))
		      (INVSQRT (lambda (x) (* -2 (^ x -3))))
		      (RECIP (lambda (x) (- (^ x -2))))))
;; The derivative of some functions
(defvar *deriv* '((IDENTITY (lambda (x) 1))
	        (LOG recip)
		(EXP exp)
	        (RECIP (lambda (x) (* -1 (^ x -2))))
	        (INV32 (lambda (x) (* -1.5 (^ x -2.5))))
	        (F32 (lambda (x) (* 1.5 (sqrt x))))
	        (INVSQRT (lambda (x) (* -0.5 (^ x -1.5))))
	        (INVSQUARE (lambda (x) (* -2 (^ x -3))))
	        (SQUARE (lambda (x) (* 2 x)))
	        (SQRT (lambda (x) (/ 1 (* 2 (sqrt x)))))))
;; The inverse of some functions
(defvar *inverses* '((IDENTITY IDENTITY)
		     (SQRT SQUARE)
		     (LOG EXP)
		     (INVSQRT INVSQUARE)
		     (RECIP RECIP)
		     (SQUARE SQRT)
		     (INVSQUARE INVSQRT)
		     (INV32 (lambda (x) (^ x (/ -2 3))))
		     (F32 (lambda (x) (^ x (/ 2 3))))))
(defun what-list (l i)
"Args: (l i) 
Returns the index of the list and the index of the element within that
list containing the ith nonnull element from list of lists l"
  (let ((j 0)(ln 0))
    (dolist (k l (list ln 0))
	    (cond ((not (equal k '(null)))
		   (setf j (+ j (length k)))
		   (if (> j i) 
		       (return (list ln (- (+ i (length k)) j)))
		     (setf ln (1+ ln))))
		  (t (setf ln (1+ ln)))))))

;; Function to initialize transform list
(defun init-transform-list (i)
    (repeat '((identity)) (+ 1 i)))

;; Transformation functions
(defun invsqrt (x)
  (/ 1 (sqrt x)))

(defun recip (x)
  (/ 1 x))

(defun square (x)
  (* x x))

(defun invsquare (x)
  (/ 1 (square x)))

(defun f32 (x)
  (^ x 1.5))

(defun inv32 (x)
  (/ 1 (f32 x)))


(defun construct-transformed-design-matrix (x tlist)
  "Args: (x) 
Returns a list representing the current-design matrix given
original data x and transform-list tlist"
  (let ((z NIL))
    (dotimes (i (length x) 
		(if z (apply #'bind-columns (reverse z)) nil))
	     (setf z (append (construct-transformed-predictor (nth i x) i tlist) z)))))

(defun construct-transformed-predictor (x i tlist)
  "Args: (x i tlist 
Returns a list of the ith list of functions from transform-list
tlist applied to original variable x"
  (let ((flist (nth (+ i 1) tlist))
	(z NIL))
    (dolist (j flist (if (null (car z)) () z))
	    (setf z (cons (apl j x) z)))))

; auxiliary to construct-transformed-predictor
(defun apl (f x)
    (if (or (atom f) (= (length f) 1))
	(funcall (if (atom f) f (car f)) x)
      (funcall (car f) (apl (car (last f)) x))))

;; Deletes column i from a matrix x
(defun delete-column (x i)
"Args: (x i)
Deletes column i from matrix x"
  (let ((row-dim (array-dimension x 0))
	(col-dim (array-dimension x 1)))
    (select x (iseq 0 (- row-dim 1)) 
	   (which (/= i (iseq 0 (- col-dim 1)))))))

;; Inserts vector v just before column i (starting from 0) in a matrix x
(defun insert-column (x v i)
"Args: (x v i)
Inserts vector v just after column i in a matrix x"
  (let ((row-dim (array-dimension x 0))
	(col-dim (array-dimension x 1)))
    (if (/= i col-dim)
	(bind-columns
	 (select x (iseq 0 (- row-dim 1)) (iseq 0 (- i 1)))
	 v
	 (select x (iseq 0 (- row-dim 1)) (iseq i (- col-dim 1))))
      (bind-columns x v))))
	
;; inverts the y-transformation
(defun inverse-apply (f x)
  "Args: (f x) Applies inverse of f to x"
  (let ((invf (assoc f *inverses*)))
    (if (null invf)
	(error "Could not invert y-transform ~A in inverse-apply" f))
    (funcall (nth 1 invf) x)))

(defun name-elim (pn tlist i)
  "Args: (r i)
reg-model r  and index i returns
name eliminated variable"
  (let* ((i1 (nth 1 i))
	 (i2 (nth 2 i))
	 (tsl (nth i1 (cdr tlist)))
	 (tn (nth i2 tsl)))
   (if (equal tn 'IDENTITY)
	(format nil "~a was eliminated" (nth i1  pn))
      (format nil "~a ~a was eliminated" tn (nth i1  pn)))))

(defun smoo-bisamp (x y &key h)
"Args: (x y &key h)
Returns (optionally smoothed with bandwidth h) predictors x
and response y"
  (let* ((n (length y))
	 (var-x (let ((j nil))
		  (dolist (i x (reverse j))
			  (setf j (cons (^ (standard-deviation i) 2) j)))))
	 (var-y (^ (standard-deviation y) 2))
	 (i (resample-indices n)))
    (list
     (let ((j nil))
       (dotimes (k (length x) (reverse j))
		(setf j (cons (if h
				  (/ (+ (* h (normal-rand n)) (resample-x (nth k x) i))
				     (sqrt (+ 1 (/ (^ h 2) (nth k var-x)))))
				(resample-x (nth k x) i))
			      j))))
     (if h
	 (/ (+ (* h (normal-rand n)) (resample-y y i))
	    (sqrt (+ 1 (/ (^ h 2) var-y))))
       (resample-y y i)))))

;;Auxiliary functions to smoo-bisamp
(defun resample-indices (l)
  (random (repeat l l)))

(defun resample-y (y l)
  (let ((z nil))
    (dolist (i l (reverse z))
	    (setf z (cons (nth i y) z)))))

(defun resample-x (x l)
  (let ((z nil))
    (dolist (i l (reverse z))
	    (setf z (cons (elt x i) z)))))


;; Function to calculate rmse's
(defun rmse (l x)
  (sqrt (/ (sum (^ (- l x) 2)) (length l))))
;; Calculate simulation errors
(defun simse (l)
  (/ (standard-deviation l) (sqrt (- (length l) 1))))
(defun iqr (x)
  "Args: (x) Returns interquartile range of x"
  (- (quantile x 0.75) (quantile x 0.25)))



;; Functions for calculating jacknife bias and se
(defun jack-bias (x e)
  (* (- (length x) 1) (- (mean x) e)))
(defun jack-se (x)
  (let ((n (length x))
	(m (mean x)))
    (sqrt (* (/ (- n 1) n)
	     (sum (^ (- x m) 2))))))
(defun wjack-se (x w)
  (let ((m (mean x)))
    (sqrt (/ (sum (* w (^ (- x m) 2))) (sum w)))))


;; Functions for calculating the interpreted value of
;; the parameter estimates.
(defun decon-interpret (e tlist x y vb-select)
  (let ((z nil)
	(index 0)
	(lx (if (listp (car x)) x (list x)))
	(ly (if (listp y) y (list y)))
	(ytf (caar tlist)))
    (dotimes (i (length vb-select) (reverse z))
	     (let ((xtf (nth (1+ i) tlist)))
	     (if (nth i vb-select)
		 (setf index (+ index (if (equal xtf '(NULL)) 0	(length xtf))))
	       (progn
		 (setf z (cons 
		 (let ((w nil))
		   (dotimes (j (length ly) (reverse w))
			    (setf w (cons (* (nth index e) 
				  (car (interpret-estimates ytf 
						       (car xtf) 
						       (nth j ly) 
						       (nth i (nth j lx))))) w)))) z))
		 (setf index (+ index (length xtf)))))))))

(defun interpret-estimates (yt xt yh xh)
  (let ((fuy (assoc yt *deriv-inv*))
	(fux (assoc xt *deriv*)))
    (if (null fuy)
	(error "Y-inverse derivative not defined in *deriv-inv*~%"))
    (if (null fux)
	(error "X derivative not defined in *deriv*~%"))
    (* (mapcar (nth 1 fuy) (if (listp yh) yh (list yh)))
       (mapcar (nth 1 fux) (if (listp xh) xh (list xh))))))

(defun trim (x &key (fl 2))
"Args: (x &key (fl 2)) x - a vector fl - multiple
of iqr from quartiles to trim returns list
of remainder and trimmed values"
  (let* ((lq (quantile x 0.25))
	 (uq (quantile x 0.75))
	 (f (* fl (- uq lq)))
	 (ub (+ uq f))
	 (lb (- uq f))
	 (i (let ((z nil))
	      (dotimes (l (length x) (reverse z))
		       (setf z (cons (or (> (nth l x) ub) (< (nth l x) lb)) z)))))
	 (j (which i))
	 (k (which (mapcar 'not i))))
    (list (select x k) (select x j))))

(defun vnames (tl v)
  "Args: (tl v) Returns list of strings of (transformed variables) from
transform list tl and variable names v"
  (let ((s ()))
    (dotimes (i (length v) (reverse s))
	     (let ((tli (nth (1+ i) tl)))
	       (if (not (equal tli '(NULL)))
		   (setf s (append (let ((z nil))
       (dolist (j tli z)
	       (setf z (cons 
			(format  nil "~A ~A" 
				 (if (equal j 'IDENTITY) "" 
				   (string-downcase (symbol-name j)))
				 (nth i v)) z)))) s)))))))

(defun bc (x y weights box-cox-indices)
  "Args: (x y weights box-cox-indices)
Return a list of the paired indices and likelihood
for the boxcox transformation"
  (let ((z nil) 
	(gmy (expt (apply '* (float y)) (/ 1 (length y))))
	(rc (regression-model x y
			      :print nil
			      :weights weights)))
    (dolist (i box-cox-indices (reverse z))
	    (push (boxcox-lik rc y gmy i) z))))

(defun boxcox-lik (rc y gmy l)
  "Args: (rc y gmy l)
Calculates the log-likelihood for the Box-Cox transformation
with index l"
  (send rc :y (box-cox-aux y l gmy))
  (* (/ (length y) -2) 
     (log (send rc :residual-sum-of-squares))))

(defun box-cox-aux (y l gmy)
  (if (/= l 0) 
      (/ (- (expt y l) 1) (* l (expt gmy (- l 1)))) 
    (* gmy (log y))))

(defun backward-elimination (t-values tlist df p-value vb-select)
  "Args: (t-values tlist df p-value vb-select)
Indicate which (if any) variable should be eliminated
under a backward elimination method"
  (let ((critical-value (abs (t-quant (/ p-value 2) df)))
	(foo (select-tvals t-values tlist vb-select)))
   (cond ((< (length t-values) 1.5) nil)
	  ((null (nth 1 foo)) nil)
	  ((< (car foo) critical-value) (nth 1 foo))
	  (t NIL))))

(defun select-tvals (t-values tlist vb-select)
"Args: (t-values tlist vb-select)
Returns min acceptable t-value, var. no and location in tlist"
  (do* ((tmax (1+ (max t-values)))
	(tmin (min t-values) (min t-values))
	(fini  (= tmax tmin) (= tmax tmin))
	(imin (min (which (= t-values tmin))) (min (which (= t-values tmin))))
	(index (what-list (cdr tlist) imin) (what-list (cdr tlist) imin)))
       ((or (nth (car index) vb-select) fini)
	(list tmin (if fini nil (cons imin index))))
       (setf (select t-values imin) tmax)))

(defun saturated-elimination (basis np tlist)
  (let ((crem (set-difference (iseq 0 (1- np)) basis)))
    (cons (max crem) (what-list (cdr tlist) (max crem)))))
	 
       
(defun log-transform (x max-ratio)
  "Arg: (x max-ratio) Indicate whether a log-transform of the data is desirable
on the basis of whether (maximum x) / (minimum x) exceeds max-ratio"
  (and (> (min x) 0) 
       (> (/ (max x) (min x)) max-ratio)))

;; Borrowed from S. Weisberg's regression diagnostics code
(defun rmel (elmts x)
"
Message Args: (elmts x)
Returns a sublist of x with elements indexed in elmts deleted. elmts may be a single number if only one element of x is to be removed, otherwise elmts must be a list."
  (let* ((z x) 
        rs)
    (cond ((numberp elmts) 
          (setf z (select x (remove elmts (iseq 0 (- (length x) 1))))))
      (t (setf rs (reverse (sort-data elmts)))
       (dotimes (i (length elmts))
         (setf z (select z (remove (nth i rs) (iseq 0 (- (length z) 1))))))))
  z))

;; Auxiliary function called by :prediction
(defun tranx (x tlist)
  (let ((p nil))
    (dolist (xx x (reverse p))
	    (push
	     (cons 1 (let ((tx nil) q)
		       (dotimes (i (length xx) (reverse tx))
				(dolist (j (nth (+ i 1) tlist))
					(setf q (apl j (nth i xx)))
					(if q (push q tx)))))) p))))
(defun transp (x)
  (if (listp (car x))
      (let ((z nil))
	(dotimes (i (length (car x)) (reverse z))
		 (push (mapcar #'(lambda (y) (nth i y)) x) z)))
    x))

(defun any (x f)
"Args: (x f) Tests if any elements of list x
satisfy condition specified by function f"
  (do* ((a (car x) (car b))
	(b (cdr x) (cdr b))
	(r1 (funcall f a) (funcall f a))
	(r2 (null b) (null b) ))
       ((or r1 r2) r1)))
       