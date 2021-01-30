;;;; XLISP-STAT 2.1 Copyright (c) 1990, by Luke Tierney
;;;; Additions to Xlisp 2.1, Copyright (c) 1989 by David Michael Betz
;;;; You may give out copies of this software; for conditions see the file
;;;; COPYING included with this distribution.
;;;;
;;;; Additional modifications by B. Narasimhan naras@euler.bd.psu.edu
;;;;
;;;; Note that I have not striven for any great generality. I did
;;;; everything with an eye towards expediency.  I was primarily
;;;; dictated by the concerns for the class I was teaching, where we
;;;; use Devore's book "Probability and Statistics for Engineering and
;;;; the Sciences." I hope to make this code more sophisticated when
;;;; I can find the time. 
;;;;

(provide "oneway")

(require "regress")
;;;;
;;;;
;;;; One Way ANOVA Model Prototype
;;;;
;;;;

(defproto oneway-model-proto '(grouped-data) '() regression-model-proto)

(defun oneway-model (data &key (print t) group-names)
"Args: ( data &key (print t))
DATA: list of compound-data
Example:"
  (let ((data (mapcar #'(lambda (x) (coerce x 'list)) data))
        (m (send oneway-model-proto :new)))
    (send m :grouped-data data)
    (send m :group-names group-names)
    (if print (send m :display))
    m))

(defmeth oneway-model-proto :display ()
"Message args: ()
Prints the least squares regression summary."
  (call-next-method)
  (format t "Group Mean Square:~25t~13,6g~40t(~,6g)~%"
          (send self :group-mean-square) (send self :group-df))
  (format t "Error Mean Square:~25t~13,6g~40t(~,6g)~%"
          (send self :error-mean-square) (send self :error-df))
  (format t "~%"))

(defmeth oneway-model-proto  :save ()
"Message args: ()
Returns an expression that will reconstruct the model."
  `(oneway-model ',(send self :grouped-data) 
                 :group-names ',(send self :group-names)))

;;;
;;; Slot Accessors and Mutators
;;;

(defmeth oneway-model-proto :grouped-data (&optional data)
"Message args: (&optional data)
Sets or returns the grouped data."
  (when data
        (let* ((y (apply #'append data))
               (indices (repeat (iseq 0 (- (length data) 1)) 
                                (mapcar #'length data)))
               (levels (remove-duplicates indices))
               (indicators (mapcar #'(lambda (x) (if-else (= x indices) 1 0))
                                   levels))
               (x (apply #'bind-columns indicators)))
          (setf (slot-value 'y) y)
          (setf (slot-value 'x) x)
          (setf (slot-value 'intercept) nil)
          (setf (slot-value 'grouped-data) data)
          (send self :needs-computing t)))
   (slot-value 'grouped-data))

(defmeth oneway-model-proto :group-names (&optional (names nil set))
"Method args: (&optional names)
Sets or returns group names."
  (if set (setf (slot-value 'predictor-names) names))
  (let ((g-names (slot-value 'predictor-names))
        (ng (length (slot-value 'grouped-data))))
    (if (not (and g-names (= ng (length g-names))))
        (setf (slot-value 'predictor-names)
              (mapcar #'(lambda (a) (format nil "Group ~a" a)) 
                      (iseq 0 (- ng 1))))))
  (slot-value 'predictor-names))

;;;
;;; Overrides for Linear Regression Methods
;;;

(defmeth oneway-model-proto :y ()
"
Message args: ()
Returns the response vector."
   (call-next-method))

(defmeth oneway-model-proto :x ()
"Message args: ()
Returns the design matrix."
   (call-next-method))

(defmeth oneway-model-proto :intercept (&rest args)
"Message args: ()
Always returns nil. For compatibility with linear regression."
  nil)

(defmeth oneway-model-proto :predictor-names () (send self :group-names))

;;;
;;; Other Methods
;;;

(defmeth oneway-model-proto :standard-deviations ()
"Message args: ()
Returns list of within group standard deviations."
  (mapcar #'standard-deviation (send self :grouped-data)))
  
(defmeth oneway-model-proto :group-df () 
"Message args: ()
Returns degrees of freedom for groups."
	(- (length (send self :grouped-data)) 1))

(defmeth oneway-model-proto :group-sum-of-squares ()
"Message args: ()
Returns sum of squares for groups."
  (sum (^ (- (send self :fit-values) (mean (send self :y))) 2)))

(defmeth oneway-model-proto :group-mean-square ()
"Message args: ()
Returns mean square for groups."
	(/ (send self :group-sum-of-squares) (send self :group-df)))
	
(defmeth oneway-model-proto :error-df ()
"Message args: ()
Returns degrees of freedom for error."
	(send self :df))
	
(defmeth oneway-model-proto :error-mean-square ()
"Message args: ()
Returna mean square for error."
	(/ (send self :sum-of-squares) (send self :df)))
	
(defmeth oneway-model-proto :boxplots ()
"Message args: ()
Produce parallel box plots of the groups."
	(boxplot (send self :grouped-data)))
	
;;;
;;; BN's additions begin.
;;; 
(require "as190")

(defmeth oneway-model-proto :display-tukey-groupings (&optional (alpha 0.05))
  "Method args: (&optional (alpha 0.05))
Displays the Tukey groupings using alpha significance. Alpha must be
in [0.01, .10]."
  (let* ((means (send self :coef-estimates))
	 (no-of-groups (length means))
	 (ji-s (mapcar #'length (send self :grouped-data)))
	 (morder (order means))
	 (ordered-means (mapcar #'(lambda(x) (select means x)) morder))
	 (gnames (send self :group-names))
	 (ordered-gnames  (mapcar #'(lambda(x) (select gnames x)) morder))
	 (mse (send self :error-mean-square))
	 (dfe (send self :error-df))
	 (q (qrtrng (- 1 alpha) dfe no-of-groups)))
    (if (not (apply #'= ji-s))
	(format t "NOTE: Unbalanced Design; assuming mild imbalance...~%"))
    (format t "Alpha = ~5,4f df = ~5d MSE = ~10,6g~%" alpha dfe mse)
    (format t "Critical value of Studentized Range = ~7,4f~%" q)
;;;
;;; Pretty naive code here...
;;;
    (format t "~% ---- Tukey Groupings ----~%")
    (let ((i 0))
      (loop
       (let* ((mean-i (select ordered-means i))
	      (ji (select ji-s i))
	      (wij 
	       (mapcar #'(lambda(x) 
			   (* q  (sqrt (* 0.5 mse (+ (/ ji) (/ x)))))) ji-s))
	      (abs-mean-diffs (abs (- ordered-means mean-i)))
	      (same (which (< abs-mean-diffs wij))))
	 (format t "~g~%" (select ordered-gnames same))
	 (if (= i (max same))
	     (setf i (1+ i))
	   (setf i (1+ (max same)))))
       (if (>= i no-of-groups)
	   (return))))))

(defmeth oneway-model-proto :display ()
  "Method args: none
Display the Anova Table."
  (call-next-method)
  (let* ((sstr (send self :group-sum-of-squares))
	 (dftr (send self :group-df))
	 (mstr (/ sstr dftr))
	 (sse (send self :sum-of-squares))
	 (dfe (send self :error-df))
	 (mse (/ sse dfe))
	 (f (/ mstr mse))
	 (p-value (- 1 (f-cdf f dftr dfe))))
    (format t "~%Analysis of Variance Table~%")
    (format t "-----------------------------------------------------------~%")
    (format t "Source        df    SS          MS             F   p-value~%")
    (format t "-----------------------------------------------------------~%")
    (format t "Treatment ~5d   ~10,6g ~10,6g ~8,2f ~8,4f~%" 
            dftr sstr mstr f p-value)
    (format t "Error     ~5d   ~10,6g ~10,6g~%" dfe sse mse)
    (format t "-----------------------------------------------------------~%")
    (format t "Total     ~5d   ~10,6g~%~%" (+ dftr dfe) (+ sstr sse)))
  (send self :display-tukey-groupings))

(defmeth oneway-model-proto :all-paired-comparisons (&optional (alpha 0.05))
  "Method args: (&optional (alpha 0.05))
Displays all possible CI's for mu_i - mu_j."
  (let* ((means (send self :coef-estimates))
	 (no-of-groups (length means))
	 (ji-s (mapcar #'length (send self :grouped-data)))
	 (gnames (send self :group-names))
	 (mse (send self :error-mean-square))
	 (dfe (send self :error-df))
	 (q (qrtrng (- 1 alpha) dfe no-of-groups)))
    (format t "~% ---- All Paired Tukey Comparisions----~%")
    (if (not (apply #'= ji-s))
	(format t "NOTE: Unbalanced Design; assuming mild imbalance...~%"))
    (dotimes (i no-of-groups)
      (dotimes (j no-of-groups)
	(when (> j i)
	  (let* ((dij (- (select means i) (select means j)))
		 (gnamei (select gnames i))
		 (gnamej (select gnames j))
		 (ji (select ji-s i))
		 (jj (select ji-s j))
		 (wij (* q  (sqrt (* 0.5 mse (+ (/ ji) (/ jj)))))))
	    (format t "~a, ~a: (~10,6g, ~10,6g)~%"
		    gnamei gnamej (- dij wij) (+ dij wij))))))))

(defmeth oneway-model-proto :individual-ci (&optional (alpha 0.05))  
  "Method args: (&optional (alpha 0.05))
Displays Individual CI's for means using the pooled standard deviation."
  (let* ((means (send self :coef-estimates))
	 (sd-s (send self :standard-deviations))
	 (ji-s (mapcar #'length (send self :grouped-data)))
	 (gnames (send self :group-names))
	 (maxlen (max (mapcar #'length gnames)))
	 (heading
	  (format nil "~va    N   Mean          SD              ~
                                       CI          " maxlen "Group"))
	 (line (make-string (length heading) :initial-element #\-))
	 (sp (sqrt (send self :error-mean-square)))
	 (dfe (send self :error-df))
	 (tc (t-cdf (- 1 (* 0.5 alpha)) dfe)))
    (format t "~% ---- Individual CI's based on Pooled Std. Dev ----~%")
    (format t "~a~%" heading)
    (format t "~a~%" line)
    (dotimes (i (length means))
      (let* ((x (select means i))
	     (ji (select ji-s i))
	     (w (/ (* tc sp) (sqrt ji)))
	     (gnamei (select gnames i))
	     (sdi (select sd-s i)))
	(format t "~va: ~4d ~10,6g ~10,6g (~10,6g, ~10,6g)~%"
		maxlen gnamei ji x sdi (- x w) (+ x w))))))

