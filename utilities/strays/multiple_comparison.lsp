#|
I don't have Dunnett's test but I do have three p-value based 
corrections (Bonferroni, Holm, and Hochberg).  Holm's test is a more 
powerful version of Bonferroni and Hochberg's test is still more 
powerful. They work well when the tests are correlated and when several 
of the null hypotheses are false (the situation when Bonferroni's 
inequality is conservative). The theory is explained in "Adjusted 
p-values for simultaneous inference" by SP Wright, Biometrics 48: 1005-1013.
Both tests multiple the smallest of k p-values by k, the next smallest by 
k-1 and so on, but they differ in the way they exploit the original 
ordering of the p-values.

Code follows:

thomas lumley
UW biostatistics

|#

(defun holm (p-values) "Args: p-values
Adjusts a list of p-values by Holm's method"
(let* ((n (length p-values))
       (ranks (rank p-values))
       (qi (* p-values (- n ranks)))
       (junk (dotimes (i (- n 1) qi)
		      (let* (
			     (index (position  (+ 1 i) ranks ))
			     (lastindex (position  i ranks ))
			     )
			     (setf (elt qi index) (max (elt qi index) (elt qi lastindex)))
			     )
       )))
  qi
))

(defun hochberg (p-values) "Args: p-values
Adjusts a list of p-values by Hochberg's method"
(let* ((n (length p-values))
       (ranks (rank p-values))
       (qi (* p-values (- n ranks)))
       (junk (dotimes (i (- n 1) qi)
		      (let* (
			     (index (position  (- n i 2) ranks ))
			     (lastindex (position  (- n i 1) ranks ))
			     )
			     (setf (elt qi index) (min (elt qi index) (elt qi lastindex)))
			     )
       )))
  qi
))

(defun bonferroni (p-values) "Args: p-values
Adjusts a list of p-values by Bonferroni's method.
Use (holm) or (hochberg) instead."
(* p-values (length p-values))
)



