#|
I have (below) a function for performing an analysis separately for each
combination of levels of a set of variables.  For example, you could get
means or other summaries of income by sex and race, or perform a
regression analysis separately for each county or state in a database.

I'm not sure where this should go in your archive but I think people
should find it useful.

thomas lumley, Biostatistics, Uni of Washington
|#

;;
;;  Perform an analysis for each combination of levels of specified 
;;  variables.
;;
;;  arguments:  analysis  variables by-variables
;;
;;  eg  (by '(gee-model :x x :y y :g g :error normal-error :verbose t)
;;                 '(x y g) (list sex race))
;;
;;     (by '(gee-model :x (list x1 x2 x3) :y y :g g :error normal-error 
;;                  :verbose t :predictor-names (list "x1" "x2" "x3"))
;;                 '(x1 x2 x3 y g) (list sex race))
;;
;;     (by '(mean age) '(age) (list smoker gender))
;;
;;     (by '(plot-points x y) '(x y) (list state))
;;
;;  The variables must all describe data of the same length, they
;; can be sequences or  matrices
;;
;; by-variables is a list of sequences.
;;
;;

(defun make-rows (data)
  (cond ((matrixp data) (row-list data))
	((sequencep data) data)
	(t (error "Can't convert this to rows"))
	)
)

(defun equal-sequence (x y) (mapcar #'(lambda (z) (equalp z y)) x))


 (defun by (analysis variables by-variables)
" Args:  analysis  variables by-variables.
For each combination of levels of by-variables, perform
analysis on that subset of the specified variables. 
Returns a list of the results."
  (let* ((byrows (row-list (apply #'bind-columns by-variables)))
	 (unique-rows (remove-duplicates byrows :test #'equalp))
	 (byindex (mapcar #'(lambda (x) (which (equal-sequence (row-list (apply
#'bind-columns by-variables)) x))) unique-rows))
	 (setup (mapcar #'(lambda (vv) (if (matrixp (eval vv)) `(,vv (select
,vv indices (iseq (second (array-dimensions ,vv)))))
					 `(,vv (select ,vv indices)))) variables))
	 (run `(mapcar #'(lambda (indices) (let* ,setup ,analysis)) byindex))
	 )
    (evalhook run nil nil (getenv))
    )
)

