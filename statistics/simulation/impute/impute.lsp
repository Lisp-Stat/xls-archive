(defun start (data num &optional like)
"Args: (DATA NUM)
This function starts the imputation and parameter estimation using   
multivariate normal distributions.  Data must be in the form of a matrix
with variables as columns and cases as rows.  NUM should be the number
of iterations that are to be carried out...careful..this could take a while.
LOGLIKE is an optional argument that, if supplied, gives a plot of the
log-likelihood."
 (let* (
       (collist (column-list data))
       (rowlist (row-list data))
       (l1 (length collist))
       (l2 (length rowlist))
       (presind (mapcar #'which collist))
       (presindt (mapcar #'which rowlist))
       (missindt (mapcar #'(lambda (x) (which
                           (mapcar #'not (combine x)))) rowlist))
       (meanlist (mapcar #'(lambda (x y) (mean (select x y))) collist presind))
       (collist (mapcar #'(lambda (x y) (if-else x x y)) collist meanlist))
       (siginv (identity-matrix l1)) 
       (mu (map 'vector #'mean collist))
       (newsigma 0)
       (likehist ())
       (outprod 0)
       (vi (make-array (list l1 l1) :initial-element 0))
       )
       (dotimes (j num)

         (dotimes (i l2)
 
           (setf torf (not (not (and (elt missindt i) (elt presindt i)))))
           (setf params (param-est siginv collist (elt missindt i)
                                     (elt presindt i) mu i torf l1))
           (setf newsigma (+ newsigma (first params) (second params)))
           (setf outprod (+ outprod (first params)))
           (cond (torf (setf rowlisti (impute params (elt missindt i)
                                         (elt presindt i) mu)))
                 ((not (not (elt missindt i))) (setf rowlisti (combine mu)))
                 (t (setf rowlisti (combine (last params)))))
           (mapcar #'(lambda (x y) (setf (elt (elt collist x) i) y))
                                              (iseq l1) rowlisti)
         )
         (setf siginv (inverse (/ newsigma l2))) 
         (setf mu (map 'vector #'mean collist))
         (setf loglike (list (- (* l2 (determinant siginv))
                             (sum (diagonal (matmult siginv outprod))))))
         (setf likehist (append likehist loglike))
         (setf newsigma 0)
         (setf outprod 0)
       )
    (def data (apply #'bind-columns collist))
    (def results (list data mu siginv likehist))
    (print-results results)
 )
)

(defun print-results (results)
    (format t "Your origional Data Was: ~%")
    (print-matrix (first results))

    (format t "~% The Estimated Variable means are: ~%")
    (print (second results))

    (format t "~%~% The Estimated Inverse Covariance is: ~%")
    (print-matrix (third results))

    (format t "~%~% The Log-Likelihood History is: ~%")
    (print (elt results 3))

    (format t "~% All of the above are in the variable results ~%")


;;    (def pp (plot-points (iseq (length (elt results 3))) (elt results 3)))

)



(defun param-est (sig data missind presind mu i torf l1)
"Args: (SIGMA DATA MISSIND PRESIND MU I TORF LEN)
Takes in the data, missing and present indicies and estimates the parameters
of a multivariate normal distribution.  SIG, the inverse covariance matrix
should be in the form of a matrix.  Data must be in the form of a list of
vectors MISSIND must be in the form of a list for case i.  PRESIND must also
be in the form of a list for case i.  MU must be in the form of a vector.  I
is the case on which the imputation is being performed.  TORF is a boolean that
is true if there are at least one missing observation and at least one present
observation for case i.  LEN is the number of variables (the dimensionality
of the multivariate normal.  V0 is a matrix of zeros of dimension l1xl1." 
(cond (torf
      (let* (
            (smm-1 (inverse (apply #'bind-rows
                   (mapcar #'(lambda (x) (mapcar #'(lambda (y) (aref sig
                              x y)) missind)) missind))))
            (smo (apply #'bind-rows (mapcar #'(lambda (x) (mapcar
                          #'(lambda (y) (aref sig x y)) presind)) missind)))
            (vi (make-vi smm-1 missind l1))
            (datai (mapcar #'(lambda (x) (elt x i)) data))
            (yminusmu (- datai mu))
            )
          (list (outer-product yminusmu yminusmu) vi smm-1 smo datai)
     )
     )
      (t   (let* (
                   (datai (mapcar #'(lambda (x) (elt x i)) data))
                   (yminusmu (- datai mu))
                 )
                   (cond ((not (not missind))
                          (list (outer-product yminusmu yminusmu) 
                                (inverse sig)))
                   (t    (list (outer-product yminusmu yminusmu) 0 datai))
                   )
           )
      )
)
)

(defun impute (params missind presind mu)
"Args: (PARAMS MISSIND PRESIND MU).
Imputes the missing data vector for case i indexed my missind by
regressing the missing values on the non-missing values.  PARAMS is
a list of matricies and lists.  MISSIND are the indicies of the missing
variables for case i.  PRESIND are the indicies of the present variables
for case i.  MU is the current mean vector.  This funtion is only called
if there are at least one present variable and at least one missing
variable for case i.  Returns the list of imputed and observed values for
case i."
 (let* (
       (lparams (combine (last params)))
       (newvals (+ (select mu missind) (inner-product
                      (matmult (select params 2) (select params 3))
                      (- (select lparams presind)
                         (select mu presind)))))
       )
  (setf (select lparams missind) newvals)
  lparams
 )
)

(defun make-vi (smm-1 missind l1)
"Args:  smm-1 missind p.
Returns a matrix of zeros with smm-1 in place of the missing variables."
 (let* (
       (vi (make-array (list l1 l1) :initial-element 0))

       (mapcar #'(lambda (x y) (mapcar #'(lambda (z w)
                                  (setf (aref vi x z) (aref smm-1 y w)))
                                   missind (iseq (length missind))))
                                   missind (iseq (length missind)))
       )
       vi
 )
)





