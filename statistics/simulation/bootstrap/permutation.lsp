(defun 2-sample (x y)
"Args: x,y 
Graphics driver for testing the equality of location parameters for two samples 
using a randomization test.  Puts up dialogs for choosing whether the data is 
paired or independent, whether to perform a parametric or nonparametric test,  
the number of samples, the significance level, and which plots to show. 
It then calls the functions which performs the test.  The only arguments are   
the sequences X and Y, from which the samples are drawn."
  (let (  
        (flag (repeat 0 5))  
        (s (pair-v-indep-test-dialog))
       )
        (if (eq s nil)  
            (message-dialog "Cancel test ? Goodbye !")
            (if (and (eq s 0) (not (eq (length x) (length y))))
                (message-dialog "Error-- Samples must be equal size for a 
        paired comparison.  Goodbye!")   
                (setf (select flag 0) 1) 
            ) 
        ) 
   (let (
        (f (par-v-nonpar-test-dialog1 flag s))     
        )
        (if (eq f nil) 
            (if (not (member 0 (select flag (iseq 1))))  
                (message-dialog "Cancel test ? Goodbye !") 
            )
            (setf (select flag 1) 1)
        )  
    (let (
            (a (a-test flag))  
          )
        (if (or (eq a nil) (eq (elt a 0) nil))    
            (if (not (member 0 (select flag (iseq 2))))  
                (message-dialog "Cancel significance level ? Goodbye !") 
            ) 
            (if (or (<= (elt a 0) 0) (>= (elt a 0) .5))
                (message-dialog "Error-- Significance level must be a number 
        in (0,.5).  Goodbye !") 
                (setf (select flag 2) 1) 
            )  
        )
      (let (
            (n (n-test flag))
           )
           (if (or (eq n nil) (eq (elt n 0) nil)) 
               (if (not (member 0 (select flag (iseq 3))))  
                   (message-dialog "Cancel resamples ? Goodbye !")  
               )
               (if (or (<= (elt n 0) 0) (not (integerp (elt n 0)))) 
                   (message-dialog "Error-- Number or resamples must be a
        positive integer.  Goodbye!")
                   (setf (select flag 3) 1)
               )   
           )
      (let (
            (p (what-plot-dialog1 flag))
           )
           (if (or (eq p nil) (not (member 't p)))  
               (if (not (member 0 (select flag (iseq 4))))  
                   (message-dialog "Cancel plots ? Goodbye !")
               )
               (setf (select flag 4) 1)
           )
   (if (not (member 0 flag))   
     (permstat x y s f (elt n 0) (elt a 0) p))
))))))

(defun pair-v-indep-test-dialog ()
"Dialog to select whether to perform a paired or independent comparison of
the 2 samples" 
  (setf y-list (send list-item-proto :new (list "Paired" "Independent")))   
  (setf y-label (send text-item-proto :new "The 2 samples are:"))
  (setf cancel (send modal-button-proto :new "Cancel"))
  (setf ok (send modal-button-proto :new "OK" 
           :action #'(lambda()
                      (send y-list :selection))
           ))
  (setf choice-dialog (send modal-dialog-proto :new
         (list 
         (list
          (list y-label y-list))
         (list ok cancel)
         )
         :default-button ok))
  (send choice-dialog :modal-dialog)
)

(defun par-v-nonpar-test-dialog1 (flag s)
"Args: flag s 
Tests whether the user selected a valid option from the paired-or-indep dialog
box.  If so, the function that creates the parametric-or-nonparam dialog box 
is called." 
(if (eq (select flag 0) 1)
    (par-v-nonpar-test-dialog2 s)
))

(defun par-v-nonpar-test-dialog2 (s)
"Args:  s  
Dialog to select whether to perform parametric (t-test) or non-parametric
 (Mann-Whitney or Signed-Rank test) comparison of the samples." 
  (setf y-list (if (eq s 0) 
                    (send list-item-proto :new (list "t-Test" 
                                               "Signed Rank")) 
                    (send list-item-proto :new (list "t-Test" 
                                               "Mann-Whitney"))  
                ))
  (setf y-label (send text-item-proto :new "Parametric or Nonparametric Test?"))
  (setf cancel (send modal-button-proto :new "Cancel"))
  (setf ok (send modal-button-proto :new "OK" 
           :action #'(lambda()
                      (send y-list :selection))
           ))
  (setf choice-dialog (send modal-dialog-proto :new
         (list 
         (list
          (list y-label y-list))
         (list ok cancel)
         )
         :default-button ok))
  (send choice-dialog :modal-dialog)
)

(defun a-test (flag)
"Arg: flag
Tests whether the previous dialog boxes have been responded to correctly.  If 
so, a dialog box is created to retrieve the significance level."    
(if (and (eq (select flag 0) 1) (eq (select flag 1) 1)) 
    (get-value-dialog "Significance Level for Confidence Region:" 
       :initial .05)
))  

(defun n-test (flag)
"Arg: flag
Tests whether the previous dialog boxes have been responded to correctly.  If 
so, a dialog box is created to retrieve the number of resamples." 
(if (and (eq (select flag 0) 1) (eq (select flag 1) 1) (eq (select flag 2) 1)) 
    (get-value-dialog "Number of resamples:" 
       :initial 100)
))  

(defun what-plot-dialog1 (flag) 
"Args: flag 
Tests whether the user selected a valid options from the previous dialog
boxes.  If so, the function that creates the what-plots dialog box
is called."
(if (and (eq (select flag 0) 1) (eq (select flag 1) 1) (eq (select flag 2) 1) 
      (eq (select flag 3) 1)) 
    (what-plot-dialog2)
))

(defun what-plot-dialog2() 
"Dialog to select what to plot"
  (setf y-item-0 (send toggle-item-proto :new "Permutation Distribution"
   :value t)) 
  (setf y-item-1 (send toggle-item-proto :new "CDF Plot" 
   :value t))
  (setf y-item-2 (send toggle-item-proto :new "Standard Error Plot" 
   :value nil))
  (setf y-item-3 (send toggle-item-proto :new 
                  "Permutation Distribution v. t-Distribution Plot"
                  :value nil))
  (setf y-label (send text-item-proto :new "Plots"))
  (setf cancel (send modal-button-proto :new "Cancel"))
  (setf ok (send modal-button-proto :new "OK"
           :action #'(lambda()
                     (list
                      (send y-item-0 :value)
                      (send y-item-1 :value)
                      (send y-item-2 :value)
                      (send y-item-3 :value)))
           ))
  (setf choice-dialog (send modal-dialog-proto :new
         (list
         (list
          (list y-label y-item-0 y-item-1 y-item-2 y-item-3))
         (list ok cancel)
         )
         :default-button ok))
  (send choice-dialog :modal-dialog)
)

(defun permstat (x y s f nboot a plt)
"Args: x y s f nboot a plt 
The 2 samples are in X and Y (both are lists).  Whether to use a paired or 
independent method is passed in S, a value of 0 or 1.  Whether to use a  
parametric or nonparametric method is passed in F, a value of 0 or 1.  The 
number of resamples is passed in NBOOT. A contains the significance level.  
Which plots are made is passed in PLT, a list of three t/nil values.  For each
new permutation (from 1 to NBOOT), the chosen plots are continually updated. 
If the cdf plot is made, the endpoints of the 100(1-A)% CI is shown on the 
plot (as dashed lines), as is the statistic for the original data (shown as 
a solid line).  If the permutation distribution plot is chosen, the normal 
and kernal densities are superimposed on the histogram.  The function returns 
a list with the permutation statistics."   
(let (
      (tt (make-list nboot))
      (p (if (first plt) (histogram nil) nil))
      (q (if (second plt) (plot-points nil nil :variable-labels 
                          (list "permutation distn" "cdf")) nil))
      (r (if (third plt) (plot-lines (list 0) (list 0) :variable-labels 
                         (list "iterations" "standard error")) nil))
      (u (if (fourth plt) (plot-points nil nil :variable-labels
                          (list "permutation distn" "t-distn")) nil))
      (lft (/ a 2))
      (rt (- 1 (/ a 2))) 
      (val (if (eq s 0) 
               (if (eq f 0)  
                   (paired-t-stat (list x y))
                   (signed-rank-stat (list x y))) 
               (if (eq f 0)  
                   (indep-t-stat (list x y)) 
                   (mann-whitney-stat (list x y))))
      )  
     )
   (if p (send p :location 0 100))
   (if q (send q :location 370 100))
   (if r (send r :location 500 600))
   (if u (send u :location 50 400))  
   (if q (send q :title "cdf plot"))
   (if r (send r :title "standard error plot"))
   (if p (send p :title "permutation distribution"))
   (if u (send u :title "permutation distn vs. t-distn plot"))
  (dotimes (i nboot)  
    (setf (nth i tt) (get-stat x y s f))
    (if p (send p :add-points (list (nth i tt))))
    (if p (send p :draw-string (num-to-string i) 20 20)) 
    (if q (send q :clear-lines)) 
    (if q (if (> i 1) 
              (send q :add-lines (sort-data (select tt (iseq i))) 
                                 (/ (iseq i) i))))  
    (if r (if (> i 1) (send r :add-lines (list (1- i) i) (list
            (standard-deviation (select tt (iseq i)))
            (standard-deviation (select tt (iseq (1+ i))))))))
    (if u (send u :clear-points)) 
    (if u (if (> i 1)
              (send u :add-points (sort-data (select tt (iseq i))) 
                                  (make-t-list i x y s))
          )) 
    (if p (send p :adjust-to-data))
    (if q (send q :adjust-to-data))
    (if r (send r :adjust-to-data))
    (if u (send u :adjust-to-data))
    )
  (if p (send p :add-lines (fit-normal tt)))
  (if p (send p :add-lines (kernel-dens tt)))
  (if q (send q :add-lines  
        (repeat val 2)  
        (list 0 (t-quantile val tt))))
  (if q (send q :add-lines  
        (list (select (send q :range 0) 0) val)
        (repeat (t-quantile val tt) 2)))
  (if q (send q :add-lines (list (quantile tt lft) (quantile tt lft) 
                                 (select (send q :range 0) 0)) 
                           (list (select (send q :range 1) 0) lft lft)
                :type "dashed"))
  (if q (send q :add-lines (list (quantile tt rt) (quantile tt rt) 
                                 (select (send q :range 0) 0)) 
                           (list (select (send q :range 1) 0) rt rt)
                :type "dashed"))
  (if r (send r :add-lines (list 0 nboot) (repeat (standard-deviation tt) 2)))
  (if r (send r :size 375 250)) 
  (if p (send p :size 375 188)) 
tt))

(defun fit-normal (tt)
"Args: tt 
Computes mean and standard deviation, and returns list of
list of arguments and list of ordinates, suitable for
add-lines in one-dim plots."
(let* (
       (m (mean tt))
       (s (standard-deviation tt))
       (u (max tt))
       (v (min tt))
       (a (/ (- u v) 50))
       (b (/ (+ u v) 2))
       (z (+ b (* a (- (iseq 51) 25))))
       (p (/ (normal-dens (/ (- z m) s)) s))
       )
(list z p)))

(defun get-stat (x y s f) 
"Args: x y s f 
Permutes the 2 samples appropriately and obtains a statistic for the permuted 
data.  The statistic is from either the Signed-Rank test, Mann-Whitney test, 
Paired t-test, or Independent t-test."   
(let* (
      (xy (append x y)) 
      (nx (length x))
      (nxy (length xy))
      (k (sample (iseq nxy) nx))  
      (kcomp (set-difference (iseq nxy) k))
      )   
(cond 
 ((and (eq s 0) (eq f 0)) (paired-t-stat (list x (sample y (length y)))))   
 ((and (eq s 1) (eq f 0)) (indep-t-stat (list (select xy k) 
                                              (select xy kcomp))))
 ((and (eq s 0) (eq f 1)) (signed-rank-stat (list x (sample y (length y))))) 
 ((and (eq s 1) (eq f 1)) (mann-whitney-stat (list (select xy k) 
                                                   (select xy kcomp)))) 
)
))

(defun makeranks (d)
"Args: d 
Returns the ranks of each element in the list, D.  If 2 or more elements in 
D have the same value, the elements receive the average of their ranks."  
  (let* (
        (ranks (+ (rank d) 1)) 
        )
    (dotimes (i (length d))  
      (if (> (length (which (= (nth i d) d))) 1) 
          (setf (select ranks (which (= (nth i d) d))) 
                (repeat (mean (select ranks (which (= (nth i d) d))))  
                        (length (which (= (nth i d) d))))) 
      ) 
    )             
    ranks))  

(defun signed-rank-stat (z)   
"Args: data
Computes a signed-rank statistic from the data, which must 
be a list of two lists of equal length."  
  (let* (
        (x (elt z 0))
        (y (elt z 1))
        (n (length x))
        (d (- x y))
        (newd (select d (which (/= 0 d))))    
	(ranks (makeranks newd)) 
	(w (sum (select ranks (which (< 0 newd))))) 
        )
   w)) 

(defun mann-whitney-stat (z) 
"Args: data 
Computes the Mann-Whitney statistic from the data, which must 
be a list of two lists, not necessarily of equal length." 
  (let* (
        (x (elt z 0))
        (y (elt z 1)) 
	(nx (length x)) 
	(xy (combine x y)) 
	(ranks (makeranks xy))      
	(w (sum (select ranks (iseq nx)))) 
	)
  w))  

(defun paired-t-stat (z)
"Args: data
Computes a paired t-statistic from the data, which
must be a list of two lists of equal length."
  (let* (
        (x (elt z 0))
        (y (elt z 1))
        (n (length x))
        (d (- x y))
        )
(* (sqrt n) (/ (mean d) (standard-deviation d)))
))

(defun indep-t-stat (z)
"Args: data
Computes an independent t-statistic from the data, which
must be a list of two lists, not necessarily
of the same length."
  (let* (
        (x (elt z 0))
        (y (elt z 1))
        (nx (length x))
        (ny (length y))
        (mx (mean x))
        (my (mean y))
        (vx (^ (standard-deviation x) 2))
        (vy (^ (standard-deviation y) 2))
        (sp (sqrt (/ (+ (* (1- nx) vx) (* (1- ny) vy)) (+ nx ny -2))))
        (fc (sqrt (+ (/ nx) (/ ny))))
        )
(/ (- mx my) (* sp fc)))
)

(defun t-quantile (val tt)
"Args: val
Determines the proportion of values in tt that are less than val." 
 (let* (
       (wh (which (> val (sort-data tt))))
       (num (if (eq wh nil) 0 (+ 1 (max wh))))
       )
(/ num (length tt))    
))  

(defun make-t-list (n x y s)  
 (let* (
       (list (make-list n)) 
       (nx (length x))
       (ny (length y))
       (df (if (eq s 0) (- nx 1) (+ nx ny -2)))
       ) 
  (dotimes (i n)
     (setf (nth i list) (t-quant (/ (+ 1 i) (+ 1 n)) df)))
  list))  


;;;;;;;; additional stuff

(defun paired-t-stat (z)
"Args: data
Performs a paired t-test on the data, which
must be a list of two lists of equal length."
  (let* (
        (x (elt z 0))
        (y (elt z 1))
        (n (length x))
        (d (- x y))
        )
(* (sqrt n) (/ (mean d) (standard-deviation d)))
))

(defun indep-t-stat (z)
"Args: data
Performs a two-sample t-test on the data, which
must be a list of two lists, not necessarily
of the same length."
  (let* (
        (x (elt z 0))
        (y (elt z 1))
        (nx (length x))
        (ny (length y))
        (mx (mean x))
        (my (mean y))
        (vx (^ (standard-deviation x) 2))
        (vy (^ (standard-deviation y) 2))
        (sp (sqrt (/ (+ (* (1- nx) vx) (* (1- ny) vy)) (+ nx ny -2))))
        (fc (sqrt (+ (/ nx) (/ ny))))
        )
(/ (- mx my) (* sp fc)))
)

(defun anova-stat (x))
    
(defun sign-test-stat (z)        
"Args: data
Performs a paired t-test on the data, which
must be a list of two lists of equal length."
  (let* (
        (x (elt z 0))
        (y (elt z 1))
        (n (length x))
        (d (sign (- x y)))
        )
    ))

(defun signed-rank-stat (z)
  "Args: data
Performs a Wilcoxon signed-rank test on the data, which
must be either a list of two lists of equal length (two-sample
situation) or a list consisting of list and a single number
(one-sample situation)."
  (let* (
        (x (elt z 0))
        (y (elt z 1))
        (n (length x))
        (d (- x y))
        )
    ))

(defun rank-sum-stat (z)
  "Args: data
Performs Wilcoxon-Mann-Whitney test on the data, which
must be a list of two lists of not necessarily equal length."
  (let* (
        (x (elt z 0))
        (y (elt z 1))
        (n (length x))
        (m (length y))
        (a (max n m))
        (b (min n m))
        (p (if (< n m) (append x y) (append y x)))
        (r (1+ (rank p))
        )
(/ (- (sum (select (iseq n) r)) (/ (* b (+ 1 n m)) 2))
   (sqrt (/ (* n m (+ 1 n m)) 12)))
)))

(defun kruskal-wallis-stat (x))

(defun friedman (x))

(defun npos (x)
"Args: sequence
Returns the number of positive members of the SEQUENCE."
(length (select x (which (> x 0))))
)

(defun sign (x)
"Args: sequence
Returns the signs of the elements of SEQUENCE."
(cond ((sequencep x)
       (let (
             (n (length x))
             )
         (setf x (if-else (> x 0) (repeat 1 n) x))
         (if-else (< x 0) (repeat -1 n) x)))
       (t (cond ((> x 0) 1) ((< x 0) -1) (t 0)))
       )
)

    

