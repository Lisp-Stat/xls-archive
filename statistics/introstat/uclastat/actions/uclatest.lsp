(defmeth data-variable-proto :test-dialog ()
)

(defmeth data-variable-proto :test ()
)

(defun hypo-test (x) 
"Args: x 
Graphics driver for performing various tests of location, scale and distrib- 
ution.  Puts up dialog for choosing which tests to perform.  It then calls the 
functions which perform the chosen tests.  The only argument is the data, X." 
 (let* ( 
       (list (test-dialog)) 
       ) 
  (if list  
      (if (which list) 
          (let* ( 
                (act (first list)) 
                (ind (which (rest list)))  
                (parm-list (get-parms ind)) 
                (results (get-results x ind parm-list))
                ) 
          (show-results results ind) 
          )  
      ) 
  ) 
)) 


(defun show-results (results ind) 
"Args:  results ind
Makes a dialog box which displays the results of the tests that were chosen.
The results are in the  list RESULTS and the information on the chosen
tests are in the list IND."
 (let* ( 
       (tlist (get-tests ind)) 
       )  
(if ind  
      (let* ( 
            (top1 (send text-item-proto :new "TEST:"))
            (top2 (send text-item-proto :new "STATISTIC:"))
            (top3 (send text-item-proto :new "P-VALUE:"))
            (vlist1 (mapcar #'(lambda (x) (send text-item-proto :new x)) tlist))
            (vlist2 (mapcar #'(lambda (x) (send text-item-proto :new
                              (format nil "~,6g" x))) (select results 0)))
            (vlist3 (mapcar #'(lambda (x) (send text-item-proto :new
                              (format nil "~,6g" x))) (select results 1)))
            (ok (send modal-button-proto :new "OK"))
            (tdiag (send modal-dialog-proto :new
                   (list (list (adjoin top1 vlist1) (adjoin top2 vlist2) 
                   (adjoin top3 vlist3)) ok) :default-button ok))
            )
(send tdiag :modal-dialog)))
))




(defun get-results (x ind parm-list)
"Args: x ind parm-list
Based on which tests are chosen by the user (info in list IND), it calls
the various functions that run the tests using the data set X and parameters in
PARM-LIST, and returns a list of all test results."
(if ind 
 (let* ( 
       (tlist (if (which (= 0 ind))  
                  (t-test x (select parm-list 0)) 
                  (list nil nil) )) 
       (tboot (if (which (= 1 ind))  
                  (t-boot-test x parm-list)  
                  (list nil nil) )) 
       (sign  (if (which (= 2 ind)) 
                  (list nil nil)  
                  (list nil nil))) 
       (srank (if (which (= 3 ind)) 
                  (list nil nil) 
                  (list nil nil))) 
       (chsqr (if (which (= 4 ind)) 
                  (list nil nil)  
                  (list nil nil))) 
       (gof   (if (which (= 5 ind)) 
                  (list nil nil) 
                  (list nil nil)))
       (vlist1 (list (select tlist 0) (select tboot 0) (select sign 0) 
                     (select srank 0) (select chsqr 0) (select gof 0)))     
       (vlist2 (list (select tlist 1) (select tboot 1) (select sign 1) 
                     (select srank 1) (select chsqr 1) (select gof 1)))     
       (vlist (list (select vlist1 ind) (select vlist2 ind))) 
       ) 
 vlist 
 )   
))   


(defun t-boot-test (x parm-list) 
"Args: x parm-list
Takes the data set, X, and runs a bootstrapped t-test on it.  It returns
the original t-statistic, and its p-value (based on where it lies among
the n bootstrapped t-statistics).  The number of bootstraps, n, and the
hypothesized mean, mu, are in PARM-LIST."
 (let* ( 
       (mu (select parm-list 0)) 
       (num (select parm-list 1)) 
       (t (select (t-test x (select parm-list 0)) 0))   
       (tt (make-list num)) 
       (p nil)  
       ) 
  (dotimes (i num)   
    (setf (nth i tt) 
          (select (t-test (sample (+ (- x (mean x)) mu) (length x) t) mu) 0))
  )
  (if (< t 0) 
      (setf p (+ (t-quantile t tt) (t-quantile t (- tt))))
      (setf p (+ (t-quantile (- t) tt) (t-quantile (- t) (- tt))))
  )  
  (list t p)   
 ) 
)     


(defun t-test (x mu) 
"Args:  x mu
Calculates the t-statistic for the data set X given the hypothesized mean,
MU. Returns the t-statistic and its p-value (based on the t-distribution."
 (let* (
       (n (- (length x) 1)) 
       (t (* (/ (- (mean x) mu) (standard-deviation x)) (^ n .5)))   
       (p (* (if (< t 0) (t-cdf t n) (- 1 (t-cdf t n))) 2)) 
       (list2 (list t p))
       (list1 (list "t-value" "p-value"))  
       ) 
  list2 
)) 


(defun convert (str) 
"Evaluates a string, STR." 
(eval (with-input-from-string (num str) (read num)))
) 


(defun what-parm (ind) 
"Args: ind 
Returns a list of flags that determine what information must be received from 
the user before the tests that were chosen can be run."   
(let* ( 
      (f1 (if (or (which (= ind 0)) (which (= ind 1))) 0 nil))  
      (f2 (if (or (which (= ind 1)) (which (= ind 2)) (which (= ind 3))) 1 nil))
      (f3 (if (or (which (= ind 2)) (which (= ind 3))) 2 nil))  
      ) 
(list f1 f2 f3) 
))    

(defun get-parms (ind)
"Args: ind 
Makes a dialog box that prompts the user for information that is needed to 
run the tests that chosen." 
(if ind
 (let* (
     (cancel (send modal-button-proto :new "Cancel"))
     (title1 (send text-item-proto :new "Mean ="))
     (title2 (send text-item-proto :new "Number of Resamples ="))
     (title3 (send text-item-proto :new "Median ="))
     (val1 (send edit-text-item-proto :new "0"))
     (val2 (send edit-text-item-proto :new "100"))
     (val3 (send edit-text-item-proto :new "0"))
     (ls (select (list (list title1 val1) (list title2 val2) (list title3 val3))
                 (which (what-parm ind)))) 
     (ok (send modal-button-proto :new "OK"
         :action #'(lambda () 
                   (combine (convert (send val1 :text))
                            (convert (send val2 :text))
                            (convert (send val3 :text)))  )))
     (pdiag (send modal-dialog-proto :new
            (list ls  
                  (list ok cancel))  
            :default-button ok))
       )
 (send pdiag :modal-dialog)
 )
)) 


(defun get-tests (ind) 
"Args: ind 
Makes a list of the tests that were chosen to be performed." 
(if ind 
  (let* (
      (ls (list "t-test"
                "Bootstrapped t-test"  
                "Sign test" 
                "Signed Rank test" 
                "Chi-Square test" 
                "Goodness of Fit test")) 
      (test-ls (select ls ind)) 
      ) 
test-ls
  )  
)) 
 
(defun test-dialog ()
"Args: None
Dialog to select which tests to run."
(let* (
     (cancel (send modal-button-proto :new "Cancel"))
     (list1 (list "t-test" 
                  "Bootstrapped t-test"  
                  "Sign test" 
                  "Signed Rank test" ))
     (vlist1 (mapcar #'(lambda (x) (send toggle-item-proto :new x)) list1))
     (list2 (list "Chi-Square test"))
     (vlist2 (mapcar #'(lambda (x) (send toggle-item-proto :new x)) list2))
     (list3 (list "Goodness of Fit test" ))
     (vlist3 (mapcar #'(lambda (x) (send toggle-item-proto :new x)) list3))
     (ok (send modal-button-proto :new "OK"
         :action #'(lambda () (mapcar #'(lambda (x)
            (send x :value)) (adjoin vf (append vlist1 vlist2 vlist3))))))
     (vf (send choice-item-proto :new (list "Show" "Save") :value 0))
     (top1 (send text-item-proto :new "Tests of Location"))
     (top2 (send text-item-proto :new "Tests of Spread"))
     (top3 (send text-item-proto :new "Tests of Distribution"))
     (tip (send text-item-proto :new "Type of Action"))
     (pdiag (send modal-dialog-proto :new
                 (list (list (adjoin top1 vlist1) (adjoin top2 vlist2)
                       (adjoin top3 vlist3) (list tip vf))
                       (list ok cancel))
                  :default-button ok))
     )
(send pdiag :modal-dialog)
))
 


(defun t-quantile (val tt)
"Args: val tt 
Determines the proportion of values in TT that are less than VAL."
 (let* (
       (wh (which (> val (sort-data tt))))
       (num (if (eq wh nil) 0 (+ 1 (max wh))))
       )
(/ num (length tt))
))
 

