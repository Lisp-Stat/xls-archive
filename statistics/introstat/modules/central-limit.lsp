(defun central-limit-theorem ()
"Args: none.
Demonstrates the central limit theorem of an arbitrary
statistic (as long as it is known to the current environment), 
sampling from an arbitrary population (as long
as a random number generator and a density function are
defined in the environment)."
(let* (
     (args (central-limit-dialog))
     (f1 (elt (various-parents (elt args 0)) 0))
     (f2 (elt (various-parents (elt args 0)) 1))
     (g (elt args 1))
     (n (elt args 2))
     (m (elt args 3))
     (p (elt args 4))
	    (s (elt args 5))
     (b (make-list n))
     (q (histogram nil :title "Sampling Distribution" :show nil))
     )
(send q :location 35 200)
(send q :size 500 150)
(send q :show-window)
(dotimes (i n)
    (setf (nth i b) (sample-me f1 f2 g m s i p))
    (send q :add-points (list (nth i b)))
    (send q :draw-string (num-to-string (1+ i)) 20 20)
    (send q :adjust-to-data)
    )
(send q :add-lines (kernel-dens b))
(send q :add-lines (fit-normal b))
(send q :add-lines (list (repeat (quantile b .025) 2) (list 0 n)) :type 'dashed)
(send q :add-lines (list (repeat (quantile b .975) 2) (list 0 n)) :type 'dashed)
))

(defun central-limit-dialog ()
"Args: none
Dialog to select user parameters" 
(let* (
  	 (first-label (send text-item-proto :new "Parent Population"))
    (second-label (send text-item-proto :new "Statistic"))
  	 (third-label (send text-item-proto :new "Number of Samples"))
  	 (fourth-label (send text-item-proto :new "Sample Size"))
  	 (fifth-label (send text-item-proto :new "Number of Full Displays"))
  	 (sixth-label (send text-item-proto :new "Pause in Full Displays"))
  	 (first-reply (send edit-text-item-proto :new "normal"))
  	 (second-reply (send edit-text-item-proto :new "mean"))
  	 (third-reply (send edit-text-item-proto :new "100"))
  	 (fourth-reply (send edit-text-item-proto :new "12"))
  	 (fifth-reply (send edit-text-item-proto :new "3"))
  	 (sixth-reply (send edit-text-item-proto :new "5"))
  	 (cancel (send modal-button-proto :new "Cancel"))
  	 (ok (send modal-button-proto :new "OK" 
           :action #'(lambda()
                     (list
                     (send first-reply :text)
                     (send second-reply :text)
                     (dconvert (coerce (send third-reply :text) 'list))
                     (dconvert (coerce (send fourth-reply :text) 'list))
                     (dconvert (coerce (send fifth-reply :text) 'list))
                     (dconvert (coerce (send sixth-reply :text) 'list))))
           ))
  (choice-dialog (send modal-dialog-proto :new
         (list 
         (list
         (list first-label second-label third-label 
               fourth-label fifth-label sixth-label)
		       (list first-reply second-reply third-reply 
               fourth-reply fifth-reply sixth-reply))
         (list ok cancel)
         )
         :default-button ok)))
  (send choice-dialog :modal-dialog)
))

(defun sample-me (f1 f2 g m spd i p)
(let* (
      (w (if (< i p) (histogram nil :title "Sample")))
      (ff (intern (string-upcase f1)))
      (fd (intern (string-upcase f2)))
      (gg (intern (string-upcase g)))
      (x (funcall ff m))
      (y (rseq -3 3 50))
      (z (funcall fd y))
      )
;(break)
(if (< i p) (send w :add-points x))
(if (< i p) (send w :add-lines (list y z) :draw nil))
(if (< i p) (send w :adjust-to-data))
(if (< i p) (pause (* 60 spd)))
(if (< i p) (send w :close))
(funcall gg x)
))

;;; miscellaneous utilities

(defun dconvert (cs)
"Args: list
Converts a LIST of characters to an integer. Only
characters -, +, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9 
are allowed. Leading zeroes or spaces are no problem.
Neither are trailing spaces."
(let  (
      (sgn 1)
      )
(cond ((member #\+ cs) (setf cs (rest (member #\+ cs))))
      ((member #\- cs) (setf cs (rest (member #\- cs)))
                       (setf sgn -1))
      )
(let*  (
       (tc (- (mapcar #'char-code cs) 48))
       (v (select tc (which (>= tc 0))))
       (b (reverse (** 10 (iseq (length v)))))
       )
(* sgn (sum (* b v)))
)))

(defun fit-normal (x)
"Args: x
Computes mean and standard deviation, and returns list of
list of arguments and list of ordinates, suitable for
add-lines in one-dim plots"
(let* (
       (m (mean x))
       (s (standard-deviation x))
       (u (max x))
       (v (min x))
       (a (/ (- u v) 50))
       (b (/ (+ u v) 2))
       (z (+ b (* a (- (iseq 51) 25))))
       (p (/ (normal-dens (/ (- z m) s)) s))
       )
(list z p)))

;;; some additional statistics (alternatives to min, max, 
;;; mean, median, standard-deviation, interquartile-range)

(defun range (x)
(- (max x) (min x)))

(defun mid-range (x)
(/ (range x) 2))

(defun skewness (x)
(let* (
      (m (mean x))
      (n (length x))
      (r (- x m))
      )
(/ (/ (sum (^ r 3)) n) (^ (/ (sum (^ r 2)) n) 1.5))
))

(defun kurtosis (x)
(let* (
      (m (mean x))
      (n (length x))
      (r (- x m))
      )
(- (/ (/ (sum (^ r 4)) n) (^ (/ (sum (^ r 2)) n) 2)) 3)
))

(defun various-parents (f)
(let (
     (f-rand nil)
     (f-dens nil)
     )
(cond ((string= f "normal") 
       (setf f-rand "normal-rand") 
       (setf f-dens "normal-dens"))
      ((string= f "uniform")
       (setf f-rand "uniform-rand") 
       (setf f-dens "uniform-dens"))
      ((string= f "cauchy")
       (setf f-rand "cauchy-rand") 
       (setf f-dens "cauchy-dens")))
(list f-rand f-dens))
)
  
(defun uniform-dens (x)
(flet (
      (uni (z) (cond ((< z 0) 0) ((> z 1) 0) (t 1)))
      )
(if (sequencep x) (mapcar #'uni x) (uni x)))
)


