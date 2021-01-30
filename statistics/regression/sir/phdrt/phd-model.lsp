;;;;
;;;; phd-model.lsp XLISP-STAT sir model proto and methods
;;;; sir 1.0 Copyright (c) 1990 by Ker-Chau Li
;;;; XLISP-STAT 2.1 Copyright (c) 1990, by Luke Tierney
;;;; Additions to Xlisp 2.1, Copyright (c) 1989 by David Michael Betz
;;;; You may give out copies of this software; for conditions see the file
;;;; COPYING included with this distribution.
;;;;
;;;;
;;;;
;;;; pHd Model Prototype
;;;;
;;;;

(defproto phd-model-proto
  '(x y eigen-vectors eigen-values total-evalue)
  ()
  *object*
  "Principal Hessian Directions Model")

(defun phd-model (x y &key (print T))
"Args: (x y  &key(print T))
X           - list of independent variables (list of lists)
Y           - dependent variable.
PRINT       - if not NIL print summary information and plot sir1-y-sir2
Returns a sir model object. To examine the model further assign the
result to a variable and send it messages.
Messages include :
                  x         -  original independent variables
                  matrix-x  -  matrix form of x  
                  y         -  original dependent variable
                  phd1      -  the variable for the first direction of sir
                  phd2      -  the variable for the second direction of sir
                  phd3      -  the variable for the third direction of sir
                  newx      -  the list of variables for all sir directions
                  nobs      -  # of observations in the data set
                  dim       -  # of independent variables
                  eigen-values   -  list of all absolute
                               values of eigenvalues
                  eigen-vectors  -  list of eigenvectors in list form
                  total-evalue   -  sum of all eigenvalues
                  p-values  -  p-values for checking if directions found are
                               real. if only the first k p-values are needed,
                               use the format :
                               :p-values :only k
                  plot1     -  spin-plot of phd1-y-phd2
                  plot2     -  spin-plot of phd1-phd2-phd3"

  (let (
        (m (send phd-model-proto :new))
        )
    (send m :x x)
    (send m :y y)
    (send m :compute)
    (if print (send m :display))
    m))

(defmeth phd-model-proto :isnew () (send self :needs-computing t))

(defmeth phd-model-proto :save ()
  `(phd-model',(send self :x)
                     ',(send self :y)
                     ))
(defmeth phd-model-proto :compute ()
  (let*(
        (x (send self :x))
        (y (send self :y))
        (phdout (phd-no-output x y))
        )
    (setf (slot-value 'x) x)
    (setf (slot-value 'y) y)
    (setf (slot-value 'eigen-vectors) (nth 0 phdout))
    (setf (slot-value 'eigen-values) (nth 1 phdout))
    (setf (slot-value 'total-evalue) (nth 2 phdout))
    ))

(defmeth phd-model-proto :needs-computing (&optional set)
  (if set (setf (slot-value 'eigen-values) nil))
  (null (slot-value 'eigen-values)))

(defmeth phd-model-proto :display ()
  (let (
        (evt (send self :eigen-vectors))
        )
    (format t "==============================================================")
    (format t "~2%      **** Principal Hessian Directions Model ****")
    (format t "~2%Number of observation:               ~s" (send self :nobs))
    (format t "~2%Number of independent variables:      ~s" (send self :dim))
    (format t "~2%the first direction found by phd:~%")
    (format t "~s" (nth 0 evt))
    (format t "~2%the second direction found by phd:~%")
    (format t "~S"(nth 1 evt))
    (format t "~2%the third direction found by phd:~%")
    (format t "~s"(nth 2 evt))
    (format t "~2%the companion output eigenvalues of phd:~%")
    (format t "~s"(send self :eigen-values))
    (format t "~2%the sum of all eigenvalues:~%")
    (format t "~s"(send self :total-evalue))
    (format t "~2%==============================================================~%")
;    (send self :plot1)
;    (send self :plot2)
    ))

(defmeth phd-model-proto :x (&optional new-x)
  (when new-x
        (setf (slot-value 'x) new-x)
        (send self :needs-computing t))
  (slot-value 'x))

(defmeth phd-model-proto :y (&optional new-y)
  (when new-y
        (setf (slot-value 'y) new-y)
        (send self :needs-computing t))
  (slot-value 'y))

(defmeth phd-model-proto :eigen-values ()
  (slot-value 'eigen-values))

(defmeth phd-model-proto :eigen-vectors ()
  (slot-value 'eigen-vectors))

(defmeth phd-model-proto :total-evalue ()
  (slot-value 'total-evalue))

(defmeth phd-model-proto :newx ()
  (let*(
        (mx (send self :matrix-x))
        (nx (copy-list (send self :x)))
        (evector (send self :eigen-vectors))
        )
      (dotimes (i (send self :dim))
           (setf (nth i nx) (coerce (%* mx (nth i evector)) 'list)))
    nx))

(defmeth phd-model-proto :matrix-x ()
  (let*(
        (x (send self :x))
        (dim (send self :dim))
        (nobs (send self :nobs))
        (mx (transpose (make-array (list dim nobs) :initial-contents x)))
        )
    mx))

(defmeth phd-model-proto :phd1 ()
  (let*(
        (mx (send self :matrix-x))
        (ev1 (nth 0 (send self :eigen-vectors)))
        )
    (coerce (%* mx ev1) 'list)))

(defmeth phd-model-proto :phd2 ()
  (let*(
        (mx (send self :matrix-x))
        (ev2 (nth 1 (send self :eigen-vectors)))
        )
    (coerce (%* mx ev2) 'list)))

(defmeth phd-model-proto :phd3 ()
  (let*(
        (mx (send self :matrix-x))
        (ev3 (nth 2 (send self :eigen-vectors)))
        )
    (coerce (%* mx ev3) 'list)))

(defmeth phd-model-proto :nobs ()
  (length (send self :y)))

(defmeth phd-model-proto :dim ()
  (length (send self :x)))

(defmeth phd-model-proto :p-values (&key (only nil))
  (let*(
        (dim (send self :dim))
        (nobs (send self :nobs))
        (tev (send self :total-evalue))
        (dim1 (if only only dim))
        (evl (send self :eigen-values))
        (sq-ev (sum (** evl 2)))
        (var-y  (** (standard-deviation (send self :y)) 2))
        count
        pv
        chi
        (p-val (repeat 0 dim1))
        )
    (dotimes (i dim1)
             (setf count (+ i 1))
             (setf chi (chisq-cdf (/ (*  nobs   sq-ev ) (* 2 var-y))  (/ (* (- dim i) (+ 1 (- dim i))) 2)))
             (setf pv (- 1 chi))
             (setf (nth i p-val) pv)
             (setf sq-ev (- sq-ev (** (nth i evl) 2)))
             )
    p-val))

(defmeth phd-model-proto :plot1 ()
  (setf plot1 (spin-plot (list (send self :phd1) (send self :y) 
                               (send self :phd2))
                         :variable-labels (list "phd1" "y" "phd2")
                         :title "Phd-view: phd1-y-phd2"
                         :location '(618 310) :size '(280 180)))
  (send plot1 :linked t)
  )

(defmeth phd-model-proto :plot2 ()
  (setf plot2 (spin-plot (list (send self :phd1) (send self :phd2) 
                               (send self :phd3))
                         :variable-labels (list "phd1" "phd2" "phd3")
                         :title "Phd-view: phd1-phd2-phd3"))
  (send plot2 :linked t)
  )
;;===========================================================

(defun phd-no-output (x y)
"phd method, by li, output ( (eigenvectors-rowvectors, eigenvalues), newx) "
(let* ( (nobs(length y))
        (dim (length x))
        (mx (transpose (make-array (list dim nobs) :initial-contents x)))
        (syxx (covw x (- y (mean y))))
        (sxx (covw x (repeat 1 nobs)))
        (meat (eigen-decomp syxx sxx))
        (evalues (coerce (nth 1 meat) 'list))
        (total-evalue (sum evalues))
        (ev (coerce (nth 0 meat) 'list))
        (order-evalue (reverse (order (abs evalues))))
        (evalues (select evalues order-evalue))
        (evv (repeat 0 dim))
        (evector (repeat 0 dim))
        )

  (dotimes (i dim)
           (setf (nth i evv) (coerce (nth (nth i order-evalue) ev) 'list))
           )
  (list evv evalues total-evalue)
  ))
;;============================================================
