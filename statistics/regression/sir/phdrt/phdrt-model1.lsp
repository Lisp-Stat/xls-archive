;;;;
;;;; phdrt-model.lsp (XLISP-STAT) proto and methods
;;;; phd 1.0 Copyright (c) 1991 by Ker-Chau Li
;;;; XLISP-STAT 2.1 Copyright (c) 1990, by Luke Tierney
;;;; Additions to Xlisp 2.1, Copyright (c) 1989 by David Michael Betz
;;;; You may give out copies of this software; for conditions see the file
;;;; COPYING included with this distribution.
;;;;
;;;;
;;;;
;;;; pHd-based Regression Trees Model Prototype
;;;;
;;;;


(defproto phdrt-model-proto
       '(x oldx y directions eigen-values r-squares p-val 
         node-size sig-hat t-sighat2 r2-old sighat-old resid) 
       ()
      *object*
      "Principal-Hessian-Direction-based Regression Trees Model")


(defun phdrt-model (x oldx y &key (print T))
"Args: (x y  &key(print T))
X           - list of independent variables (list of lists)
Y           - dependent variable.
PRINT       - if not NIL print summary information and plot sir1-y-sir2
Returns a sir model object. To examine the model further assign the
result to a variable and send it messages.
Messages include :
                  x         -  a list of original independent variables
                  matrix-x  -  matrix form of x
                  y         -  original dependent variable
                  set11     -  the subset corresponding to 1st
                  set12     -  the subset corresponding to 2nd
                  case-2
                  set21     -  the subset corresponding to first
                  set22     -  the subset corresponding to second
                  set23     -  the subset corresponding to third 
                  set24     -  the subset corresponding to four
                  case-4
                  nobs      -  # of observations in the data set
                  dim       -  # of independent variables
                  phd1      -  the variable for the first component of  pHd
                  phd2      -  the variable for the second component of pHd
                  phd3      -  the variable for the third component of  pHd
                  node-size -  the sample sizes of each subnodes
                  p-val     -  the p-values of the pHd's
                  plot1     -  spin-plot of phd1-residuals-phd2
                  r-squares -  the R-squared for each subnodes
                  directions-  list of principal Hessian directions (pHd)'s
                               with cut-point in list form
                  sig-hat   -  estimated sigma-hat.
                  t-sighat2 -  the prediction weighted sigma-hat in the
                               immediate node.
                  eigen-values- list of all absolute values of eigenvalues."
  (let (
        (m (send phdrt-model-proto :new))
       )
       (send m :x x)
       (send m :oldx oldx)
       (send m :y y)
       (send m :compute)
       (if print (send m :display))
     m))

(defmeth phdrt-model-proto :isnew () (send self :needs-computing t))

(defmeth phdrt-model-proto :save ()
       `(phdrt-model',(send self :x)
                    ',(send self :y)
                     ))

(defmeth phdrt-model-proto :compute ()
  (let*(
        (x   (send self :x))
        (oldx(send self :oldx))
        (y   (send self :y))
        (phdout (phdrt-output x oldx y))
       )
    (setf (slot-value 'x) x)
    (setf (slot-value 'oldx) oldx)
    (setf (slot-value 'y) y)
    (setf (slot-value 'directions)   (nth 0 phdout))
    (setf (slot-value 'eigen-values) (nth 1 phdout))
    (setf (slot-value 'r-squares)    (nth 2 phdout))
    (setf (slot-value 'p-val)        (nth 3 phdout))
    (setf (slot-value 'node-size)    (nth 4 phdout))
    (setf (slot-value 'sig-hat)      (nth 5 phdout))
    (setf (slot-value 't-sighat2)    (nth 6 phdout))
    (setf (slot-value 'r2-old)       (nth 7 phdout))
    (setf (slot-value 'sighat-old)   (nth 8 phdout))
    (setf (slot-value 'resid)        (nth 9 phdout))
    ))

(defmeth phdrt-model-proto :needs-computing (&optional set)
      (if set (setf (slot-value 'eigen-values) nil))
      (null (slot-value 'eigen-values)))

(defmeth phdrt-model-proto :display ()
      (let (
            (evt (send self :directions))
           )
(format t "===============================================================")
(format t "~2%**** Principal Hessian Directions Regression Tree Model ****")
(format t "~2%Number of observations:               ~s" (send self :nobs))
(format t "~2%Number of independent variables:      ~s" (send self :dim))
    (format t "~2%the p-values found by pHd:~%")
    (format t "~s"(send self :p-val))
    (format t "~2%the companion output eigenvalues of pHd:~%")
    (format t "~s"(send self :eigen-values))
    (format t "~2%the first direction found by pHd:~%")
    (format t "~s"(nth 0 evt))
    (format t "~2%the second direction found by pHd:~%")
    (format t "~S"(nth 1 evt))
    (format t "~2%the third direction found by pHd:~%")
    (format t "~s"(nth 2 evt))
    (format t "~2%the r-squared of this node:~%")
    (format t "~s"(send self :r2-old))
    (format t "~2%the sigma-hat of this node:~%")
    (format t "~s"(send self :sighat-old))
    (format t "~2%the r-squared values of splitted subnodes:~%")
    (format t "~s"(send self :r-squares))
    (format t "~2%the sigma-hat values of splitted subnodes:~%")
    (format t "~s"(send self :sig-hat))
    (format t "~2%splitted sizes:~%")
    (format t "~s"(send self :node-size))
    (format t "~2%the prediction weighted sigma-hat:~%")
    (format t "~s"(send self :t-sighat2))
(format t "~2%==============================================================~%")
;    (send self :plot1)
     ))

(defmeth phdrt-model-proto :x (&optional new-x)
  (when new-x
        (setf (slot-value 'x) new-x)
        (send self :needs-computing t))
  (slot-value 'x))

(defmeth phdrt-model-proto :oldx (&optional new-x)
  (when new-x
        (setf (slot-value 'oldx) new-x)
        (send self :needs-computing t))
  (slot-value 'oldx))

(defmeth phdrt-model-proto :y (&optional new-y)
  (when new-y
        (setf (slot-value 'y) new-y)
        (send self :needs-computing t))
  (slot-value 'y))

(defmeth phdrt-model-proto :directions ()
  (slot-value 'directions))

(defmeth phdrt-model-proto :eigen-values ()
  (slot-value 'eigen-values))

(defmeth phdrt-model-proto :r-squares ()
  (slot-value 'r-squares))

(defmeth phdrt-model-proto :p-val ()
  (slot-value 'p-val))

(defmeth phdrt-model-proto :node-size()
  (slot-value 'node-size))

(defmeth phdrt-model-proto :sig-hat()
  (slot-value 'sig-hat))

(defmeth phdrt-model-proto :t-sighat2()
  (slot-value 't-sighat2))

(defmeth phdrt-model-proto :r2-old()
  (slot-value 'r2-old))

(defmeth phdrt-model-proto :sighat-old()
  (slot-value 'sighat-old))

(defmeth phdrt-model-proto :resid()
  (slot-value 'resid))

(defmeth phdrt-model-proto :nobs ()
  (length (send self :y)))

(defmeth phdrt-model-proto :dim ()
  (length (send self :x)))

(defmeth phdrt-model-proto :matrix-x ()
  (let*(
        (x   (send self :x))
        (dim (send self :dim))
        (nobs (send self :nobs))
        (mx (transpose (make-array (list dim nobs) :initial-contents x)))
        )
    mx))

(defmeth phdrt-model-proto :phd1 ()
  (let*(
        (mx (send self :matrix-x))
        (ev1 (cdr (nth 0 (send self :directions))))
        )
    (coerce (%* mx ev1) 'list)))

(defmeth phdrt-model-proto :phd2 ()
  (let*(
        (mx (send self :matrix-x))
        (ev2 (cdr (nth 1 (send self :directions))))
        )
    (coerce (%* mx ev2) 'list)))

(defmeth phdrt-model-proto :phd3 ()
  (let*(
        (mx (send self :matrix-x))
        (ev3 (cdr (nth 2 (send self :directions))))
        )
    (coerce (%* mx ev3) 'list)))

(defmeth phdrt-model-proto :plot1 ()
  (let*(
        (x  (send self :x))
        (y  (send self :y))
        (reg (regression-model x y :print nil))
        (res (send reg :residuals))
        )
  (setf plot1(spin-plot (list (send self :phd1) res (send self :phd2))
                        :variable-labels (list "phd1" "residuals" "phd2")
                        :title "phd1-res-phd2 view "
                        :location '(600 600) :size '(280 220)))
                      (send plot1 :linked t)
 ;                     (send plot1 :use-color t)
 ;                     (send plot1 :mouse-mode 'hand-rotate)
 ;                     (send plot1 :axis-rotate)
  ))

(defmeth phdrt-model-proto :f-test ()
  (let*(
        (s1   (send self :set11))
        (s2   (send self :set12))
        (test (f_test  (nth 2 s1)(nth 1 s1)(nth 2 s2)(nth 1 s2)))
       ) 
     test))

(defmeth phdrt-model-proto :case-2 ()
  (let*(
        (p1x  (send self :phd1))
        (ba   (car (car (send self :directions))))
        (case-m (which (< p1x ba)))
        (case-p (which (>= p1x ba)))
       )
     (list case-m case-p)))

(defmeth phdrt-model-proto :set11 ()
  (let*(
        (x  (send self :x))
        (ox (send self :oldx))
        (y  (send self :y))
;        (p1x(send self :phd1))
;        (ba1(car (car (send self :directions))))
;        (case  (which (< p1x ba1)))
        (case  (car (send self :case-2)))
        (newx  (subset x case))
        (nx    (subset ox case))
        (newy  (select y case))
       )
     (list newx newy nx case)))

(defmeth phdrt-model-proto :set12 ()
  (let*(
        (x  (send self :x))
        (ox (send self :oldx))
        (y  (send self :y))
;        (p1x(send self :phd1))
;        (ba1(car (car (send self :directions))))
;        (case  (which (>= p1x ba1)))
        (case  (nth 1 (send self :case-2)))
        (newx  (subset x case))
        (nx    (subset ox case))
        (newy  (select y case))
       )
     (list newx newy nx case)))

(defmeth phdrt-model-proto :case-4 ()
  (let*( 
        (p1x(send self :phd1))
        (p2x(send self :phd2))
        (ba1(car (car (send self :directions))))
        (ba2(car (nth 1 (send self :directions))))
        (c1 (intersection (which (>= p1x ba1))
                          (which (>= p2x ba2))))
        (c2 (intersection (which (< p1x  ba1))
                          (which (>= p2x ba2))))
        (c3 (intersection (which (< p1x  ba1))
                          (which (< p2x  ba2))))
        (c4 (intersection (which (>= p1x ba1))
                          (which (< p2x  ba2))))
       )
    (list c1 c2 c3 c4)))

(defmeth phdrt-model-proto :set21 ()
  (let*( 
        (x  (send self :x))
        (ox (send self :oldx))
        (y  (send self :y))
        (p1x(send self :phd1))
        (p2x(send self :phd2))
        (ba1(car (send self :directions)))
        (ba2(nth 1 (send self :directions)))
        (case (intersection (which (>= p1x (car ba1)))
                            (which (>= p2x (car ba2)))))
        (newx  (transpose (select (transpose x) case)))
        (nx    (transpose (select (transpose ox) case)))
        (newy  (select y case))
       )
     (list newx newy nx case)))

(defmeth phdrt-model-proto :set22 ()
  (let*(
        (x  (send self :x))
        (ox (send self :oldx))
        (y  (send self :y))
        (p1x(send self :phd1))
        (p2x(send self :phd2))
        (ba1(car (send self :directions)))
        (ba2(nth 1 (send self :directions)))
        (case (intersection (which (< p1x (car ba1)))
                            (which (>= p2x (car ba2)))))
        (newx  (transpose (select (transpose x) case)))
        (nx    (transpose (select (transpose ox) case)))
        (newy  (select y case))
       )
     (list newx newy nx case)))

(defmeth phdrt-model-proto :set23 ()
  (let*(
        (x  (send self :x))
        (ox (send self :oldx))
        (y  (send self :y))
        (p1x(send self :phd1))
        (p2x(send self :phd2))
        (ba1(car (send self :directions)))
        (ba2(nth 1 (send self :directions)))
        (case (intersection (which (< p1x (car ba1)))
                            (which (< p2x (car ba2)))))
        (newx  (transpose (select (transpose x) case)))
        (nx    (transpose (select (transpose ox) case)))
        (newy  (select y case))
       )
     (list newx newy nx case)))

(defmeth phdrt-model-proto :set24 ()
  (let*(
        (x  (send self :x))
        (ox (send self :oldx))
        (y  (send self :y))
        (p1x(send self :phd1))
        (p2x(send self :phd2))
        (ba1(car (send self :directions)))
        (ba2(nth 1 (send self :directions)))
        (case (intersection (which (>= p1x (car ba1)))
                            (which (< p2x (car ba2)))))
        (newx  (transpose (select (transpose x) case)))
        (nx    (transpose (select (transpose ox) case)))
        (newy  (select y case))
       )
     (list newx newy nx case)))

;;=================================================================

(defun phdrt-output (x oldx y)
"pHd method by Dr. Li, (output: directions eigenvalues r-squared 
p-values node-size sigma-hat w-sigmahat) "
(let* (
        (reg   (reg-model x y :print nil))
        (res   (send reg :residuals))
        (nphd 1)
        (ba    (direction nphd x res))
        (evdir (select ba (iseq 0 2)))
        (evl   (nth 3 ba))
        (pval  (nth 4 ba))
        (px    (select ba (iseq 5 7)))
        (t-res (set-r2 (car evdir) (nth 1 evdir) (nth 2 evdir)
                       (car px) (nth 1 px) (nth 2 px) nphd x oldx y))
         rsq
         node-size
         sighat
         r2-o
         sighat-o
         t-sighat2
      )
     (setf rsq       (nth 0 t-res))
     (setf node-size (nth 1 t-res))
     (setf sighat    (nth 2 t-res))
     (setf r2-o      (nth 3 t-res))
     (setf sighat-o  (nth 4 t-res))
     (setf t-sighat2 (sq-sum sighat node-size))
  (list evdir evl rsq pval node-size sighat t-sighat2 r2-o sighat-o res)))
;;=======================================================================
