;;;; Course project:  Sta2102S, University of Toronto, Spring 1993
;;;; Instructor: N. Reid
;;;; Student: A. Canty
;;;; Project Title:  Exercise 9.5, p. 273

;;;;
;;;;             EXERCISE   9.5
;;;;             PAGE       273
;;;;
;;;;    THIS IS A PROGRAM TO SET UP A METHOD FOR A GRAPH
;;;;    WHICH TAKES IN TWO INDICES (ZERO BASE) AND MAKES
;;;;    A SMOOTH TRANSFORMATION FROM THE CURRENT PLOT TO
;;;;    A PLOT OF THE TWO VARIABLES SPECIFIED.
;;;;    


(defproto my-graph-proto '() '() graph-proto)
;;  To avoid accidently changing any of graph-proto's own
;;  messages, I defined a new prototype to be a child of
;;  graph-proto and defined any new methods for this 
;;  prototype.




(defmeth my-graph-proto :sm-rotate 
         (i1 i2 &key (num-steps 10) (inter-type 'trig))
;;;
;;;    This is the method which answers exercise 9.5
;;;    It does so by calling a more general transformation
;;;    interpolation function :smooth to do the actual
;;;    work.
;;;
;;;    The parameters for this function are the two indices
;;;    which are required to be plot.  It also has two
;;;    keyword parameters which control the number of steps
;;;    the transformation takes and the type of interpolation
;;;    which is done
;;;
    (let* ((m (send self :num-variables))
           (ident (identity-matrix m))
           (rl (row-list ident))
           (ri1 (select rl i1))
           (ri2 (select rl i2))
           (rest (select rl (which (mapcar #'(lambda (x y) (and x y))
                                           (/= (iseq m) i1)
                                           (/= (iseq m) i2)))))
           (newrl (cons ri1 (cons ri2 rest)))
           (newtrans (make-array (repeat m 2)
                                 :initial-contents newrl)))
;;  These assignments simply do row changes on the identity
;;  matrix so that the rows corresponding to the required 
;;  variables are the first and second rows.  This matrix is
;;  then sent to :smooth and becomes the new transformation.
         (send self :smooth newtrans 
                            :num-steps num-steps 
                            :inter-type inter-type)))



#|

|#
(defmeth my-graph-proto :smooth 
         (trans &key (num-steps 10) (inter-type 'trig))
;;;
;;;    This is a more general method than is actually needed
;;;    for the exercise.  It basically takes any new trans-
;;;    formation matrix and smoothly rotates from the current
;;;    transformation to the new one.  It also has the same
;;;    keyword arguments as :sm-rotate to control the inter-
;;;    polation.
;;;
;;;    In it's present state it allows only two interpolation
;;;    types -- trigonometric and convex (p 300).  Trig-
;;;    onometric interpolation is the default type.
;;;
    (let* ((n num-steps)
           (temp (send self :transformation))
           (old-trans (if temp 
                          temp
                          (identity-matrix (send self :num-variables)))))
       (case inter-type
             (convex (dolist (p (rseq 0 1 num-steps))
                        (send self :transformation 
                                   (+ (* p trans)
                                      (* (- 1 p) old-trans)))))
             (t (dolist (theta (rseq 0 (/ pi 2) num-steps))
                   (send self :transformation 
                              (+ (* (sin theta) trans)
                                 (* (cos theta) old-trans))))))))



