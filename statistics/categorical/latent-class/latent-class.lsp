(defun latent-class (yy nclass &optional (size t) (mono nil) (eps 1e-4))
"Args: (yy nclass &optional (fixed nil) (mono nil))
YY is a complete n x m true-false array (actually, a list of lists)
which is decomposed into NCLASS classes using latent class analysis.
We do not use the profile representation, but rather the score vector representation of the data. There is an option SIZE to keep the sizes of
the latent classes fixed and equal, and an option MONO to constrain 
the conditional probabilities of a positive reponse to be monotonic
over classes. These options are useful for nonparametric item
analysis. This is written to practice mapping and computing with
lists. I am pretty sure it can be done more efficiently with matrices."
(let* (
       (n (length yy))
       (m (length (first yy)))
       (yb (map-elements #'if-else yy 1 0))
       (pc (repeat (/ nclass) nclass))
       (pd (mapcar #'uniform-rand (repeat m nclass)))
       (pj (make-list nclass :initial-element (repeat 0 n)))
       (lo -1.e30)
     )
;;;
;;; outer iteration loop
;;;
(loop
;;;
;;; compute current joint distribution of data
;;; and states (the E step) in a loop over classes
;;;
(dotimes (s nclass)
(let* (
       (ps (elt pd s))
       (cd (mapcar #'(lambda (x) (if-else x ps (- 1 ps))) yy))
       )
(setf (elt pj s) (* (elt pc s) (mapcar #'prod cd)))
))
;;;
;;; compute marginals and posterior distributions
;;; and do the M step
;;;
(let* (
      (pu (apply #'+ pj))
      (ln (sum (log pu)))
      (pv (/ pj (make-list nclass :initial-element pu)))
      (s1 (mapcar #'(lambda (x) 
                      (apply #'+ (map-elements #'* x yb))) pv))
      (s2 (mapcar #'(lambda (x) 
                      (apply #'+ (map-elements #'* x (- 1 yb)))) pv))
      )
(setf pd (/ s1 (+ s1 s2)))
(if size 
    (setf pc (/ (mapcar #'sum pv) n)))
;;;
;;; generate some output within the loop
;;;
(format t "~f~%" ln)
(if (< eps (- ln lo)) (return))
))
;;;
;;; and perhaps some output to wind things up
;;;
(print-matrix (coerce pu 'vector))
(print-matrix (transpose (make-array (list nclass n) :initial-contents pv)))
(print-matrix (make-array (list nclass m) :initial-contents pd))
))



