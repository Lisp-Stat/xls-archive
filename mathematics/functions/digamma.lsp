;;LISP-STAT supplies a log-gamma function, but not the first
;;and second derivatives (the digamma and trigamma functions).
;;Here are some simple functions for calculating them. For 
;;details, see Bernardo (1976), Appl. Stat., 315-317, and
;;Schneider (1978), Appl. Stat., 97-99, also Francis (1991?),
;;Appl. Stat., 514-515.

 
;;;; LISP-STAT functions for evaluating the first and
;;;; second derivatives of the log-gamma function (the
;;;; digamma and trigamma functions, respectively).
;;;; Both functions take a real positive argument.
 
(defun digam (x)
  (let ((s3 8.333333333e-2)
        (s4 8.333333333e-3)
        (s5 3.968253968e-3)
        (d1 -0.5772156649))
    (cond ((<= x 0) (princ "error: non-positive argument - ") x)
          ((<= x 1e-5) (- d1 (/ x)))
          (t (do ((z    x (+ z 1))
                  (dgam 0 (- dgam (/ z))))
                 ((>= z 8.5) (setf y (/ z))
          (setf dgam (+ dgam (log z) (- (* 0.5 y))))
          (setf y (* y y))
          (- dgam (* y (- s3 (* y (- s4 (* y s5))))))))))))
 
(defun trigam (x)
  (let ((b2 0.1666666667)
        (b4 -0.03333333333)
        (b6 0.02380952381)
        (b8 -0.03333333333))
    (cond ((<= x 0) (princ "error: non-positive argument - ") x)
          ((<= x 1e-4) (/ (* x x)))
          (t (do ((z    x (+ z 1))
                  (tgam 0 (+ tgam (/ (* z z)))))
                 ((>= z 5) (setf y (/ (* z z)))
    (+ tgam (* 0.5 y)
       (/ (+ 1 (* y (+ b2 (* y (+ b4 (* y (+ b6 (* y b8))))))))
          z))))))))



;; Paul E. Green                             E-mail: pgreen@umich.edu
;; University of Michigan                    Phone: (313) 764-8213
;; Transportation Research Institute         Fax:   (313) 936-1081


