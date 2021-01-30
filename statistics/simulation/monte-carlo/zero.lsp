;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; This implements the root finders discussed in Numerical Recipes,
;;; second edition, section 9.1 - 9.3. We add some embellishments
;;; based on Thisted, and on the FZERO routine from the various
;;; FORTRAN libraries.
;;;
;;; version 0.5 -- secant does not work, brent is not finished
;;;
;;; version 1.0 -- 06-17-95
;;;                secant does work and brent is finished
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun bisection (func x1 x2 &key (eps .0001) (jmax 40))
(let* (
       (f (funcall func x1))
       (fmid (funcall func x2))
       (dx (abs (- x1 x2)))
       (rtb (if (< f 0.0) x1 x2))
       (itl 0)
       )
(loop
(let* (
       (xmid (+ rtb (setf dx (* .5 dx))))
       (fmid (funcall func xmid))
       )
(setf rtb (if (<= fmid 0.0) xmid rtb))
(if (or (< (abs dx) eps) (= fmid 0.0)) (return rtb))
(if (= itl jmax) (error "Maximum number of bisections reached") 
    (setf itl (1+ itl)))))
))

(defun regula-falsi (func x1 x2 &key (eps .0001) (maxit 30) (verbose nil))
(let* (
       (f1 (funcall func x1))
       (f2 (funcall func x2))
       (xl (if (< f1 0.0) x1 x2))
       (xh (if (< f1 0.0) x2 x1))
       (fl (if (< f1 0.0) f1 f2))
       (fh (if (< f1 0.0) f2 f1))
       (dx (- xh xl))
       (itl 0)
       )
(loop
(let* (
       (rtf (+ xl (/ (* dx fl) (- fl fh))))
       (ff (funcall func rtf))
       (del (if (< ff 0.0) (- xl rtf) (- xh rtf)))
       )
(if (< ff 0.0) (setf xl rtf fl ff)
    (setf xh rtf fh ff))
(setf dx (- xh xl))
(if verbose (format t "~,8f ~,8f ~,8f ~,8f~%" xh xl fh fl))
(if (or (< (abs del) eps) (= ff 0.0)) (return rtf))
(if (= itl maxit) (error "Maximum number of iterations reached")
    (setf itl (1+ itl)))))
))
     
(defun secant (func x1 x2 &key (eps .0001) (maxit 30) (verbose nil))
(let ((f1 (funcall func x1))
      (f2 (funcall func x2))
      (rts 0)
      (xl 0)
      (itl 0)
      (fl 0)
      (f 0))
  (cond ((< (abs f1) (abs f2)) (setf rts x1 xl x2 f f1 fl f2))
        (t (setf rts x2 xl x1 f f2 fl f1)))
  (loop
   (let (
         (dx (* f (/ (- xl rts) (- f fl))))
         )
(setf xl rts fl f) 
(incf rts dx) 
(setf f (funcall func rts))
(if verbose (format t "~,8f ~,8f ~,8f ~,8f~%" xl rts fl f))
(if (or (< (abs dx) eps) (= f 0.0)) (return rts))
(if (= itl maxit) (error "Maximum number of iterations reached")
    (setf itl (1+ itl)))))
))

(defun ridder (func x1 x2 &key (eps .0001) (maxit 30) (verbose nil))
(let (
      (fl (funcall func x1))
      (fh (funcall func x2))
      (xl x1)
      (xh x2)
      (ans -1.11e30)
      (xnew 0)
      )
(if (< (* fl fh) 0.0)
    (loop
     (let* (
            (xm (* 0.5 (+ xl xh)))
            (fm (funcall func xm))
            (ss (sqrt (- (* fm fm) (* fl fh))))
            )
       (if (= ss 0.0) (return ans))
       (setf xnew (+ xm (/ (* (- xm xl) (if (> fl fh) 1 -1) fm) ss)))
       (if (<= (abs (- xnew ans)) eps) (return ans))
       (setf ans xnew fnew (funcall func ans))
       (if (= fnew 0.0) (return ans))
       (cond ((not (= (msign fm fnew) fm))
              (setf xl xm fl fm xh ans fh fnew))
             ((not (= (msign fl fnew) fl))
              (setf xh ans fh fnew))
             ((not (= (msign fh fnew) fh))
              (setf xl ans fl fnew)))
       (if verbose (format t "~,8f ~,8f ~,8f ~,8f~%" xl xh fl fh))
       (if (<= (abs (- xh xl)) eps) (return ans)))))
))

(defun brent (func x1 x2 &key (eps 1e-16) 
                   (tol 0.0001) (maxit 100) (verbose nil))
  (let ((a x1) (b x2) (xm 0) (tol1 0) (min1 0) (min2 0)
        (c 0) (d 0) (e 0) (p 0) (q 0) (r 0) (s 0)
        (fa (funcall func x1))
        (fb (funcall func x2))
        (fc (funcall func x2)))
    (loop
     (if (> (* fb fc) 0.0) (setf c a fc fa e (- b a) d (- b a)))
     (if (< (abs fc) (abs fb)) 
         (setf a b b c c a fa fb fb fc fc fa))
     (setf tol1 (+ (* 2.0 eps (abs b)) (* .5 tol))
           xm (* 0.5 (- c b)))
     (if (or (<= (abs xm) tol1) (= fb 0.0)) (return b))
     (cond ((and (>= (abs e) tol1) (> (abs fa) (abs fb)))
            (setf s (/ fb fa))
            (if (= a c) (setf p (* 2 xm s) q (- 1 s))
              (setf q (/ fa fc) r (/ fb fc) 
                    p (* s (- (* 2 xm q (- q r))
                              (* (- b a) (1- r))))
                    q (* (1- q) (1- r) (1- s))))
            (if (> p 0) (setf q (- q)))
            (setf p (abs p)
                  min1 (- (* 3 xm q) (abs (* tol1 q)))
                  min2 (abs (* e q)))
            (if (< (* 2 p) (min min1 min2))
                (setf e d d (/ p q))
              (setf d xm e d)))
           (t (setf d xm e d)))
     (setf a b fa fb)
     (if (> (abs d) tol1) (incf b d)
       (incf b (if (> xm 0) (abs tol1) (- (abs tol1)))))
     (setf fb (funcall func b))
     (if verbose (format t "~,8f ~,8f ~,8f ~,8f~%" a b fa fb))
     )
    )
  )

(defun msign (x y)
(* (abs x) (cond ((> y 0) 1) ((< y 0) -1) (t 0))))

#|
;;; here is an example function from Thisted

(defun example (x)
(let (
     (c1 .61489)
     (c2 .38511)
     (c3 (exp (- x)))
     )
(+ (- (- (/ (* 3062 c2 c3) (+ c1 (* c2 c3)))) 1013) (/ 1628 x))
))
|#
     
(provide "zero") 


