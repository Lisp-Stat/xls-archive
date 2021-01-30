;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This is the simulated annealing algorithm discussed by Corana
;; et al in ACM Transactions on Mathematical Software, 13, 1987,
;; 262 - 280. It has been used (and slightly adapted) by Goffe
;; et al in Journal of Econometrics, 60, 1994, 65-100.
;;
;; FORTRAN code from Goffe is in simann.f
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun annealing (x0 v0 t0 func &key (eps 1e-6) (neps 4) (ns 20)   
                     (cu (repeat 2.0 (length x0)))
                     (nt (max 100 (* 5 (length x0))))
                     (rt 0.85) (verbose 2) 
                     (upper (repeat positive-infinity (length x0)))
                     (lower (repeat negative-infinity (length x0))))
   (let* (
         (n (length x0))
         (fc (funcall func x0))
         (xc x0)
         (vc v0)
         (tc t0)
         (xopt xc)
         (fopt fc)
         (nu (repeat 0 n))
         (ff (repeat fc neps))
         (nacc 0)
         (nfnc 0)
         (ntop 0)
         )                      
     (loop
      (dotimes (k nt)
        (dotimes (j ns)
          (dotimes (i n)
            (let* (
                   (xr (* (- 1 (random 2.0)) (elt vc i)))
                   (xq (+ (elt xc i) xr))
                   (li (elt lower i))
                   (ui (elt upper i))
                   (xp (if (or (< xq li) (> xq ui))
                           (+ li (* (random 1.0) (- ui li))) xr))
                   (xt (+ xc (* xp (unit n i))))
                   (ft (funcall func xt))
                   )
              (if (< ft fc)
                  (block ok
                         (setf xc xt fc ft)
                         (incf (elt nu i))
                         (incf nacc)
                         (if (< ft fopt)
                             (setf xopt xc fopt fc)))
                  (if (metropolis fc ft tc)
                      (block ok-too
                             (setf xc xt fc ft)
                             (incf (elt nu i))
                             (incf nacc))))
                   (incf nfnc)
                   (if (> verbose 4)
                       (format t "i-loop ~d ~,10f ~%" i fopt))
                   ))
                 (if (> verbose 3)
                     (format t "j-loop ~d ~,10f ~%" j fopt))
                )
               (if (> verbose 1)
                   (format t "k-loop ~d ~,10f~%" k fopt))
               (setf vc (update-steps vc nu ns cu))
               (if (> verbose 2)
                   (block inter
                   (format t "step sizes~%")
                   (prin1 vc) (terpri)
                   (format t "acceptance rates~%")
                   (prin1 (/ nu (* n ns))) (terpri)))        
               (setf nu (make-list n :initial-element 0))
          )
      (setf tc (reduce-temp rt tc))
      (setf ff (combine fc (but-last ff)))
      (if (and (<= (max (abs (- fc ff))) eps) (<= (- fc fopt) eps))
          (return (values fopt xopt)))
      (setf xc xopt fc fopt)
      (if (> verbose 0)
          (block top
                 (format t "top-loop ~d ~,10f ~,10f ~d ~d ~%" 
                         ntop fopt tc nacc nfnc)
                 (prin1 xopt) (terpri)))
      (incf ntop)
      )
     )
  )

(defun unit (n i)
  (if-else (= i (iseq n)) (repeat 1 n) (repeat 0 n)))

(defun but-last (x)
  (select x (iseq (1- (length x)))))

(defun metropolis (fc ft tc)
  (< (random 1.0) (exp (/ (- fc ft) tc)))
)

(defun update-steps (vc nu ns cu upper lower)
  (let (
        (n (length vc))
        (vn (copy-list vc))
        )
    (dotimes (i n vn)
             (let* (
                    (nuu (elt nu i))
                    (vcu (elt vc i))
                    (ccu (elt cu i))
                    (del (- (elt upper i) (elt lower i)))
                    (rat (/ nuu ns))
                    )
             (if (> rat .6)
                    (setf (elt vn i)
                          (* vcu (1+ (* ccu (/ (- rat 0.6) 0.4))))))
             (if (< rat .4)
                    (setf (elt vn i)
                          (/ vcu (1+ (* ccu (/ (- 0.4 rat) 0.4))))))
             (if (> (elt vn i) del) (setf (elt vn i) del))
             )
)))
          
(defun reduce-temp (rt tc)
  (* rt tc))







