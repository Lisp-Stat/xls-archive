(defun sn-estimate (x)
 (let* (
        (x (sort-data x))
        (n (length x))
        (endlist (repeat nil n))
        (cn 1)
        (midval nil)
       )
   (setf (elt endlist 0) (- (elt x (floor (/ n 2))) (elt x 0)))
   (setf endlist (sub-fcn x n endlist t))
   (setf endlist (sub-fcn x n endlist nil))
   (setf (elt endlist (1- n)) (- (elt x (1- n)) 
                                 (elt x (floor (1- (/ (1+ n) 2))))))
   (cond ((> n 9) 
          (if (= (mod n 2) 1) (setf cn (/ n (- n .9)))))
         (t
          (case n (2 (setf cn .743))
                  (3 (setf cn 1.851))
                  (4 (setf cn .954))
                  (5 (setf cn 1.351))
                  (6 (setf cn .993))
                  (7 (setf cn 1.198))
                  (8 (setf cn 1.005))
                  (9 (setf cn 1.131)))))
   
   (setf midval (quantile endlist (/ (1- n) (* 2 n))))
   (* cn 1.1926 midval)
 )
)


(defun sub-fcn (x n endlist ind)
(let (
      (seq1 (if ind (iseq 2 (floor (/ (1+ n) 2))) 
                    (iseq (floor (1+ (/ (1+ n) 2))) (1- n))))
     )
(dolist (i seq1 endlist)
 (let* (
        (na (if ind (1- i) (- n i)))
        (nb (if ind (- n i) (1- i)))
        (diff (- nb na))
        (lefta 1)
        (leftb 1)
        (righta nb)
        (rightb nb)
        (amin (floor (1+ (/ diff 2))))
        (amax (floor (+ na (/ diff 2))))
        (even 0)
        (half 0)
        (length 0)
        (trya 0)
        (tryb 0)
        (meda 0)
        (medb 0)
       )
  (loop 
   (cond ((< lefta righta)
          (setf length (floor (1+ (- righta lefta))))
          (setf even (- 1 (mod length 2)))
          (setf half (floor (/ (1- length) 2)))
          (setf trya (floor (+ half lefta)))
          (setf tryb (floor (+ half leftb)))
          (cond ((< trya amin)
                 (setf rightb tryb)
                 (setf lefta (+ even trya)))
                (t 
                   (cond ((> trya amax)
                          (setf righta trya)
                          (setf leftb (+ even tryb)))
                         (t 
                            (cond (ind  
                                   (setf meda (- (elt x (1- i))
                                     (elt x (floor (- (+ i amin) trya 2)))))
                                   (setf medb (- (elt x (floor (1- (+ tryb i))))
                                           (elt x (1- i)))))
                                  (t
                                   (setf meda (- (elt x 
                                                   (floor (+ i trya (- amin))))
                                                 (elt x (1- i))))
                                   (setf medb (- (elt x (1- i))
                                             (elt x (floor (- i tryb 1)))))))

                            (cond ((>= meda medb)
                                   (setf righta trya)
                                   (setf leftb (+ even tryb)))
                                  (t
                                     (setf rightb tryb)
                                     (setf lefta (+ even trya)))
                            ))
                   ))
          ))
         (t (return))
    )
   )
   (cond ((> lefta amax)
          (cond (ind 
                 (setf (elt endlist (1- i)) (- (elt x (floor (1- (+ leftb i))))
                                               (elt x (1- i)))))
                (t
                 (setf (elt endlist (1- i)) (- (elt x (1- i))
                                              (elt x (floor (- i leftb 1))))))))
         (t
          (cond (ind 
                  (setf meda (- (elt x (1- i)) 
                                (elt x (floor (- (+ i amin) lefta 2)))))
                  (setf medb (- (elt x (floor (1- (+ i leftb))))
                                       (elt x (1- i)))))
                (t
                  (setf meda (- (elt x (floor (- (+ i lefta) amin)))
                                (elt x (1- i))))
                  (setf medb (- (elt x (1- i)) (elt x (floor (- i leftb 1)))))))
          
          (setf (elt endlist (1- i)) (min meda medb))))
))))




      

