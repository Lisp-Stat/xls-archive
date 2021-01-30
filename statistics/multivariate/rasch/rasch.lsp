;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; The rasch-model-proto allows for maximum likelihood analysis of
;; the Rasch model. It has (or will have) full maximum likelihood,
;; conditional maximum likelihood, marginal maximum likelihood
;; (both parametric and non-parametric).
;;
;; Jan de Leeuw
;;
;; Version 1.0 *** 03-18-95 
;; Version 1.1 *** 04-18-95
;;         Added Gustafsson initial estimates
;;         Added Newton-Raphson version of CML
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require "item-analysis")
(require "symmetric-functions")

(defproto rasch-model-proto
  '(eps tht row col) () item-analysis-proto)

(defmeth rasch-model-proto :isnew (data)
  (call-next-method data)
  (send self :set-row
        (send self :make-row))
  (send self :set-col
        (send self :make-col))
  (send self :set-eps
        (send self :make-eps))
  (send self :set-tht
        (send self :make-tht))
  )

(make-assessors rasch-model-proto
               (data) (eps tht row col))

(defmeth rasch-model-proto :make-row ()
  (let ((row (send self :set-row-sums)))
    (mapcar #'(lambda (x) (length (which (= row x))))
            (1+ (iseq (1- (send self :nvar)))))))

(defmeth rasch-model-proto :make-col ()
  (let ((m (send self :nvar))
        (row (send self :set-row-sums))
        (col (send self :set-column-sums)))
    (- col (length (which (= row m))))))

(defmeth rasch-model-proto :make-eps ()
  (let* ((m (send self :nvar))
         (k (1+ (iseq (1- m)))))
  (log (/ k (- m k)))
  ))

(defmeth rasch-model-proto :make-tht ()
  (let* ((n (send self :nobs)) 
         (col (send self :set-col))
         (l (log (/ col (- n col)))))
    (- l (mean l))
    ))

(defmeth rasch-model-proto :full-ml-newton
  (&key (crit 1e-6) (crut 1e-6) (bias nil))
  (let* ((m (send self :nvar))
         (kkk (1+ (iseq (1- m))))
         (eps (repeat 0 (1- m)))
         (tht (repeat 0 m))
         (col (send self :set-col))
         (row (send self :set-row))
         (fnc 0)
         (fnd 0))
    (loop
     (send self :full-item-newton-loop eps tht row col m crit)
     (setf fnc (send self :full-ml-function eps tht row col kkk ))
     (format t "Function Value Item   Loop ~,10f~%" fnc)
     (send self :full-person-newton-loop eps tht kkk m crit)
     (setf fnc (send self :full-ml-function eps tht row col kkk ))
     (format t "Function Value Person Loop ~,10f~%" fnc)
     (if (< (abs (- fnc fnd)) crut) (return)
       (setf fnd fnc))
     )
    (if bias (progn
              (format t "Bias Correction~%")
              (setf tht (* tht (/ (1- m) m)))
              (send self :full-person-newton-loop eps tht kkk m crit)
              (setf eps (* eps (/ (- m 2) (1- m))))))
    (list eps tht)
    )
  )

(defmeth rasch-model-proto :full-ml-function (eps tht row col kkk)
    (- (sum (matmult row (log (1+ (exp (outer-product eps tht #'+))))))
       (+ (sum (* kkk row eps)) (sum (* col tht))))
    )

(defmeth rasch-model-proto :full-person-newton-loop (eps tht kkk m crit)
     (loop
      (let ((chg 0))
        (dotimes (i (1- m))
          (let* (
                 (ss (exp (+ (elt eps i) tht)))
                 (pp (/ ss (1+ ss)))
                 (vv (* pp (- 1 pp)))
                 (dd (/ (- (sum pp) (elt kkk i)) (sum vv)))
                 )
            (decf (elt eps i) dd)
            (setf chg (max chg (abs dd)))
            ))
        (if (< chg crit) (return))))
     )

(defmeth rasch-model-proto :full-item-newton-loop (eps tht row col m crit)
  (loop
      (let ((chg 0))
        (dotimes (j m)
          (let* (
                 (ss (exp (+ eps (elt tht j))))
                 (pp (/ ss (1+ ss)))
                 (vv (* row pp (- 1 pp)))
                 (dd (/ (- (sum (* row pp)) (elt col j)) (sum vv)))
                 )
            (decf (elt tht j) dd)
            (setf chg (max chg (abs dd)))
            ))
        (if (< chg crit)
            (progn
              (setf tht (- tht (mean tht)))
              (return)))))
  )

(defmeth rasch-model-proto :conditional-ml-linear
  (&key (crit 1e-6))
  (let* ((col (send self :set-col))
         (row (send self :set-row))
         (par (send self :initial-item row col))
         (fnc 0)
         (fnd 0))
    (loop
     (setf par (send self :conditional-ml-correction par row col))
     (setf par (exp (- (log par) (mean (log par)))))
     (setf fnc (send self :conditional-ml-function par row col)) 
     (format t "Function Value ~,10f~%" fnc)
     (if (< (abs (- fnc fnd)) crit) (return (log par))
       (setf fnd fnc))
     )
))

(defmeth rasch-model-proto :conditional-ml-newton
  (&key (crit 1e-6))
  (let* ((col (send self :set-col))
         (row (send self :set-row))
         (par (send self :initial-item row col))
         (nn (length col))
         (kk (1+ (iseq (1- nn))))
         (fnc 0)
         (fnd 0))
    (loop
     (let ((gg (send self :conditional-ml-gradient par row col))
           (hh (send self :conditional-ml-hessian par row col)))
       (setf par (- par (combine 0 (solve (select hh kk kk) (select gg kk))))) 
       (setf fnc (send self :conditional-ml-function par row col)) 
       (format t "Function Value ~,10f~%" fnc)
       (if (< (abs (- fnc fnd)) crit) (return (log par))
         (setf fnd fnc))
       ))
))

(defmeth rasch-model-proto :conditional-ml-function (par row col)
(let ((sym (log (butlast (symmetric-functions par)))))
  (- (sum (* row sym)) (sum (* col (log par))))))

(defmeth rasch-model-proto :conditional-ml-correction (par row col)
(let ((sym (butlast (symmetric-functions par))))
     (/ col 
        (matmult (apply #'bind-rows 
                        (symmetric-functions-gradient par))
                        (/ row sym)))
))

(defmeth rasch-model-proto :conditional-ml-gradient (par row col)
(let ((sym (butlast (symmetric-functions par))))
  (- (matmult (apply #'bind-rows 
                     (symmetric-functions-gradient par))
              (/ row sym)) (/ col par))
))

(defmeth rasch-model-proto :conditional-ml-hessian (par row col)
  (let* ((n (length par))
         (nn (iseq (length col)))
         (ff (butlast (symmetric-functions par)))
         (hh (symmetric-functions-hessian par))
         (gg (apply #'bind-rows (symmetric-functions-gradient par)))
         (aa (make-array (list n n) :initial-element (/ row ff)))
         )
    (- (+ (diagonal (/ col (* par par)))
          (map-elements #'inner-product aa hh))
          (matmult gg (matmult (diagonal (/ row (* ff ff)))
                               (transpose gg))))
    ))


(defmeth rasch-model-proto :initial-item (row col)
  (let* ((m (length col))
         (n (1+ (iseq (1- m)))))
    (exp (/ (- col (mean col))
            (sum (* row n (- m n) (/ m) (/ (1- m))))))
    ))








