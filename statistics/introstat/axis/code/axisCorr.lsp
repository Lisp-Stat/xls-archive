;;;;
;;;;     AxisCorr.lsp © Robert A. Stine, 1992
;;;;                 Correlation object.
;;;;
;;;;     7 Feb 94 ... Add matrix form.
;;;;     3 Jul 92 ... Add the data and popup menus.
;;;;     2 Jul 92 ... Created
;;;;
;;;; Additions to
;;;; XLISP-STAT 2.0 Copyright (c) 1988, by Luke Tierney
;;;; Xlisp 2.0 Copyright (c) 1985, 1987 by David Michael Betz
;;;;    All Rights Reserved
;;;;    Permission is granted for unrestricted non-commercial use

(provide "axisCorr")

;;;;

(require "axisUtil")
(require "axisBoot")

#||

(setf cc (correlate (list (iseq 10) (iseq 10) (uniform-rand 10))
                    (list "x" "y" "Z")))
(send cc :calc-value)
(send cc :value)
(send cc :se)

(send cc :print-summary)

(setf bf (send cc :bootstrap-function))
(funcall bf (sample (iseq 10) 10 t))
(send cc :bootstrap )

||#
;;;;
;;;;     CORRELATION OBJECT THAT DOES THE WORK
;;;;


(defun CORRELATE (x labels)
  (if (and (< 1 (length x)) (= (length x) (length labels)))
      (send corr-proto :new x labels)
      (message-dialog "Cannot do correlation of 1.")
      ))
  

(defproto CORR-PROTO 
  '(x 
    labels
    value
      ))

(defmeth corr-proto :ISNEW (x labels)
  (setf (slot-value 'x) x)
  (setf (slot-value 'labels) labels)
  (send self :calc-value)
  )

(defmeth corr-proto :VALUE ()
  (let ((v (if (slot-value 'value)
               (slot-value 'value)
               (send self :calc-value)))   )
    (if (= 2 (length (slot-value 'x)))
        (aref v 0 1)
        v
        )))
  
(defmeth corr-proto :CALC-VALUE ()
  (let* ((cm (apply #'covariance-matrix (slot-value 'x)))
         (d  (sqrt (diagonal cm)))  )
    (setf (slot-value 'value)
          (/ cm (outer-product d d #'* )))
    ))
        

(defmeth corr-proto :N ()
  (length (first (slot-value 'x)))  )

(defmeth corr-proto :SE ()
  (if (= 2 (length (slot-value 'x)))
      (let ((r (send self :value))
            (n (send self :n))  )
        (/ (- 1 (* r r)) (sqrt (- n 3)))  )
      (message-dialog "SE only runs with bivariate correlation.")
      ))


(defmeth corr-proto :CI (cc)
  (let* ((r   (send self :value))
         (n   (send self :n))
         (phi (* .5 (log (/ (+ 1 r) (- 1 r)))))
         (z   (normal-quant (- 1 (* .5 (- 1 cc)))))
         (wid (* z (/ 1 (sqrt (- n 3))))) 
         (int (list (- phi wid) (+ phi wid)))   )
    (format t "~a% interval for correlation -> ~a~%"
            (* 100 cc)
            (mapcar #'(lambda (p) (let ((e+p  (exp p))
                                        (e-p  (exp (- p)))  )
                                    (/ (- e+p e-p) (+ e+p e-p))))
                    int))))


(defmeth corr-proto :PRINT-SUMMARY ()
  (if (= 2 (length (slot-value 'x)))
      (progn (format t "corr~a = ~a.~%"
                     (slot-value 'labels) (send self :value))
             (send self :ci .9))
      (let ((c (* .001 (round (* 1000 (send self :value)))))  )
        (print-matrix 
         (bind-columns (cons " " (slot-value 'labels))
                       (bind-rows (slot-value 'labels) c)
                       ))
        )))
  

(defmeth corr-proto :BOOTSTRAP-FUNCTION ()
  ; Returns function that given input indices computes corr for
  ; those observations.  Furnction returns a list of the corrs.
  (let* ((x   (transpose (slot-value 'x)))
         (dim (iseq (length (first x))))
         (tri (which (coerce (outer-product dim dim #'<) 'vector)))  )
    #'(lambda (index)    ; given index, calc corr as a list
        (let* ((dim (length x))  ; number of vars
               (cm* (apply #'covariance-matrix (transpose (select x index))))
               (d*  (sqrt (diagonal cm*)))    )
          (coerce
           (select (coerce (/ cm* (outer-product d* d*)) 'vector)
                   tri)     ; upper triangular selection
           'list)  ; have to do vector first, or need to combine...
          ))))


(defmeth corr-proto :BOOTSTRAP (b)
  (format t "Bootstrapping correlation...~%")
  (let ((n (length (first (slot-value 'x))))   )
    (bootstrap "Correlation"
               (list (send self :bootstrap-function))
                `(resample (iseq ,n) ,n)
               (send *active-icon-window* :dataset)
               b  :labels (list 'Corr)
               )
    ))