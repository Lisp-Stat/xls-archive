;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Code to do one-sample and two-sample Kolmogorov-Smirnov test (with
;; no estimated parameters).
;;
;; I did not check if this is the best method to sum the series, but
;; if I compare with existing tables, it is seems good enough.
;;
;; Version 1.0 -- 06-18-95 -- Jan de Leeuw
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun ks-function (u &key (eps 1e-6))
"Args: (u &key (eps 1e-6))
Computes Kolmogorov-Smirnov distribution at U,
with precision EPS."
  (let ((s 0)
        (i 1))
    (loop
     (let ((term (exp (* -2 (^ (* i u) 2)))))
       (if (< term eps) (return (- 1 (* 2 s))))
       (incf s (if (evenp i) (- term) term))
       (incf i))
     )
    )
)

(defun ks-one-sample (data cdf &key (eps 1e-6) (graphics nil))
"Args: (data cdf &key (eps 1e-6))
Kolmogorov-Smirnov one-sample test of sequence DATA versus
cumulative distribution function CDF."
  (let ((n (length data))
        (x (remove-duplicates data))
        (dev 0)
        (prb 0)
        (pp (if graphics (plot-points nil nil) nil)))
    (if graphics 
        (progn
          (send pp :add-edf data)
          (send pp :add-function cdf (min data) (max data)))
      )
    (dolist (s x)
      (let ((a (funcall cdf s))
            (p-low (/ (length (which (< data s))) n))
            (p-up (/ (length (which (<= data s))) n)))
        (setf dev (max (abs (- p-up a)) (abs (- p-low a)) dev))
        )
      )
    (setf prb (- 1 (ks-function (* dev (sqrt n)) :eps eps)))
    (format t "Deviation ~12,10f Probability ~12,10f Significance ~a~%"
            dev prb (cond ((< prb .001) "***")
                          ((< prb .01)  "**")
                          ((< prb .05)  "*")
                          (t "n.s.")))
    )
  )

(defun ks-two-sample (data1 data2 &key (eps 1e-6) (graphics nil))
 "Args: (data1 data2 &key (eps 1e-6))
Kolmogorov-Smirnov two-sample test of sequence DATA1 versus
sequence DATA2."
 (let ((n (length data1))
        (m (length data2))
        (x (union data1 data2))
        (dev 0)
        (prb 0)
        (pp (if graphics (plot-points nil nil))))
   (if graphics (progn
                  (send pp :add-edf data1)
                  (send pp :add-edf data2 :type 'dashed)))
    (dolist (s x)
      (let ((p-low (/ (length (which (< data1 s))) n))
            (p-up (/ (length (which (<= data1 s))) n))
            (q-low (/ (length (which (< data2 s))) m))
            (q-up (/ (length (which (<= data2 s))) m)))
        (setf dev (max dev (abs (- p-up q-up))
                           (abs (- p-up q-low))
                           (abs (- p-low q-up))
                           (abs (- p-low q-low))))
        )
      )
    (setf prb (- 1 (ks-function (* dev (sqrt (/ (* m n) (+ m n)))) :eps eps)))
    (format t "Deviation ~12,10f Probability ~12,10f Significance ~a~%"
            dev prb (cond ((< prb .001) "***")
                          ((< prb .01)  "**")
                          ((< prb .05)  "*")
                          (t "n.s.")))
    )
  )


(defun maximum-spacing (data)
  (max (difference (sort-data data))))

(defmeth scatterplot-proto :add-edf (data &key (type 'normal))
  (let* ((n (length data))
         (ma (max data))
         (mi (min data))
         (dif (maximum-spacing data))
         (srt (remove-duplicates (sort-data data)))
         (ksg (1+ (length srt)))
         (frq (mapcar #'(lambda (x)
                          (/ (length (which (< data x))) n)) srt)))
    (dotimes (k ksg)
      (let ((x0 (if (= k 0) (- mi dif) (elt srt (1- k)))) 
            (x1 (if (= k (1- ksg)) (+ ma dif) (elt srt k)))
            (yy (if (= k (1- ksg)) 1 (elt frq k))))
        (send self :add-lines 
              (list x0 x1) (list yy yy))
        (if (> k 0)
            (send self :add-points (list x0) (list yy) :type type))
        )
      (send self :adjust-to-data)
      )
    )
  )