;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; In this file we collect a large number of more or less robust
;; non-standard scale estimators, based on the work of Hample,
;; Rousseuw, Bickel, Lehmann, and others. This work was inspired
;; mainly by Rousseeuw and Croux, JASA, 88, 1993, 1273-1283.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun median-average-deviation (x &key (b 1.4826))
"Args: (x &key (b 1.4826))
50% breakdown point, bounded influence, 37% Gaussian
efficiency."
  (* b (median (abs (- x (median x)))))
  )

(defun average-deviation (x &key (b 1.2533))
"Args: (x &key (b 1.2533))
0% Breakdown, unbounded influence."
  (* b (mean (abs (- x (mean x)))))
  )

(defun gini-mean-difference (x &key (b 1.1284))
  (let (
        (n (length x))
        )
  (/ (* b 4 (sum (* (- x (mean x)) (rank x)))) (* n (1- n)))
))

(defun gini-median-difference (x &key (b 1.1926))
"Args: (x &key (b 1.1926))
50% breakdown, bounded influence, 58% Gaussian efficiency"
  (* b (median (mapcar #'median
                       (row-list (abs (outer-product x x #'-))))))
  )

(defun overall-median-difference (x &key (b 1.0483))
"Args: (x (&key b 1.0483))
29% breakdown, 86% Gaussian efficiency."
  (* b (median (abs (outer-product x x #'-))))
 )







