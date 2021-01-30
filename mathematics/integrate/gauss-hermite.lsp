;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Simple function for computing the integral between minus and plus
;; infinity of f(x)w(x), with w(x) the standard normal density.
;; Abscissas and weights taken from Abramowitz and Segun, page 924.
;;
;; Version 1.0 -- 09/01/95 -- Jan de Leeuw
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun gauss-hermite (n)
  (let ((xw (case n
              (2 (list (list 0.707106781186548)
                       (list 8.862269254528e-1)))
              (3 (list (list 0.000000000000000
                             1.224744871391589)
                       (list 1.181635900604e0
                             2.954089751509e-1)))
              (4 (list (list 0.524647623275290
                             1.650680123885785)
                       (list 8.049140900055e-1
                             8.131283544725e-2)))
              (5 (list (list 0.000000000000000
                             0.958572464613819
                             2.020182870456086)
                       (list 9.453087204829e-1
                             3.936193231522e-1
                             1.995324205905e-2)))
              (6 (list (list 0.436077411927617
                             1.335849074013697
                             2.350604973674492)
                       (list 7.246295952244e-1
                             1.570673203229e-1
                             4.530009905509e-3)))
              (7 (list (list 0.000000000000000
                             0.816287882858965
                             1.673551628767471
                             2.651961356835233)
                       (list 8.102646175568e-1
                             4.256072526101e-1
                             5.451558281913e-2
                             9.717812450995e-4)))
              (8 (list (list 0.381186990207322
                             1.157193712446780
                             1.981656756695843
                             2.930637420257244)
                       (list 6.611470125582e-1
                             2.078023258149e-1
                             1.707798300741e-2
                             1.996040722114e-4)))
              (9 (list (list 0.000000000000000
                             0.723551018752838
                             1.468553289216668
                             2.266580584531843
                             3.190993201781528)
                       (list 7.202352156061e-1
                             4.326515590026e-1
                             8.847452739438e-2
                             4.943624275537e-3
                             3.960697726326e-5)))
             (10 (list (list 0.342901327223705
                             1.036610829789514
                             1.756683649299882
                             2.532731647232790
                             3.436159118837738)
                       (list 6.108626337353e-1
                             2.401386110823e-1
                             3.387439445548e-2
                             1.343645746781e-3
                             7.640432855233e-6)))
              )))
    (let ((fxw (first xw))
          (sxw (second xw)))
      (if (evenp n) 
          (setf fxw (append (- (reverse fxw)) fxw)
                sxw (append (reverse sxw) sxw))
          (setf fxw (append (- (reverse (rest fxw))) fxw)
                sxw (append (reverse (rest sxw)) sxw))
          )
      (list (* fxw (sqrt 2))
            (/ sxw (sqrt pi)))
      )
    )
  )

(defun gauss-hermite-integration (func n)
"Args: (func x)
Compute the integral of FUNC over (-\infty,+\infty)
with weights equal to the standard normal, using
Gauss-Hermite integration with n quadrature points."
  (let ((xw (gauss-hermite n)))
    (sum (* (second xw)
            (mapcar #'(lambda (x) (funcall func x)) (first xw))))
    )
  )