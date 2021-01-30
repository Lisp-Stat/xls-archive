;
; Generate some data to play with.
;
; x and y are uniform (0,1). z=x*y.
; times are distributed exponentially
; with lambda = 10x + y. some times
; are simply marked as censored; censoring
; times are not generated.
;
; ENA - 3/5/91.
;
(load "exp-rand")
(load "cosort")
(provide 'gen-km-scat-data)

(defun gen-km-scat-data (num prob-cen)
  (let* ((xt (uniform-rand num))
         (yt (uniform-rand num))
         (zt (* xt yt))
         (timest (exp-rand (+ (* 10 xt) yt)))
         (statust (binomial-rand num 1 (- 1 prob-cen)))
         (data (cosort timest statust xt yt zt)))
    (setf time1 (elt data 0))
    (setf stat1 (elt data 1))
    (setf covdata (cdr (cdr data)))    
    nil))
