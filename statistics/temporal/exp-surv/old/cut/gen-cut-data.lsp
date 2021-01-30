;
; generate data to test the cut-point
; slider plot.
;
(load "gen-expo-data")

(provide 'gen-cut-data)
(defun gen-cut-data ()
  (gen-expo-data 50 50 0 1 3)
  (setf time1 (append time1 time2))
  (setf stat1 (append stat1 stat2))
  (setf covar (append (* 3 (uniform-rand 50)) (+ 3 (uniform-rand 50))))
  (setf data (cosort time1 stat1 covar))
  (setf time1 (elt data 0))
  (setf stat1 (elt data 1))
  (setf covar (elt data 2))
  nil)
