(provide 'read-col)

(defun READ-COL (filea)
  
  (def expdata (read-data-columns filea 3))
  (def time1 (elt expdata 0))
  (def stat1 (elt expdata 1))
  (def covar (elt expdata 2))

  (setf data (cosort time1 stat1 covar))
  (setf time1 (elt data 0))
  (setf stat1 (elt data 1))
  (setf covar (elt data 2)))
