(provide 'read-col)
(require 'cosort)
;
;READ DATA FROM COLUMNS AND SORT APPROPRIATELY
;
;
(defun READ-COL (filena numcov)
 (let ((expdata (read-data-columns filena (+ 2 numcov))))
    (def time1 (elt expdata 0))
    (def stat1 (elt expdata 1))
    (def covdata NIL)
    (dolist (mm (cdr (cdr expdata)))
            (setf covdata (cons (second (cosort time1 mm)) covdata)))
    (setf covdata (reverse covdata))
    (setf data (cosort time1 stat1))
    (setf time1 (elt data 0))
    (setf stat1 (elt data 1))))


 
