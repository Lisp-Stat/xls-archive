(provide 'cosort)

; ENA 3/91

(defun COSORT (x &rest others)
  (let ((xorder (order x)))
    (mapcar #'(lambda (z) (select z xorder))
            (cons x others))))
