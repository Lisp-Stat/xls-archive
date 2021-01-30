(defun tester (nord ntim)
(time  (dotimes (i ntim)
       (setf x (make-array (list nord nord)
          :displaced-to (coerce (uniform-rand (* nord nord)) 'vector)))
       (setf e (eigen (+ x (transpose x))))
       )
))

