(load "super-smoother")
(defun gam-fit (x y &optional (w nil) (num 10))
 (let* (
        (n (length x))
        (loopseq (iseq n))
        (x (cond ((matrixp x) (mapcar #'(lambda (x) (coerce x 'list))
                                (column-list x)))
                 ((vectorp x) (mapcar #'(lambda (x) (coerce x 'list)) (list x)))
                 ((and (consp x) (numberp (car x))) (list x))
                 (t x)))
        (smooth-list (repeat nil n))
       )
   (flet (
          (first-smooth (x y) 
             (let ((reg (regression-model x y :weights w :print nil)))
                        (send reg :fit-values)))
          (smoother-fun (s-list xi y)
              (let* ((y_s (- y (apply #'+ s-list)))
                     (smooth (send supersmoother-proto 
                                     :new xi y_s w nil nil)))
                (send smooth :smoothed-y)))
         )
      (setf smooth-list (mapcar #'(lambda (z) (first-smooth z y)) x))
      (dotimes (j num smooth-list)
        (dotimes (i n)
          (setf (elt smooth-list i) 
                (smoother-fun (select smooth-list (remove i loopseq))
                              (elt x i) y)))))))
 




