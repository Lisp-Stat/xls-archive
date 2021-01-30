(defun acondf (x y c fun) (declare (type double-float x))
 (declare (type double-float y)) (declare (type double-float c))
 (prog ((a 0.0d0) (acondf 0.0d0)) (declare (type double-float a))
  (declare (type double-float acondf))
  (multiple-value-setq (x dummy_var a b) (fun x 0.0 a b))
  (setf acondf (/ a (+ (+ x c) 1.0))) (return acondf)
))

