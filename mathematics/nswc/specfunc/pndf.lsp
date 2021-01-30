(defun pndf (x ind) (declare (type double-float x)) (declare (type fixnum ind))
 (prog ((c 0.0d0) (pndf 0.0d0) (a 0.0d0) (t_ 0.0d0))
  (declare (type double-float c)) (declare (type double-float pndf))
  (declare (type double-float a)) (declare (type double-float t_))
  (setq a 0.70710677) (setq c 0.7978846) (setf t_ (* a x))
  (if (/= ind 0) (go label20)) (if (< x (- 8.0)) (go label10))
  (setf pndf (* 0.5 (erfc1 0 (- t_)))) (go end_label) label10
  (setf pndf (/ c (erfc1 1 (- t_)))) (go end_label) label20
  (if (> x 8.0) (go label30)) (setf pndf (* 0.5 (erfc1 0 t_))) (go end_label)
  label30 (setf pndf (/ c (erfc1 1 t_))) (go end_label) end_label (return pndf)
))

