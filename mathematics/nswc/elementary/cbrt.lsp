(defun cbrt (x) (declare (type double-float x))
 (prog ((cbrt 0.0d0) (r 0.0d0)) (declare (type real cbrt))
  (declare (type double-float r))
  (arithmetic-if x (go label30) (go label10) (go label20)) label10
  (setf cbrt 0.0) (go end_label) label20 (setf r (/ (alog x) 3.0))
  (setf cbrt (exp r)) (go end_label) label30 (setf r (/ (alog (- x)) 3.0))
  (setf cbrt (- (exp r))) (go end_label) end_label (return cbrt)
))

