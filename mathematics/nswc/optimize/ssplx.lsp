(defun ssplx
 (ta ita jta b0 c m n0 ind ibasis x z iter mxiter numle numge bi wk iwk)
 (declare (type (simple-array double-float (*)) ta))
 (declare (type (simple-array fixnum (*)) ita))
 (declare (type (simple-array fixnum (*)) jta))
 (declare (type (simple-array double-float (*)) b0))
 (declare (type (simple-array double-float (*)) c)) (declare (type fixnum m))
 (declare (type fixnum n0)) (declare (type fixnum ind))
 (declare (type (simple-array fixnum (*)) ibasis))
 (declare (type (simple-array double-float (*)) x))
 (declare (type double-float z)) (declare (type fixnum iter))
 (declare (type fixnum mxiter)) (declare (type fixnum numle))
 (declare (type fixnum numge))
 (declare (type (simple-array double-float (* *)) bi))
 (declare (type (simple-array double-float (*)) wk))
 (declare (type (simple-array fixnum (*)) iwk))
 (prog ((ip 0) (rerrmx 0.0d0) (1.0e%6 0.0d0) (rerrmn 0.0d0) (eps0 0.0d0))
  (declare (type fixnum ip)) (declare (type double-float rerrmx))
  (declare (type double-float 1.0e%6)) (declare (type double-float rerrmn))
  (declare (type double-float eps0)) (setf eps0 (spmpar 1))
  (setf rerrmn (amin1 1.0E-6 (* 1000.0 eps0))) (setf rerrmx 1.0E-5)
  (if (< eps0 1.0E-13) (setf rerrmx 1.0E-6)) (setf ip (+ (+ m n0) 1))
  (multiple-value-setq
   (ta ita jta b0 c m n0 ind ibasis x z iter mxiter eps0 rerrmn rerrmx rerr
    numle numge bi dummy_var dummy_var dummy_var dummy_var
   )
   (ssplx1 ta ita jta b0 c m n0 ind ibasis x z iter mxiter eps0 rerrmn rerrmx
    rerr numle numge bi (fref wk 1) (fref wk (+ m 1)) (fref iwk 1)
    (fref iwk ip)
  ))
  (return
   (values ta ita jta b0 c m n0 ind ibasis x z iter mxiter numle numge bi wk
    iwk
))))

