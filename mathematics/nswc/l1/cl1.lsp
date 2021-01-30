(defun cl1 (k l m n q kq kode toler iter x res error wk iwk)
 (declare (type fixnum k)) (declare (type fixnum l)) (declare (type fixnum m))
 (declare (type fixnum n)) (declare (type (simple-array double-float (* *)) q))
 (declare (type fixnum kq)) (declare (type fixnum kode))
 (declare (type double-float toler)) (declare (type fixnum iter))
 (declare (type (simple-array double-float (*)) x))
 (declare (type (simple-array double-float (*)) res))
 (declare (type double-float error))
 (declare (type (simple-array double-float (*)) wk))
 (declare (type (simple-array fixnum (*)) iwk))
 (prog ((klm 0)) (declare (type fixnum klm)) (setf klm (+ (+ k l) m))
  (multiple-value-setq
   (k l m n klm kq dummy_var dummy_var q kode toler iter x res error wk
    dummy_var dummy_var
   )
   (xl1 k l m n klm kq (+ klm n) (+ n 2) q kode toler iter x res error wk
    (fref iwk (+ klm 1)) (fref iwk 1)
  ))
  (return (values k l m n q kq kode toler iter x res error wk iwk))
))

