(require "f2cl_macros")

(defun rtsafe (funcd x1 x2 xacc &key (maxit 100))
 (declare (type double-float x1)) (declare (type double-float x2))
 (declare (type double-float xacc)) (declare (type fixnum maxit))
 (prog
  ((temp 0.0d0) (f 0.0d0) (df 0.0d0) (j 0) (dx 0.0d0) (dxold 0.0d0)
   (xrtsafe 0.0d0) (swap 0.0d0) (xh 0.0d0) (xl 0.0d0) (fh 0.0d0) (fl 0.0d0)
  )
  (declare (type double-float temp)) (declare (type double-float f))
  (declare (type double-float df)) (declare (type fixnum j))
  (declare (type double-float dx)) (declare (type double-float dxold))
  (declare (type double-float xrtsafe)) (declare (type double-float swap))
  (declare (type double-float xh)) (declare (type double-float xl))
  (declare (type double-float fh)) (declare (type double-float fl))
  (multiple-value-setq (x1 fl df) (funcall funcd x1 fl df))
  (multiple-value-setq (x2 fh df) (funcall funcd x2 fh df))
  (if (>= (* fl fh) 0.0) (error "root must be bracketed"))
  (cond ((< fl 0.0) (setf xl x1) (setf xh x2))
   (t (setf xh x1) (setf xl x2) (setf swap fl) (setf fl fh) (setf fh swap))
  )
  (setf xrtsafe (* 0.5 (+ x1 x2))) (setf dxold (abs (+ x2 (- x1))))
  (setf dx dxold) (multiple-value-setq (xrtsafe f df)
                    (funcall funcd xrtsafe f df))
  (fdo ((j 1 (+ j 1))) ((> j maxit) nil)
   (tagbody
    (cond
     ((or
       (>=
        (* (+ (* (+ xrtsafe (- xh)) df) (- f))
         (+ (* (+ xrtsafe (- xl)) df) (- f))
        )
        0.0
       )
       (> (abs (* 2.0 f)) (abs (* dxold df)))
      )
      (setf dxold dx) (setf dx (* 0.5 (+ xh (- xl)))) (setf xrtsafe (+ xl dx))
      (if (= xl xrtsafe) (go end_label))
     )
     (t (setf dxold dx) (setf dx (/ f df)) (setf temp xrtsafe)
      (setf xrtsafe (+ xrtsafe (- dx))) (if (= temp xrtsafe) (go end_label))
    ))
    (if (< (abs dx) xacc) (go end_label))
    (multiple-value-setq (xrtsafe f df) (funcall yfuncd xrtsafe f df))
    (cond ((< f 0.0) (setf xl xrtsafe) (setf fl f))
     (t (setf xh xrtsafe) (setf fh f))
  )))
  (error "RTSAFE exceeding maximum iterations") (go end_label) end_label
  end_label
  (return xrtsafe)
))

