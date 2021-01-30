(require "f2cl_macros")

(defun rtsec (func x1 x2 xacc &key (maxit 30)) (declare (type double-float x1))
 (declare (type double-float x2)) (declare (type double-float xacc))
 (declare (type fixnum maxit))
 (prog
  ((dx 0.0d0) (j 0) (swap 0.0d0) (xl 0.0d0) (xrtsec 0.0d0) (f 0.0d0) (fl 0.0d0))
  (declare (type double-float dx)) (declare (type fixnum j))
  (declare (type double-float swap)) (declare (type double-float xl))
  (declare (type double-float xrtsec)) (declare (type double-float f))
  (declare (type double-float fl)) (setf fl (funcall func x1))
  (setf f (funcall func x2))
  (cond
   ((< (abs fl) (abs f)) (setf xrtsec x1) (setf xl x2) (setf swap fl)
    (setf fl f) (setf f swap)
   )
   (t (setf xl x1) (setf xrtsec x2))
  )
  (fdo ((j 1 (+ j 1))) ((> j maxit) nil)
   (tagbody (setf dx (/ (* (+ xl (- xrtsec)) f) (+ f (- fl)))) (setf xl xrtsec)
    (setf fl f) (setf xrtsec (+ xrtsec dx)) (setf f (funcall func xrtsec))
    (if (or (< (abs dx) xacc) (= f 0.0)) (go end_label))
  ))
  end_label
  (return xrtsec)
))

