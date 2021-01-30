(require "f2cl_macros")

(defun zbrac (func x1 x2 succes &key (factor 1.6) (ntry 50))
 (declare (type double-float x1)) (declare (type double-float x2))
 (declare (type t succes)) (declare (type double-float factor))
 (declare (type fixnum ntry))
 (prog ((j 0) (f2 0.0d0) (f1 0.0d0)) (declare (type fixnum j))
  (declare (type double-float f2)) (declare (type double-float f1))
  (if (= x1 x2) (error "You have to guess an initial range"))
  (setf f1 (funcall func x1)) (setf f2 (funcall func x2))
  (setf succes t)
  (fdo ((j 1 (+ j 1))) ((> j ntry) nil)
   (tagbody (if (< (* f1 f2) 0.0) (go end_label))
    (cond
     ((< (abs f1) (abs f2)) (setf x1 (+ x1 (* factor (+ x1 (- x2)))))
      (setf f1 (funcall (func x1)))
     )
     (t (setf x2 (+ x2 (* factor (+ x2 (- x1))))) (setf f2 (funcall func x2))
  ))))
  (setf succes nil) (go end_label) end_label
  (return (values func x1 x2 succes))
))

