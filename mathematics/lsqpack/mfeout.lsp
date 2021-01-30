(defun mfeout (a mda m n names mode)
 (declare (type (simple-array double-float (* *)) a))
 (declare (type fixnum mda)) (declare (type fixnum m))
 (declare (type fixnum n)) (declare (type (simple-array t (1)) names))
 (declare (type fixnum mode))
 (prog
  ((noblk nil) (iblan (make-array '(8) :element-type 'string-char))
   (name (make-array '(8) :element-type 'string-char))
   (ihead (make-array '(2) :element-type '(simple-array string-char (4))))
   (i 0) (j2 0) (j1 0) (ncol 0) (last 0) (nbloc 0)
  )
  (declare (type t noblk))
  (declare (type (simple-array string-char (8)) iblan))
  (declare (type (simple-array string-char (8)) name))
  (declare (type (simple-array t (1)) ihead)) (declare (type fixnum i))
  (declare (type fixnum j2)) (declare (type fixnum j1))
  (declare (type fixnum ncol)) (declare (type fixnum last))
  (declare (type fixnum nbloc))
  (comment
   "     c.l.lawson and r.j.hanson, jet propulsion laboratory, 1973 jun 12"
  )
  (comment
   "     to appear in @solving least squares problems@, prentice-hall, 1974"
  )
  (comment "          subroutine for matrix output with labeling.")
  (comment "") (comment "     a( )         matrix to be output")
  (comment "                  mda     first dimension of a array")
  (comment "                  m         no. of rows in a matrix")
  (comment "                  n         no. of cols in a matrix")
  (comment "     names()      array of names.  if names(1) = 1h , the rest")
  (comment "                  of the names() array will be ignored.")
  (comment "     mode         =1   for   4p8f15.0  format  for v matrix.")
  (comment
   "                  =2   for   8e15.8  format  for candidate solutions."
  )
  (comment "") (setq iblan "        ") (setq maxco 8)
  (replace ihead '(" COL" "SOLN") :end 1) (comment "")
  (setf noblk (/= (fref names 1) iblan))
  (if (or (<= m 0) (<= n 0)) (go end_label)) (comment "")
  (if (= mode 2) (go label10))
  (format 1
   "~A~A~A~A~A~A~A~A~A~A~A~A~A~A~A~A~A~A~A~A~A~A~A~A~A~A~A~A~A~A~A~A~A~A~A~A~A~A~A~A~A~A~A~A~A~A~%~A~A~A~A~A~A~A~A~A~A~A~A~A~A~A~A~A~A~A~A~A~A~A~A~A~A~A~A~A~A~A~A~A~A~A~A~A~A~A~A~A~A~A~A~A~A~A"
   (list)
  )
  (go label20) label10 (format 1 "~NIL,NIL,2,0,,'*E~1@T" (list)) label20
  (comment "") (setf nbloc (/ n maxco))
  (setf last (+ n (* (* -1 nbloc) maxco))) (setf ncol maxco) (setf j1 1)
  (comment "") (comment "                            main loop starts here")
  (comment "") label30 (if (> nbloc 0) (go label40))
  (if (<= last 0) (go end_label)) (setf ncol last) (setf last 0) (comment "")
  label40 (setf j2 (+ (+ j1 ncol) (- 1)))
  (format 1 "~A~11@T~8{~A~A~4D~A~}~%~A"
   (do ((j j1 (+ j 1)) (ret nil (append ret (list (fref ihead mode) j))))
    ((> j j2) ret)
  ))
  (comment "")
  (fdo ((i 1 (+ i 1))) ((> i m) nil)
   (tagbody (setf name iblan) (if noblk (setf name (fref names i)))
    (comment "") (if (= mode 2) (go label50))
    (format 1 "~A~3D~A~A~A~14,0,1,,'*F" i name
     (do ((j j1 (+ j 1)) (ret nil (append ret (list (fref a i j)))))
      ((> j j2) ret)
    ))
    (return) label50
    (format 1 "~A~3D~A~A~A~14,7,2,0,,'*E" i name
     (do ((j j1 (+ j 1)) (ret nil (append ret (list (fref a i j)))))
      ((> j j2) ret)
  ))))
  (comment "") (setf j1 (+ j1 maxco)) (setf nbloc (+ nbloc (- 1))) (go label30)
  (comment "") end_label (return (values a mda m n names mode))
))

