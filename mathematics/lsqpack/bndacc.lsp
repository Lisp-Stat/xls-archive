(defun bndacc (g mdg nb ip ir mt jt)
 (declare (type (simple-array double-float (* *)) g))
 (declare (type fixnum mdg)) (declare (type fixnum nb))
 (declare (type fixnum ip)) (declare (type fixnum ir))
 (declare (type fixnum mt)) (declare (type fixnum jt))
 (prog
  ((kh 0) (mh 0) (jg 0) (lp1 0) (k 0) (l 0) (mu 0) (ig 0) (ie 0) (j 0) (ig2 0)
   (ig1 0) (i 0) (nbp1 0) (zero 0.0d0)
  )
  (declare (type fixnum kh)) (declare (type fixnum mh))
  (declare (type fixnum jg)) (declare (type fixnum lp1))
  (declare (type fixnum k)) (declare (type fixnum l))
  (declare (type fixnum mu)) (declare (type fixnum ig))
  (declare (type fixnum ie)) (declare (type fixnum j))
  (declare (type fixnum ig2)) (declare (type fixnum ig1))
  (declare (type fixnum i)) (declare (type fixnum nbp1))
  (declare (type double-float zero))
  (comment
   "     c.l.lawson and r.j.hanson, jet propulsion laboratory, 1973 jun 12"
  )
  (comment
   "     to appear in @solving least squares problems@, prentice-hall, 1974"
  )
  (comment "          sequential algorithm for banded least squares problem..")
  (comment "          accumulation phase.      for solution phase use bndsol.")
  (comment "")
  (comment
   "     the calling program must set ir=1 and ip=1 before the first call"
  )
  (comment "     to bndacc for a new case.") (comment "")
  (comment "     the second subscript of g( ) must be dimensioned at least")
  (comment "     nb+1 in the calling program.") (setf zero 0.0) (comment "")
  (comment
   "              alg. steps 1-4 are performed external to this subroutine."
  )
  (comment "") (setf nbp1 (+ nb 1)) (if (<= mt 0) (go end_label))
  (comment "                                             alg. step 5")
  (if (= jt ip) (go label70))
  (comment "                                             alg. steps 6-7")
  (if (<= jt ir) (go label30))
  (comment "                                             alg. steps 8-9")
  (fdo ((i 1 (+ i 1))) ((> i mt) nil)
   (tagbody (setf ig1 (+ (+ jt mt) (- i))) (setf ig2 (+ (+ ir mt) (- i)))
    (fdo ((j 1 (+ j 1))) ((> j nbp1) nil)
     (tagbody (fset (fref g ig1 j) (fref g ig2 j)))
  )))
  (comment "                                             alg. step 10")
  (setf ie (+ jt (- ir)))
  (fdo ((i 1 (+ i 1))) ((> i ie) nil)
   (tagbody (setf ig (+ (+ ir i) (- 1)))
    (fdo ((j 1 (+ j 1))) ((> j nbp1) nil) (tagbody (fset (fref g ig j) zero)))
  ))
  (comment "                                             alg. step 11")
  (setf ir jt)
  (comment "                                             alg. step 12") label30
  (setf mu (min0 (+ nb (- 1)) (+ (+ ir (- ip)) (- 1))))
  (if (= mu 0) (go label60))
  (comment "                                             alg. step 13")
  (fdo ((l 1 (+ l 1))) ((> l mu) nil)
   (tagbody
    (comment "                                             alg. step 14")
    (setf k (min0 l (+ jt (- ip))))
    (comment "                                             alg. step 15")
    (setf lp1 (+ l 1)) (setf ig (+ ip l))
    (fdo ((i lp1 (+ i 1))) ((> i nb) nil)
     (tagbody (setf jg (+ i (- k))) (fset (fref g ig jg) (fref g ig i)))
    )
    (comment "                                             alg. step 16")
    (fdo ((i 1 (+ i 1))) ((> i k) nil)
     (tagbody (setf jg (+ nbp1 (- i))) (fset (fref g ig jg) zero))
  )))
  (comment "                                             alg. step 17") label60
  (setf ip jt)
  (comment "                                             alg. steps 18-19")
  label70 (setf mh (+ (+ ir mt) (- ip))) (setf kh (min0 nbp1 mh))
  (comment "                                             alg. step 20")
  (fdo ((i 1 (+ i 1))) ((> i kh) nil)
   (tagbody
    (multiple-value-setq
     (dummy_var i dummy_var mh dummy_var dummy_var rho dummy_var dummy_var mdg
      dummy_var
     )
     (h12 1 i (max0 (+ i 1) (+ (+ ir (- ip)) 1)) mh (fref g ip i) 1 rho
      (fref g ip (+ i 1)) 1 mdg (+ nbp1 (- i))
  ))))
  (comment "                                             alg. step 21")
  (setf ir (+ ip kh))
  (comment "                                             alg. step 22")
  (if (< kh nbp1) (go label100))
  (comment "                                             alg. step 23")
  (fdo ((i 1 (+ i 1))) ((> i nb) nil)
   (tagbody (fset (fref g (+ ir (- 1)) i) zero))
  )
  (comment "                                             alg. step 24")
  label100
  (comment "                                             alg. step 25")
  (go end_label) end_label (return (values g mdg nb ip ir mt jt))
))

