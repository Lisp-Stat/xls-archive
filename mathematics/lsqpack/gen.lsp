(defun gen (anoise) (declare (type double-float anoise))
 (prog ((ai 0.0d0) (gen 0.0d0) (aj 0.0d0) (j 0) (i 0) (mj 0) (mi 0))
  (declare (type double-float ai)) (declare (type double-float gen))
  (declare (type double-float aj)) (declare (type fixnum j))
  (declare (type fixnum i)) (declare (type fixnum mj))
  (declare (type fixnum mi))
  (comment
   "     c.l.lawson and r.j.hanson, jet propulsion laboratory, 1972 dec 15"
  )
  (comment
   "     to appear in @solving least squares problems@, prentice-hall, 1974"
  )
  (comment "          generate numbers for construction of test cases.")
  (arithmetic-if anoise (go label10) (go label30) (go label20)) label10
  (setf mi 891) (setf mj 457) (setf i 5) (setf j 7) (setf aj 0.0)
  (setf gen 0.0) (go end_label) (comment "")
  (comment "     the sequence of values of j  is bounded between 1 and 996")
  (comment "     if initial j = 1,2,3,4,5,6,7,8, or 9, the period is 332")
  label20 (setf j (* j mj)) (setf j (+ j (* (* -1 997) (/ j 997))))
  (setf aj (+ j (- 498)))
  (comment "     the sequence of values of i  is bounded between 1 and 999")
  (comment "     if initial i = 1,2,3,6,7, or 9,  the period will be 50")
  (comment "     if initial i = 4 or 8   the period will be 25")
  (comment "     if initial i = 5        the period will be 10") label30
  (setf i (* i mi)) (setf i (+ i (* (* -1 1000) (/ i 1000))))
  (setf ai (+ i (- 500))) (setf gen (+ ai (* aj anoise))) (go end_label)
  end_label (return gen)
))

