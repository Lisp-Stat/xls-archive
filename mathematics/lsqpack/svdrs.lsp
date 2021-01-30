(defun svdrs (a mda mm nn b mdb nb s)
 (declare (type (simple-array double-float (* *)) a))
 (declare (type fixnum mda)) (declare (type fixnum mm))
 (declare (type fixnum nn))
 (declare (type (simple-array double-float (* *)) b))
 (declare (type fixnum mdb)) (declare (type fixnum nb))
 (declare (type (simple-array double-float (* *)) s))
 (prog
  ((np1 0) (nsp1 0) (k 0) (l 0) (t_ 0.0d0) (m 0) (ns 0) (i 0) (j 0) (n 0)
   (one 0.0d0) (zero 0.0d0)
  )
  (declare (type fixnum np1)) (declare (type fixnum nsp1))
  (declare (type fixnum k)) (declare (type fixnum l))
  (declare (type double-float t_)) (declare (type fixnum m))
  (declare (type fixnum ns)) (declare (type fixnum i))
  (declare (type fixnum j)) (declare (type fixnum n))
  (declare (type double-float one)) (declare (type double-float zero))
  (comment "     subroutine svdrs (a,mda,mm,nn,b,mdb,nb,s)")
  (comment
   "     c.l.lawson and r.j.hanson, jet propulsion laboratory, 1974 mar 1"
  )
  (comment
   "     to appear in @solving least squares problems@, prentice-hall, 1974"
  )
  (comment
   "          singular value decomposition also treating right side vector."
  )
  (comment "") (comment "     the array s occupies 3*n cells.")
  (comment "     a occupies m*n cells") (comment "     b occupies m*nb cells.")
  (comment "")
  (comment "     special singular value decomposition subroutine.")
  (comment "     we have the m x n matrix a and the system a*x=b to solve.")
  (comment "     either m .ge. n  or  m .lt. n is permitted.")
  (comment "                       the singular value decomposition")
  (comment "     a = u*s*v**(t) is made in such a way that one gets")
  (comment "        (1) the matrix v in the first n rows and columns of a.")
  (comment "        (2) the diagonal matrix of ordered singular values in")
  (comment "            the first n cells of the array s(ip), ip .ge. 3*n.")
  (comment "        (3) the matrix product u**(t)*b=g gets placed back in b.")
  (comment "        (4) the user must complete the solution and do his own")
  (comment "            singular value analysis.") (comment "     *******")
  (comment "     give special")
  (comment "     treatment to rows and columns which are entirely zero.  this")
  (comment
   "     causes certain zero sing. vals. to appear as exact zeros rather"
  )
  (comment "     than as about eta times the largest sing. val.   it similarly"
  )
  (comment "     cleans up the associated columns of u and v.")
  (comment "     method..")
  (comment "      1. exchange cols of a to pack nonzero cols to the left.")
  (comment "         set n = no. of nonzero cols.")
  (comment
   "         use locations a(1,nn),a(1,nn-1),...,a(1,n+1) to record the"
  )
  (comment "         col permutations.")
  (comment "      2. exchange rows of a to pack nonzero rows to the top.")
  (comment
   "         quit packing if find n nonzero rows.  make same row exchanges"
  )
  (comment "         in b.  set m so that all nonzero rows of the permuted a")
  (comment "         are in first m rows.  if m .le. n then all m rows are")
  (comment
   "         nonzero.  if m .gt. n then      the first n rows are known"
  )
  (comment "         to be nonzero,and rows n+1 thru m may be zero or nonzero."
  )
  (comment "      3. apply original algorithm to the m by n problem.")
  (comment "      4. move permutation record from a(,) to s(i),i=n+1,...,nn.")
  (comment "      5. build v up from  n by n  to  nn by nn  by placing ones on"
  )
  (comment
   "         the diagonal and zeros elsewhere.  this is only partly done"
  )
  (comment "         explicitly.  it is completed during step 6.")
  (comment
   "      6. exchange rows of v to compensate for col exchanges of step 2."
  )
  (comment
   "      7. place zeros in  s(i),i=n+1,nn  to represent zero sing vals."
  )
  (comment "") (setf zero 0.0) (setf one 1.0) (comment "")
  (comment
   "                             begin.. special for zero rows and cols."
  )
  (comment "")
  (comment "                             pack the nonzero cols to the left")
  (comment "") (setf n nn) (if (or (<= n 0) (<= mm 0)) (go end_label))
  (setf j n) label10
  (fdo ((i 1 (+ i 1))) ((> i mm) nil)
   (tagbody (arithmetic-if (fref a i j) (go label50) (go label20) (go label50))
    label20
  ))
  (comment "") (comment "         col j  is zero. exchange it with col n.")
  (comment "") (if (= j n) (go label40))
  (fdo ((i 1 (+ i 1))) ((> i mm) nil)
   (tagbody (fset (fref a i j) (fref a i n)))
  )
  label40 (fset (fref a 1 n) j) (setf n (+ n (- 1))) label50
  (setf j (+ j (- 1))) (if (>= j 1) (go label10))
  (comment
   "                             if n=0 then a is entirely zero and svd"
  )
  (comment "                             computation can be skipped")
  (setf ns 0) (if (= n 0) (go label240))
  (comment "                             pack nonzero rows to the top")
  (comment "                             quit packing if find n nonzero rows")
  (setf i 1) (setf m mm) label60 (if (or (> i n) (>= i m)) (go label150))
  (arithmetic-if (fref a i i) (go label90) (go label70) (go label90)) label70
  (fdo ((j 1 (+ j 1))) ((> j n) nil)
   (tagbody (arithmetic-if (fref a i j) (go label90) (go label80) (go label90))
    label80
  ))
  (go label100) label90 (setf i (+ i 1)) (go label60)
  (comment "                             row i is zero")
  (comment "                             exchange rows i and m") label100
  (if (<= nb 0) (go label115))
  (fdo ((j 1 (+ j 1))) ((> j nb) nil)
   (tagbody (setf t_ (fref b i j)) (fset (fref b i j) (fref b m j))
    (fset (fref b m j) t_)
  ))
  label115
  (fdo ((j 1 (+ j 1))) ((> j n) nil) (tagbody (fset (fref a i j) (fref a m j)))
  )
  (if (> m n) (go label140))
  (fdo ((j 1 (+ j 1))) ((> j n) nil) (tagbody (fset (fref a m j) zero)))
  label140 (comment "                             exchange is finished")
  (setf m (+ m (- 1))) (go label60) (comment "") label150
  (comment
   "                             end.. special for zero rows and columns"
  )
  (comment "                             begin.. svd algorithm")
  (comment "     method..")
  (comment "     (1)     reduce the matrix to upper bidiagonal form with")
  (comment "     householder transformations.")
  (comment "          h(n)...h(1)aq(1)...q(n-2) = (d**t,0)**t")
  (comment "     where d is upper bidiagonal.") (comment "")
  (comment
   "     (2)     apply h(n)...h(1) to b.  here h(n)...h(1)*b replaces b"
  )
  (comment "     in storage.") (comment "")
  (comment
   "     (3)     the matrix product w= q(1)...q(n-2) overwrites the first"
  )
  (comment "     n rows of a in storage.") (comment "")
  (comment
   "     (4)     an svd for d is computed.  here k rotations ri and pi are"
  )
  (comment "     computed so that")
  (comment "          rk...r1*d*p1**(t)...pk**(t) = diag(s1,...,sm)")
  (comment
   "     to working accuracy.  the si are nonnegative and nonincreasing."
  )
  (comment "     here rk...r1*b overwrites b in storage while")
  (comment "     a*p1**(t)...pk**(t)  overwrites a in storage.") (comment "")
  (comment "     (5)     it follows that,with the proper definitions,")
  (comment "     u**(t)*b overwrites b, while v overwrites the first n row and"
  )
  (comment "     columns of a.") (comment "") (setf l (min0 m n))
  (comment "             the following loop reduces a to upper bidiagonal and")
  (comment "             also applies the premultiplying transformations to b."
  )
  (comment "")
  (fdo ((j 1 (+ j 1))) ((> j l) nil)
   (tagbody (if (>= j m) (go label160))
    (multiple-value-setq
     (dummy_var j dummy_var m dummy_var dummy_var t_ dummy_var dummy_var mda
      dummy_var
     )
     (h12 1 j (+ j 1) m (fref a 1 j) 1 t_ (fref a 1 (+ j 1)) 1 mda (+ n (- j)))
    )
    (multiple-value-setq
     (dummy_var j dummy_var m dummy_var dummy_var t_ b dummy_var mdb nb)
     (h12 2 j (+ j 1) m (fref a 1 j) 1 t_ b 1 mdb nb)
    )
    label160 (if (>= j (+ n (- 1))) (go label170))
    (multiple-value-setq
     (dummy_var dummy_var dummy_var n dummy_var mda dummy_var dummy_var mda
      dummy_var dummy_var
     )
     (h12 1 (+ j 1) (+ j 2) n (fref a j 1) mda (fref s j 3) (fref a (+ j 1) 1)
      mda 1 (+ m (- j))
    ))
    label170
  ))
  (comment "")
  (comment "     copy the bidiagonal matrix into the array s() for qrbd.")
  (comment "") (if (= n 1) (go label190))
  (fdo ((j 2 (+ j 1))) ((> j n) nil)
   (tagbody (fset (fref s j 1) (fref a j j))
    (fset (fref s j 2) (fref a (+ j (- 1)) j))
  ))
  label190 (fset (fref s 1 1) (fref a 1 1)) (comment "") (setf ns n)
  (if (>= m n) (go label200)) (setf ns (+ m 1)) (fset (fref s ns 1) zero)
  (fset (fref s ns 2) (fref a m (+ m 1))) label200 (comment "")
  (comment
   "     construct the explicit n by n product matrix, w=q1*q2*...*ql*i"
  )
  (comment "     in the array a().") (comment "")
  (fdo ((k 1 (+ k 1))) ((> k n) nil)
   (tagbody (setf i (+ (+ n 1) (- k)))
    (if (> i (min0 m (+ n (- 2)))) (go label210))
    (multiple-value-setq
     (dummy_var dummy_var dummy_var n dummy_var mda dummy_var dummy_var
      dummy_var mda dummy_var
     )
     (h12 2 (+ i 1) (+ i 2) n (fref a i 1) mda (fref s i 3) (fref a 1 (+ i 1))
      1 mda (+ n (- i))
    ))
    label210
    (fdo ((j 1 (+ j 1))) ((> j n) nil) (tagbody (fset (fref a i j) zero)))
    (fset (fref a i i) one)
  ))
  (comment "") (comment "          compute the svd of the bidiagonal matrix")
  (comment "")
  (multiple-value-setq (ipass dummy_var dummy_var ns a mda n b mdb nb)
   (qrbd ipass (fref s 1 1) (fref s 1 2) ns a mda n b mdb nb)
  )
  (comment "") (case ipass (1 label240) (2)) label240
  (if (>= ns n) (go label260)) (setf nsp1 (+ ns 1))
  (fdo ((j nsp1 (+ j 1))) ((> j n) nil) (tagbody (fset (fref s j 1) zero)))
  label260 (if (= n nn) (go end_label)) (setf np1 (+ n 1))
  (comment "                                  move record of permutations")
  (comment "                                  and store zeros")
  (fdo ((j np1 (+ j 1))) ((> j nn) nil)
   (tagbody (fset (fref s j 1) (fref a 1 j))
    (fdo ((i 1 (+ i 1))) ((> i n) nil) (tagbody (fset (fref a i j) zero)))
  ))
  (comment
   "                             permute rows and set zero singular values."
  )
  (fdo ((k np1 (+ k 1))) ((> k nn) nil)
   (tagbody (setf i (fref s k 1)) (fset (fref s k 1) zero)
    (fdo ((j 1 (+ j 1))) ((> j nn) nil)
     (tagbody (fset (fref a k j) (fref a i j)) (fset (fref a i j) zero))
    )
    (fset (fref a i k) one)
  ))
  (comment
   "                             end.. special for zero rows and columns"
  )
  (go end_label)
  (format 6
   "~A~A~A~A~A~A~A~A~A~A~A~A~A~A~A~A~A~A~A~A~A~A~A~A~A~A~A~A~A~A~A~A~A~A~A~A~A~A~A~A~A~A~A~A~A~A~A~A~A"
   (list)
  )
  ----> (stop) end_label (return (values a mda mm nn b mdb nb s))
))

