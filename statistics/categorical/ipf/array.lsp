#| 
Product of a lazy weekend. 
Version 0.2 -- June 12, 1994. 
I really should have been doing something useful instead. 
|# 


#| 
The four basic APL operators (see, for example,
Garry Helzel, An Encyclopedia of APL, 2e edition, 1989,
I-APL, 6611 Linville Drive, Weed., CA) are inner-product,
outer-product, reduce, and scan. They can be used to
produce new binary and unary functions from existing
ones. We give Xlisp-Stat implementations below, together
with some helpers.
|#

#| 
This is for former APL addicts such as myself. Although
Xlisp-Stat has some of the basic multidimensional array
operators, is is not as complete as APL (and certainly not
as complete as APL-2 or J). Here are the basic slicing,
subscripting, compression, and expansion operators for
Xlisp-Stat. They make it much easier to write programs
for multiway scaling, for analysis of multidimensional
contingency tables, and so on. I guess it brings us closer
to MatLab, Mathematica, and the new S. With these
operators (and the existing ones, such as array-permute)
iterative proportional fitting for general log-linear
models is a breeze. 
|#

#| 
(Much) later in the day. Added pretty-printer for
arrays. Made apl-outer-product work for an arbitrary
number of arrays, and for arbitrary vector-reducing
functions. This generalizes APL's outer-product o.f, with f binary.
Defined generalized inner product (i.e.
APL's inner product f.g), with g an arbitrary binary operator
and f an vector-reducing function. Rewrote apl-reduce
to be more efficient, and use an arbitrary vector-reducing
function. This makes it equivalent to APL's reduction f/.
|#

#|
And on the Sunday: added the function array-blow-up, which
blows up an array by outer-multiplying it with an array
of constants. This is used in the IPF rotuines in loglin.lsp.
Also build in some safe-guards and made some editorial
changes.
|#

;;;;;;;;;;;;;;;;;; code starts here ;;;;;;;;;;;;;;;;;;;;;;;

(defun apl-reduction (x l &optional (f #'sum))
"Args: array list
Computes reductions for dimensions l from the array
x along remaining dimensions, using general vector-reducing
functions. Thus if x is a 10 x 3 x 7 array, then 
(array-marginal x '(0 2)) returns a a 10 x 7 matrix,
and (array-marginal x '(1)) returns a vector with 3 elements.
Same for (array-marginal x '(1) #'median)"
(let* (
       (rx (array-rank x))
       (mx (array-dimensions x))
       (mz (select mx l))
       (zz (make-array mz))
       (nz (array-total-size zz))
       )
  (dotimes (i nz zz)
           (let* (
                  (nn (repeat nil rx))
                  (kk (array-subscript zz i))
                  )
             (setf (select nn l) kk)
             (setf (row-major-aref zz i) (funcall f (array-slice x nn)))
             ))
))

(defun apl-scan (x k &optional (f #'+))
"Args: array index
Computes cumulative scans of dimension k from array x
along remaining dimensions. Thus the result has the same
dimensions as x, but successive slices are summed."
(let* (
       (rx (array-rank x))
       (mx (array-dimensions x))
       (mk (elt mx k))
       (nx (array-total-size x))
       (zz (make-array mx))
       )
  (dotimes (i nx zz)
           (let* (
                  (si (array-subscript zz i))
                  (mi (mapcar #'list si))
                  (ti (elt si k))
                 )
             (setf (elt si k) nil)
             (setf (elt mi k) (iseq mk))
             (setf (row-major-aref zz i) 
                   (elt (accumulate f (array-slice x si)) ti))
            ))
))

(defun apl-outer-product (x &optional (f #'*))
"Args: list-of-arrays &optional function 
If X_1,...,X_m are arrays of array-dimension nx_1,...,nx_m, then we
return an array of dimension nx_i x ... x nx_m, with
elements (f x_1 ... x_m)."
(let* (
       (d (mapcar #'array-dimensions x))
       (r (mapcar #'array-rank x))
       (s (map-elements #'+ (mapcar #'iseq r) 
              (select (cumsum (adjoin 0 r)) (iseq (length r)))))
       (z (make-array (apply #'concatenate (adjoin 'list d))))
       (n (array-total-size z))
       )
(dotimes (i n z)
(let* (
      (si (array-subscript z i))
      (ti (mapcar #'(lambda (x) (select si x)) s))
      (ui (map-elements #'(lambda (a b) (apply #'select (adjoin a b))) x ti))
      )
(setf (row-major-aref z i) (apply f ui))
))))

(defun apl-inner-product (x y &optional (f #'*) (g #'+))
"Args: array array &optional binary-function reducing-function
The arrays have to be conforming, i.e. the last dimension
of the first one must be equal to the first dimension of the second
one. Then an n_1 x ... x n_a x t array and a t x m_1 x ... x m_b
array produce a n_1 x ... x n_a x m_1 x ... x m_b array, reduced
along the common dimension by (g (f x y))."
(let* (
       (mx (array-dimensions x))
       (my (array-dimensions y))
       (rx (array-rank x))
       (ry (array-rank y))
       (z1 (reverse (rest (reverse mx))))
       (z2 (rest my))
       (mz (combine z1 z2))
       (zz (make-array mz))
       (nz (array-total-size zz))
       )
  (if (/= (first (reverse mx)) (first my)) 
      (error "Non-conforming arrays in apl-inner-product"))
  (dotimes (i nz zz)
           (let* (
                  (si (array-subscript zz i))
                  (xi (combine (select si (iseq (1- rx))) nil))
                  (yi (combine nil (select si (+ (1- rx) (iseq (1- ry))))))
                  (xx (coerce (array-slice x xi) 'list))
                  (yy (coerce (array-slice y yi) 'list))
                  )
             (setf (row-major-aref zz i) (apply g (funcall f xx yy)))
             ))
))

#| 
Some important auxilary functions 
|#

(defun array-slice (x y)
"Args: array list
Elements of Y are either integers or nil. List is
of order array-rank of X. The array gets sliced
along the non-nil elements of Y. Thus if X is
10 x 3 x 7, then (array-slice x '(5 nil 2))
produces a vector with three elements. And
(array-slice '(nil nil 3)) produces a 10 x 3
matrix." 
(let* (
       (mx (array-dimensions x))
       (my (if-else y (mapcar 'list y) (mapcar #'iseq mx)))
       (zy (apply #'select (adjoin x my)))
       (dy (remove-if #'(lambda (x) (= x 1)) (array-dimensions zy)))
       )
(make-array dy :displaced-to zy)
))

(defun array-subscript (x k)
"Args: array integer
Returns the subscript of the element of X
having row-major-index K. Inverse of
array-row-major-index."
(let* (
       (mx (array-dimensions x))
       (dx (array-rank x))
       (ss (make-list dx))
       )
(dotimes (i dx ss)
         (let* (
                (ex (1- (- dx i)))
                (ux (elt mx ex))
                (kx (mod k ux))
                (rx (/ (- k kx) ux))
                )
           (setf (elt ss ex) kx)
           (setf k rx)
           ))
))

(defun array-blow-up (r k y &optional (f #'*) (c 1))
"Args: list list array
Creates an array with array-rank R, which is the outer-product
(with respect to binary function F) of Y and an array with all 
elements equal to the constant C. The list K are the subscripts
of the dimensions of Y in R. Thus (select r k) is
(array-dimensions y). If Y is 3 x 2 we could say
(array-blow-up '(2 3 3) '(2 0) y). For the resulting Z 
(array-slice z '(nil 0 nil)) is (f c (transpose y))."
(unless (equal (select r k) (array-dimensions y))
        (error "Non-conforming indices in array-blow-up"))
(let* (
       (zz (make-array  r))
       (rz (array-rank zz))
       (ry (array-rank y))
       (ru (- rz ry))
       (ur (remove-if #'(lambda (x) (find x k)) (iseq rz)))
       (rr (select r ur))
       (uu (make-array rr :initial-element c))
       (yu (apl-outer-product (list y uu)))
       (ip (repeat nil rz))
)
(mapcar #'(lambda (i) (setf (elt ip (elt k i)) i)) (iseq ry))
(mapcar #'(lambda (i) (setf (elt ip (elt ur i)) (+ ry i))) (iseq ru))
(permute-array yu ip)
))

#| 
Last but not least: an array pretty printer 
|#

(defun array-print (x i j)
"Args: array
Sort of pretty-prints any array X. Makes slices
along dimensions I and J. If X is 10 x 3 x 7,
then (array-print x 0 1) prints 7 matrices of
dimensions 10 x 3, while (array-print x 2 0)
prints 3 matrices of dimensions 7 x 10."
(let* (
      (mx (array-dimensions x))
      (nx (array-total-size x))
      )
(dotimes (k nx) 
(let* (
       (sk (array-subscript x k))
       (si (elt sk i))
       (sj (elt sk j)) 
       )
(if (and (= 0 si) (= 0 sj))
(progn
 (setf (elt sk i) "row")
 (setf (elt sk j) "column")
 (print (coerce sk 'vector))
 (terpri)
 (setf (elt sk i) nil)
 (setf (elt sk j) nil)
 (if (< i j)
     (print-matrix (array-slice x sk))
     (print-matrix (transpose (array-slice x sk))))
))))
))   

(provide "array")


