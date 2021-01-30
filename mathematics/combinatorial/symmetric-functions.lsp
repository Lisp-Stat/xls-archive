; From "Estimation Problems in the Rasch-Model: the Basic Symmetric Functions"
;      N.D. Verhelst, Utrecht, C.A.W. Glas, Arnhem and A. van der Sluis,
;      Utrecht
;      CSQ - Computational Statistics Quarterly, Vol 1, Issue 3, 1984,
;      pp. 245-262

; symmetric - return the n symmetric functions of n-variables, evaluated
;             at vector (or list) eps.
;
; This computational strategy is slower than other methods, but has better
; numerical properties
;
; David A. Betz, 03-20.95
;

(defun symmetric (eps)
    (let ((n (make-array (list (length eps))))
          m
          (len (length eps))
          tmp)
      (do ((i 0 (+ i 1)))
          ((>= i len) n)
          (setf tmp (subseq eps 0 (+ i 1)))
          (setf (elt n 0) (sum tmp))
          (setf (elt n i) (prod tmp))
          (do ((j 1 (+ j 1)))
              ((>= j i) nil)
              (setf (elt n j)
                    (+ (elt m j) (* (elt m (- j 1)) (elt eps i )))))
          (setf m (copy-seq n)))))

; dsymmetric - returns the n 1st partial derivatives of the n symmetric
;              functions of n variables as an array of vectors.  The
;              expression
;
;              (aref (aref (dsymmetric eps) i) j)
;
;              gives the partial derivative wrt eps(i) of the jth symmetric
;              function of n variables.

(defun dsymmetric (eps)
  (let ((result (make-array (list (length eps)))) (len (length eps)))
    (dotimes (i len result)
      (setf (aref result i)
            (coerce (append (copy-seq '(1))
                            (coerce (symmetric (append (subseq eps 0 i)
                                                       (subseq eps (+ i 1))))
                                    'list))
                    'vector)))))

; ddsymmetric - returns the n*n 2nd partial derivatives of the n symmetric
;               of n variables as an nxn array of vectors.  The expression
;
;               (aref (aref (ddsymmetric eps) i j) k)
;
;               gives the partial derivative wrt eps(i),eps(j) of the kth
;               symmetric function of n variables.

(defun ddsymmetric (eps)
  (let (result
        (len (length eps))
        zeros)
       (setf result (make-array (repeat len 2)))
       (setf zeros (coerce (repeat 0 len) 'vector))
       (dotimes (i len nil)
         (setf (aref result i i) zeros))
       (dotimes (i len nil)
         (dotimes (j i nil)
           (setf (aref result i j)
                 (coerce (append (copy-seq '(0 1))
                            (coerce (symmetric (append (subseq eps 0 j)
                                                       (subseq eps (+ j 1) i)
                                                       (subseq eps (+ i 1))))
                                    'list))
                    'vector))))
       (dotimes (j len result)
         (dotimes (i j nil)
           (setf (aref result i j) (aref result j i))))))

