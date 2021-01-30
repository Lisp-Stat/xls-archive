;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; do-index
;   var - a symbol
;   listform - a list of non-negative numbers, or nil
;   resultform - what will be returned when the loop is terminated
;   loop-on-nil - if nil and listform is nil, skip the loop
;                 if non-nil, perform the loop at least once, even if
;                     listform is nil.  In this case, var will be bound
;                     to nil for the loop iteration.
;   body - a prog-like collection of statements to be executed.
;
; Example:
;   (do-index (i '(2 3) t)
;       (format t "i=~a~%"))
;
;   will print
;
;     (0 0)
;     (0 1)
;     (0 2)
;     (1 0)
;     (1 1)
;     (1 2)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro do-index ((var listform &optional resultform loop-on-nil) &rest body)
"Syntax (do-index (var listform [result] [loop-on-nil]) {tag | statement}*)
Executes STATEMENTS, with VAR bound to every list composed of integers
bounded by (0 0 ... 0) and LISTFORM, in row-major sequence.  It returns
the value of RESULT (which defaults to NIL).  If LOOP-ON-NIL is non-nil,
then then the loop will be executed once even if LISTFORM is nil, in which
case i will be bound to NIL for the iteration.

This construct is useful for iterating over all the indices of an array."
  (let ((n-indices (gensym))
        (i (gensym))
        (done (gensym)))
       `(do ((,n-indices (- (length ,listform) 1))
             (,var (repeat 0 (length ,listform)))
             (,done (and (not ,loop-on-nil) (<= (length ,listform) 0))))
            (,done ,resultform)
            ,@body
            (do ((,i ,n-indices (- ,i 1)))
                ((< ,i 0)
                 (setf ,done t)
                 (return))
                (incf (elt ,var ,i))
                (cond ((< (elt ,var ,i) (elt ,listform ,i))
                       (return))
                      (t (setf (elt ,var ,i) 0)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; n-outer-product - a generalized n-dimensional outer product
;
;   x - a list of lists of numbers
;   f - the function to be used to combine the numbers from the lists of x
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun n-outer-product (x &optional (f #'*))
"Syntax (n-outer-product sequence-list &optional (fcn #'*))
Forms the outer product of all the sequences in SEQUENCE-LIST under the
operation f.  The result is a matrix of dimensions (mapcar #'length x)."
    (let ((result (make-array (mapcar #'length x) :initial-element 0.0D0)))
      (do-index (i (array-dimensions result) result)
          (setf (row-major-aref result
                                (apply #'array-row-major-index result i))
                (apply f (mapcar #'elt x i))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; array-subseq - obtain a list of array elements along a varying index
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun array-subseq (x index1 range index2 &optional val)
"Args (x index1 range index2 &optional val)"
  (do ((l range (cdr l))
       (m val (cdr m))
       (result nil))
      ((eq l nil) (nreverse result))
      (cond (val
             (setf (row-major-aref x
                                (apply #'array-row-major-index
                                       x (append index1
                                                 (list (car l))
                                                 index2)))
                   (car m))))
      (setf result (cons (apply #'aref x
                                (append index1 (list (car l)) index2))
                         result))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; n-dimensional fft
;       x - an n-dimensional array
; inverse - if true, the inverse fft will be computed
;
; This function calculates an n-dimensional fft.  If we have a matrix of
; m^n elements, this routine will calculate the fft by performing a one-
; dimensional fft m*m^(n-1)=m^n times
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun n-fft (x &optional (inverse nil))
"Args: (x &optional inverse)
Returns unnormalized Fourier transform of X, or inverse transform if INVERSE
is true."
    (let ((indices (array-dimensions x))
          (n-indices (array-rank x))
          (result (copy-array x)))
         (dotimes (i n-indices result)
           (do-index (j (butlast indices (- n-indices i)) nil t)
             (do-index (k (nthcdr (+ i 1) indices) nil t)
               (array-subseq result j (iseq (elt indices i)) k
                             (fft (array-subseq result
                                                j (iseq (elt indices i)) k)
                                  inverse)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; linbin - bin observations using nearest gridpoint linear interpolation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun linbin (x range-low range-high grid-size)
  (let (
        (result (make-array grid-size :initial-element 0.0D0))
        (delta (/ (- range-high range-low) (- grid-size 1)))
        (n (array-dimension x 0))
        lxi
        rem
        weights)
       (setf x (map-elements #'coerce (row-list x) 'list))
       (dotimes (i n result)
         (setf lxi (/ (- (elt x i) range-low) delta))
         (setf li (floor lxi))
         (setf rem (- lxi li))
         (cond ((not (or (which (< li 0)) (which (>= li (- grid-size 1)))))
                (setf weights
                      (n-outer-product (reverse (mapcar #'reverse
                                               (mapcar #'list rem (- 1 rem))))))
                (do-index (j (array-dimensions weights) nil)
                    (incf (row-major-aref result
                                          (apply #'array-row-major-index
                                                 result (+ j li)))
                          (apply #'aref weights j))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;         x - data
;             an array of points of R^m
;         h - the width of the kernel smoothing function
;             a scalar, or a list of m scalars
;             called bandwidth in the bkde2d
; grid-size - optional list of m integers representing the number of
;             lattice points to use in binning the data
;   range-x - the range of x to be smoothed
;             a list of m lists of 2 scalars
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun binned-kde (x h &optional grid-size range-x)
  (let ((tau 3.4)
        a
        b
        grid-counts
        result)

       ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
       ; if h (bandwidth) was specified as a scalar, convert it into a
       ; vector of m elements
       ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
       (cond ((numberp h)
              (setf h (repeat h (array-dimension x 1)))))

       ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
       ; if grid-size was not specified, pick some default values
       ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
       (cond ((not grid-size)
              (cond ((eq (array-dimension x 1) 2)
                     (setf grid-size '(51 51)))
                    ((eq (array-dimension x 1) 3)
                     (setf grid-size '(21 21 21))))))

       ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
       ; if range-x is not specified, calculate the range of x 
       ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
       (cond ((eq range-x nil)
              (setf range-x
                    (list (- (mapcar #'min (column-list x)) (* 1.5 h))
                          (+ (mapcar #'max (column-list x)) (* 1.5 h))))))

       ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
       ; a is the lower-bound for the x
       ; b is the upper-bound for the x
       ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
       (setf a (first range-x))
       (setf b (second range-x))

       (setf grid-counts (linbin x a b grid-size))

       ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
       ; optimize the calculation of the kernel by truncating the normal
       ; beyond 3.4*sigma, or at the last grid-point, whichever is smaller.
       ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
       (setf l (mapcar #'min (floor (/ (* tau h (- grid-size 1)) (- b a)))
                             (- grid-size 1)))

       (setf k (/ (mapcar #'normal-dens
                          (* (mapcar #'iseq (+ l 1))
                             (/ (- b a) (* h (- grid-size 1))))) h))

       (setf kapp (/ (n-outer-product k) (array-dimension x 0)))
       (setf p (^ 2 (ceiling (/ (log (+ grid-size l)) (log 2)))))
       (setf kernel-matrix (make-array p))
       (setf rp (make-array p :initial-element 0.0D0))

       (do-index (i (repeat 2 (array-rank kapp)) nil)
         (do-index (j (- (array-dimensions kapp) i) nil)
           (setf (row-major-aref rp
                                 (apply #'array-row-major-index
                                        rp
                                        (+ (* i p)
                                           (* (- 1 (* 2 i)) (+ i j)))))
                 (apply #'aref kapp (+ i j)))))

       (setf sp (make-array p :initial-element 0.0D0))
       (do-index (j (array-dimensions grid-counts) nil)
           (setf (row-major-aref sp
                                 (apply #'array-row-major-index sp j))
                 (apply #'aref grid-counts j)))

       (setf result (/ (realpart (n-fft (* (n-fft rp) (n-fft sp)) t))
                        (apply #'* p)))

       (cond ((matrixp result)
              (print-matrix result *STANDARD-OUTPUT* :float-digits 2))
             (t (format t "~a~%" result)))

       result))

; (binned-kde #2a((4 4)) '(1 1) '(5 5) '((2 2) (6 6)))

; (setf a (normal-rand 1000))
; (setf b
;       (binned-kde (make-array (list (length a) 1) :initial-contents a)
;                   '(.05) '(201) '((-1) (1))))
; (plot-points (rseq -1 (+ -1 (* .01 (- (length b) 1))) (length b)) b)
