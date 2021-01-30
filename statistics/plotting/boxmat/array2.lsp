
;(def a (matrix '(2 3) (list () '(2 3) '(4 5 6) '(1) () '(7 8 9 10))))
;(def i (matrix '(2 3) (list () '(1 2) '(1 2 3) '(1) () '(1 1 2 3 ))))
;> a
;#2A((NIL (2 3) (4 5 6)) (1 NIL (7 8 9 10)))
;> (aref a 1 2)
;(7 8 9 10)
;> (setf (aref a 0 1) '(1 2 3 4 5))
;(1 2 3 4 5)
;> a
;#2A((NIL (1 2 3 4 5) (4 5 6)) (1 NIL (7 8 9 10)))

(defun plus-nil   (a b) (if (or(not a)(not b)) nil (+ a b)))
(defun minus-nil  (a b) (if (or(not a)(not b)) nil (- a b)))
(defun times-nil  (a b) (if (or(not a)(not b)) nil (* a b)))
(defun divide-nil (a b) (if (or(not a)(not b)) nil (/ a b)))
(defun equals-nil (a b) (if (or(not a)(not b)) nil (= a b)))

(defun plus   (a b) (map-elements #'plus-nil   a b))
(defun minus  (a b) (map-elements #'minus-nil  a b))
(defun times  (a b) (map-elements #'times-nil  a b))
(defun divide (a b) (map-elements #'divide-nil a b))
(defun equals (a b) (map-elements #'equals-nil a b))

;(defun copy-array (array) (plus array 0))


(defun array-list (proto data)
"Places successive data elements into non-nil positions of proto.
proto is an array whose elements are vectors of length 0 or greater."
 (let* ((copy-proto (copy-array proto))
	(proto-v (make-array (array-total-size proto) :displaced-to copy-proto))
	(lengths (coerce (map-elements #'length proto-v) 'list))
	(positions (cumsum lengths))
	(copy-data (remove nil (combine data)))
	(indices (iseq 0 (1- (length copy-data))))
       )
       (if (/= (length (remove nil (combine proto))) (length copy-data))
	   (error "Wrong number of items in replacement data.")
       )
       (dotimes (i (length positions))
	(if ( > (select lengths i) 0)
	  (dotimes (dummy 1)
	    (setf (select proto-v i)
		  (select copy-data
			  (select indices (iseq 0 (1- (select lengths i)))
			  )
		  )
	    )
	    (if ( <= (select lengths i) (1- (length indices)))
	      (setf indices (select indices
				   (iseq (select lengths i) (1- (length indices)))
			    )
	      )
	    )
	  )
	)
       )
       copy-proto
 )
)

(defun array-size-list (proto-lengths data)
"Construct proto as a list of lists of size and shape proto-size.
Places successive data elements into non-nil positions of proto.
proto is an array whose elements are vectors of length 0 or greater."
 (let* ((proto-size (array-dimensions proto-lengths))
	(proto (make-array proto-size))
	(proto-v (make-array (array-total-size proto-lengths) :displaced-to proto))
	(lengths (make-array (array-total-size proto-lengths) :displaced-to proto-lengths))
       )
       (dotimes (i (length lengths))
		(setf (select proto-v i) (repeat 0 (select lengths i)))
       )
       (array-list proto data)
 )
)


(defun wilk-sweep (work index)
"Wilkinson sweep"
  (let* ((workv (remove nil (combine work)))
	 (indexv (remove nil (combine index)))
	)
	(dolist (i (remove-duplicates indexv))
	 (setf indexvi (which (equals indexv i)))
	 (setf (select workv indexvi)
	       (repeat (mean (select workv indexvi)) (length indexvi))
	 )
	)
	(array-list work workv)
  )
)

;> (array-list tw (repeat (matrix '(2 3) '(1 1 1 2 2 2))  (map-elements #'length tw)))
;#2A(((1 1 1) (1 1 1 1) (1 1 1 1 1)) ((2 2 2 2) (2 2 2 2 2) (2 2 2 2 2 2)))
;> (array-list tw (repeat (matrix '(2 3) '(1 2 3 1 2 3))  (map-elements #'length tw)))
;#2A(((1 1 1) (2 2 2 2) (3 3 3 3 3)) ((1 1 1 1) (2 2 2 2 2) (3 3 3 3 3 3)))

(defun row-indices (tw)
"generates array of row indices in the same strucure as tw."
  (let* ((nrow (array-dimension tw 0))
	 (ncol (array-dimension tw 1))
	 (row-names (matrix (list nrow ncol)
			    (repeat (iseq 1 nrow) (repeat ncol nrow)))
	 )
	 (item-lengths (map-elements #'length tw))
	)
	(array-list tw (repeat row-names item-lengths))
  )
)

(defun col-indices (tw)
"generates array of row indices in the same strucure as tw."
  (let* ((nrow (array-dimension tw 0))
	 (ncol (array-dimension tw 1))
	 (col-names (matrix (list nrow ncol)
			    (repeat (iseq 1 ncol) nrow))
	 )
	 (item-lengths (map-elements #'length tw))
	)
	(array-list tw (repeat col-names item-lengths))
  )
)

(defun interaction-indices (a-in b-in)
"Generate set of unique indices for interaction of effects."
 (let* ((a  (remove nil (combine a-in)))
	(b  (remove nil (combine b-in)))
	(aa (remove-duplicates a))
	(bb (remove-duplicates b))
	(laa (length aa))
	(lbb (length bb))
	(aaa (copy-list a))
	(bbb (copy-list b))
	(subset nil)
       )
       (dotimes (i laa)
	 (setf subset (which (= (select aa i) a)))
	 (setf (select aaa subset) (repeat i (length subset)))
       )
       (dotimes (i lbb)
	 (setf subset (which (= (select bb i) b)))
	 (setf (select bbb subset) (repeat i (length subset)))
       )
       (array-list a-in (+ (* aaa lbb) bbb))
 )
)


(defun non-nil (array)
"coerce to vector and remove all nil elements."
 (remove nil (combine array))
)

(defun ss (array)
"sum of squares of items in array."
 (let* ((data (non-nil array))
       )
       (sum (* data data))
 )
)



;> (combine (outer-product '(2 3) (list '(3 3 4 2 3)) #'position))
;(3 0)
;> (select '(3 3 4 2 3) (combine (outer-product '(2 3) (list '(3 3 4 2 3)) #'position)))
;(2 3)


;(defun r24 (array)
;  (make-array '(2 4) :displaced-to array)
;)

(defun reshape (size array)
 (make-array size :displaced-to array)
)


;;from oneway.lsp
;(defmeth oneway-model-proto :grouped-data (&optional data)
;"Message args: (&optional data)
;Sets or returns the grouped data."
;  (when data
;	 (let* ((y (apply #'append data))
;		(indices (repeat (iseq 0 (- (length data) 1))
;				 (mapcar #'length data)))
;		(levels (remove-duplicates indices))
;		(indicators (mapcar #'(lambda (x) (if-else (= x indices) 1 0))
;				    levels))
;		(x (apply #'bind-columns indicators)))
;	   (setf (slot-value 'y) y)
;	   (setf (slot-value 'x) x)
;	   (setf (slot-value 'intercept) nil)
;	   (setf (slot-value 'grouped-data) data)
;	   (send self :needs-computing t)))
;   (slot-value 'grouped-data))
;
;(defmeth oneway-model-proto :group-names (&optional (names nil set))
;"Method args: (&optional names)
;Sets or returns group names."
;  (if set (setf (slot-value 'predictor-names) names))
;  (let ((g-names (slot-value 'predictor-names))
;	 (ng (length (slot-value 'grouped-data))))
;    (if (not (and g-names (= ng (length g-names))))
;	 (setf (slot-value 'predictor-names)
;	       (mapcar #'(lambda (a) (format nil "Group ~a" a))
;		       (iseq 0 (- ng 1))))))
;  (slot-value 'predictor-names))



(defun dummy-vars (factor)
"Constructs 0/1 dummy variables for factor."
	(let* ((f (combine factor))
	       (levels (remove-duplicates f))
	       (x (outer-product f levels
				 #'(lambda (a b) (if-else (= a b) 1 0))
		  )
	       )
	      )
	      x
	)
)


