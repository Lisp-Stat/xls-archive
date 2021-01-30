;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Appendix: Utility functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; A.1 Functions for Manipulating Tables
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun split-data (x y)
"Args: data split
DATA is a list of lists, all containing numbers or
strings. SPLIT is another list with numbers or
strings. Each list in DATA is split into a number
of lists, corresponding with the different elements
in SPLIT."
(let* (
      (z (sort-data (remove-duplicates y :test 'equal)))
      (m (length z))
      (n (length x))
      (u (coerce (make-array (list n m)) 'list))
      )
(dotimes (i n)
(dotimes (j m)
(setf (elt (elt u i) j)
      (select (elt x i) (which (map-elements #'equal y (elt z j)))))
))
u))

(defun multi-from-variable (x i)
"Args: LIST INDEX
Each element of LIST is a list of numbers. Element
INDEX is used to split the other lists. This is
converted to a list of rect-data objects."
(let* (
      (m (length x))
      (y (split-data (select x (remove i (iseq m))) (elt x i)))
      (n (length (first y)))
      (r nil)
      )
(dotimes (i n r)
(let (
      (s (mapcar #'elt y (repeat i m)))
      )
(setf r (append r (list 
        (send rect-data-proto :new s
                         :title (format nil "Group ~d" i)))))
))))

(defun multi-from-freq (x f)
"Args: LIST FREQ
Each element of LIST is a list of numbers. FREQ
gives group-sizes and partitions the list
INDEX is used to split the other lists. This is
converted to a list of rect-data objects."
(let (
      (m (length f))
      (c (cumsum f))
      (r (nil))
      )
(dotimes (i m r)
(let (
      (s nil)
      )
(setf r (append r (list (send rect-data-proto :new s
                              :title (format nil "Group ~d" i)))))
))))

(defun make-indicator (x)
"Args: sequence
Elements of SEQUENCE are either numbers or strings. 
Returns a dummy with sorted category values."
(let* (
      (y (sort-data (remove-duplicates x :test 'equal)))
      (m (length y))
      (n (length x))
      (a (make-array (list n m) :initial-element 1))
      (b (make-array (list n m) :initial-element 0))
      )
(if-else (outer-product x y #'equal) a b)
))

(defun cross-table (x y)
"Args: first second
FIRST and SECOND are sequences of numbers or of strings.
They have equal length. 
Their two-dimensional cross-table is formed."
(let (
     (g (make-indicator x))
     (h (make-indicator y))
     )
(matmult (transpose g) h)
))

(defun marginals (x)
"Args: first
FIRST is a sequence of numbers or strings.
Different entries are sorted and counted."
(mapcar #'sum (column-list (make-indicator x))))

(defun unique (x)
"Args: first
FIRST is a sequence of numbers or strings.
Different entries are sorted and returned."
(sort-data (remove-duplicates x :test 'equal)))

(defun row-percentage (x)
"Args: table
Compute a row-percentaged version of TABLE"
(let ( 
     (r (mapcar #'sum (row-list x)))
     (n (length (elt (coerce x 'list) 0)))
     )
(/ x (outer-product r (repeat 1 n)))
))

(defun column-percentage (x)
"Args: table
Compute a column-percentaged version of TABLE"
(let ( 
     (c (mapcar #'sum (column-list x)))
     (n (length (coerce x 'list)))
     )
(/ x (outer-product (repeat 1 n) c))
))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; A.2 Additional descriptive statistics
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun skewness (x)
(let* (
     (n(length x))
     (m (mean x))
     (y (- x m))
     (s2 (/ (sum (^ y 2)) n))
     (s3 (/ (sum (^ y 3)) n))
     )
(/ s3 (^ s2 1.5))
))

(defun kurtosis (x)
(let* (
     (n(length x))
     (m (mean x))
     (y (- x m))
     (s2 (/ (sum (^ y 2)) n))
     (s4 (/ (sum (^ y 4)) n))
     )
(- (/ s4 (^ s2 2)) 3)
))

(defun range (x)
(- (max x) (min x))
)

(defun statfun (x)
(funcall (intern (string-upcase (get-string-dialog "Statistic:"))) x)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; A.3 Additional Transformations
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun square (x)
(^ x 2))

(defun cube (x)
(^ x 3))

(defun ln (x)
(if (> 0 (min x)) (warning x)
(log x)))

(defun squareroot (x)
(if (> 0 (min x)) (warning x)
(sqrt x)))

(defun cuberoot (x)
(^ x (/ 3)))

(defun reciprocal (x)
(/ x))

(defun neg (x)
(- x))

(defun sign (x)
(cond ((> x 0) 1)
      ((= x 0) 0)
      ((< x 0) -1))
)

(defun arcsin (x)
(if (or (> 0 (min x)) (< 1 (max x))) (warning x)
(asin x))
)

(defun atanh (x)
(if (or (>= -1 (min x)) (<= 1 (max x))) (warning x)
(/ (log (/ (+ 1 x) (- 1 x))) 2))
)

(defun logit (x)
(if (or (>= 0 (min x)) (<= 1 (max x))) (warning x)
(log (/ x (- 1 x))))
)

(defun probit (x)
(if (or (>= 0 (min x)) (<= 1 (max x))) (warning x)
(normal-quant x))
)

(defun warning (x)
(message-dialog "Argument not in Range")
x)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; A.4 Defuns for manipulation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun non-missing (data code)
"Args: data code
Extracts the list of strings not equal to the string CODE
from list of strings DATA."
(select data (which 
 (map-elements #'string/= (to-string-list data) code)))
)


(defun missing (data code)
"Args: data code
Extracts the indices of strings equal to the string CODE
from list of strings DATA."
(which (map-elements #'string= (to-string-list data) code))
)

(defun which-numerical (data code)
"Args: data code
Extract the indices of numerical data values
from those strings from the list DATA that are not equal to CODE."
(select-number-indices (non-missing data code))
)

(defun numerical (data code)
"Args: data code
Extracts the numerical data values
from those strings from the list DATA that are not equal to CODE."
(select-number-list (non-missing data code))
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; A.5 Predicates and Type-conversion
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun listpp (x)
"Args: ANY
Predicate checks if ANY is a list of lists."
(if (listp x) 
    (not (member nil (mapcar #'listp x))) nil)
)

(defun stringpp (x)
"Args: ANY
Predicate checks if ANY is a list of strings."
(if (listp x) 
    (not (member nil (mapcar #'stringp x))) nil)
)

(defun processp (x)
(or (equal x "continuous") (equal x "discrete"))
)

(defun levelp (x)
(or (equal x "nominal") (equal x "ordinal") (equal x "numerical"))
)

(defun to-string-list (ls)
"Args: LIST
Converts LIST to list of strings."
(mapcar #'(lambda (x) (format nil "~a" x)) ls)
)

(defun to-number-list (ls)
"Args: LIST
Reads strings from LIST. If they are numbers, they are converted."
(let* (
     (cs (mapcar #'(lambda (x) 
                     (read (make-string-input-stream x) nil)) ls))
     (ss (which (mapcar #'numberp cs)))
     )
(setf (select ls ss) (select cs ss))
ls))

(defun select-number-list (ls)
"Args: LIST
Reads strings from LIST. If they are numbers, they are selected."
(let* (
     (cs (mapcar #'(lambda (x) 
                     (read (make-string-input-stream x) nil)) ls))
     (ss (which (mapcar #'numberp cs)))
     )
(select cs ss)
))

(defun select-number-indices (ls)
"Args: LIST
Reads strings from LIST. If they are numbers, their index is
returned."
(which (mapcar #'numberp 
               (mapcar #'(lambda (x) 
                     (read (make-string-input-stream x) nil)) ls)))
)

(defun eval-string-input (s)
"Args: string
Converts STRING to expression, and evaluates."
(eval (read (make-string-input-stream s) nil)))

(defun filter-data (data)
"Args: data
Checks if DATA is a sequence. If it is not, exit with
error and return nil. If it is, coerce it to a list
of strings."
(if (sequencep data) 
    (setf data (to-string-list (coerce data 'list)))
    (progn (error "Data is not a Sequence") nil))
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; A.6 Recode Methods
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun recode-discrete (x a b)
"Args: sequence old-value new-value
All values equal to OLD-VALUE in SEQUENCE 
are replaced by NEW-VALUE.
Elements of SEQUENCE can be anything simple or
compound, as long as EQUAL is defined."
(let* (
      (n (length x))
      (e (map-elements (function equal) x a))
      )
(if-else e (repeat b n) x)
))

(defun recode-interval (x a b c)
"Args: sequence lower-bound upper-bound new-value
All values between LOWER-BOUND and UPPER-BOUND in SEQUENCE 
are replaced by NEW-VALUE. LOWER-BOUND and UPPER-BOUND
should be numbers. Values equal to LOWER-BOUND are
not recoded, values equal to UPPER-BOUND are recoded.
The function is non-destructive."
(let (
     (e (intersection (which (<= x b)) (which (> x a))))
     (y (copy-list x))
     )
(setf (select y e) (repeat c (length e)))
y))

(defun recode-equal (x a)
"Args: sequence width
Numerical SEQUENCE is recoded in equal intervals of width WIDTH."
(let (
     (b (round (/ x a)))
     )
(- b (min b))
))

(defun recode-uniform (x m)
"Args: sequence num
Numerical SEQUENCE is recoded in NUM intervals with equal frequencies."
(let (
     (b (rank x))
     (n (round (/ (length x) m)))
     )
(dotimes (i m)
         (setf b (recode-interval b (* i n) (* (1+ i) n) i))
)
b))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; A.7 Resampling
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun boot-plain (a)
"Args: a
Draws a simple bootstrap sample from a list A."
(sample a (length a) t)
)

(defun fit-normal (x)
"Args: x
Computes mean and standard deviation, and returns list of
list of arguments and list of ordinates, suitable for
add-lines in one-dim plots"
(let* (
       (m (mean x))
       (s (standard-deviation x))
       (u (max x))
       (v (min x))
       (a (/ (- u v) 50))
       (b (/ (+ u v) 2))
       (z (+ b (* a (- (iseq 51) 25))))
       (p (/ (normal-dens (/ (- z m) s)) s))
       )
(list z p)))

(defun boot-histogram (data)
(let* (
      (n (length data))
      (w1 (histogram nil :title "Bootstrap Sample" ))
      (w2 (histogram data :title "The Data"))
      (jj 0)
      (jo nil)
      )
(send w2 :location 300 20)
(dotimes (i n)
(setf jj (random n))
(setf jo (append jo (list (elt data jj))))
(send w2 :selection (list jj))
(send w1 :add-points (list (elt data jj)))
(send w1 :draw-string (num-to-string i) 20 20)
(send w1 :selection (list i))
(send w1 :adjust-to-data)
(pause 10)
)
(send w1 :remove)
(send w2 :remove)
jo
))

(defun dconvert (cs)
"Args: list
Converts a LIST of characters to an integer. Only
characters -, +, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9 
are allowed. Leading zeroes or spaces are no problem.
Neither are trailing spaces."
(let  (
      (sgn 1)
      )
(cond ((member #\+ cs) (setf cs (rest (member #\+ cs))))
      ((member #\- cs) (setf cs (rest (member #\- cs)))
                       (setf sgn -1))
      )
(let*  (
       (t (- (mapcar #'char-code cs) 48))
       (v (select t (which (>= t 0))))
       (b (reverse (** 10 (iseq (length v)))))
       )
(* sgn (sum (* b v)))
)))



