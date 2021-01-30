;;; Copyright 1991 Russell G. Almond
;;; License is granted to copy this program under the GNU library
;;; public licence.  This program comes with NO WARANTEE.  See file
;;; COPYRIGHT for details.


;;; These functions are stupid little routines which are used all the
;;; time for checking parameter items.  They are also used in the
;;; implementation of the range-slider, so they are included here.  

;;; They are used for such operations as (1) insuring a value is
;;; within a range, (2) insuring a range is within limit, and (3)
;;; insuring that a value is an integer if required to be


;;; Argument Checking predicates

(require :new-provide (strcat ElToY-directory "utils" *d-sep*  "new-provide.lsp"))

(defun checknump (num? &optional integer?)
  "Is argument <num?> a valid number.  If <integer?> <num?> must be an
integer. (vector-reducing).
"
  (if integer?
      (if (compound-data-p num?) (every #'integerp (element-seq num?))
	(integerp num?))
    (if (compound-data-p num?) (every #'numberp (element-seq num?))
	(numberp num?))))

; tests to make sure its argument is a list of two numbers
(defun test-pair (isit &optional integer?)
  "Is argument <isit> a list of two number in increasing order.  
   If optional argument <integer?>, values must be integers.
"
  (and (eql 2 (list-length isit))
       (if integer?
 	   (and (integerp (first isit)) (integerp (second isit)))
	 (and (numberp (first isit)) (numberp (second isit))))
       (< (first isit) (second isit))))

; 
(defun between (middle ends)
  "test to make sure first argument <middle> is between second arg
<ends> (a list of two numbers) 
*vector-reducing*
"
  (if (compound-data-p middle)
      (every #'(lambda (mid) (<= (first ends) mid (second ends)))
	     (element-seq middle))
    (<= (first ends) middle (second ends))))

;test to make sure first argument (a list) is between first arg (a
;list)
(defun between2 (middle ends)
  "test to make sure first argument <middle> (a list of two numbers)
is between second arg <ends> (a list of two numbers)
"
  (<= (first ends) (first middle) (second middle) (second ends)))



;; force a value to be between limits --- returns new value
(defun force-between (value limits)
  "Forces <value> to lie between <limits> which is a list of two
numbers in increasing order.  Returns <value> or closest endpoint. 
*vectorized*
"
  (cond ((compound-data-p value)
	 (map-elements #'force-between value (list limits)))
	((< value (first limits)) (first limits))
	((> value (second limits)) (second limits))
	(t value)))


;; force a range to be between limits --- if range is out of limits,
;; new range is limits
(defun force2-between (range limits)
  "Forces <range> a list of two number in increasing order to lie
between <limits> which is a list of two numbers in increasing order.
Returns <range> adjusted so that each endpoint lies within limits.  If
<range> lies entirely outside range, returns <limits>.
"
  (let ((new (mapcar #'(lambda (v) (force-between v limits)) range)))
    (if (apply #'eql new) limits
      new)))

;; force a value to be within range by adjusting range, as long as it
;; is within limits --- returns list, car is new value, cdr is new range
(defun force-within (value range limits)
  "Forces <value> to be within <limits> and adjust <range> to contain
<value>.  Returns (cons <new-value> <new-range>))
*vectorized*
"
  (let ((value (force-between value limits)))
    (list value (min (first range) value) (max (second range) value))))

;; force a value to be an integer if second argument is true.
(defun round-if (num int?)
  "Rounds values in <num> to the nearest integer if corresponding
value of <int?> is non-nil.
Partially *vectorized*.  
"
  (cond ((null int?) num)
	((and (listp int?) (eql (length int?) (length num)))
	 (mapcar #'(lambda (num1 int?1) (if int?1 (round num1) num1))
		 num int?))
	(t (round num))))

(defun ceiling-if (num int?)
  "Forces values in <num> upwards to the nearest integer if
corresponding value of <int?> is non-nil.
Partially *vectorized*.
"
  (cond ((null int?) num)
	((and (listp int?) (eql (length int?) (length num)))
	 (mapcar #'(lambda (num1 int?1) (if int?1 (ceiling num1) num1))
		 num int?))
	(t (ceiling num))))


(new-provide :el-checks)
