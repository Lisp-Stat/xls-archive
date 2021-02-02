;;; functions for handling tables
;;; John Fox, May 1995  (fox@yorku.ca)
;;; table-apply behaves like the tapply funtion is S to construct a table
;;;  of statistics
;;; print-table formats and prints a multidimensional table, constructed
;;;   e.g., by table-apply

(require "array")

(defun table-apply (var &key factors (function #'length))
"Args: (var &key factors (function #'length))
Applies the function argument to each cell of the ragged array of elements
of var cross-classified by the factors. Returns a two-item list, the first
item containing lists of factor categories, the second the array of results.
var      a sequence of length n, if nil a sequence of 1's is supplied
factors  a list of sequences, each of length n
function a function to be applied cell-wise to the cross-classification,
         default is #'length, counting the entries in each cell
Examples: (table-apply income :factors (list region gender) :function #'mean)
                         ; table of income means by region and gender
          (table-apply response :factors (list a b c) :function #'list)
                         ; returns ragged array
          (table-apply nil :factors (list age gender income)
                         ; table of counts"
(flet ((sort-dat (x)
                 (if (or (numberp (first x)) (stringp (first x)))
                     (sort-data x)
                     x)))
  (setf var (if var
                var
                (repeat 1 (length (first factors)))))
  (let*  ((levels (mapcar #'sort-dat (mapcar #'remove-duplicates factors)))
          (tab
           (let* ((n (length var))
                  (dim (mapcar #'length levels))
                  (tab (make-array dim))
                  (vec (make-array (array-total-size tab) :displaced-to tab)))
             (dotimes (obs n tab)
                      (let* ((values (map-elements #'nth obs factors))
                             (value (nth obs var))
                             (sub (mapcar #'position values levels))
                             (old (apply #'select tab sub))
                             (new (cons value old))
                             (index (apply #'array-row-major-index tab sub)))
                        (setf (select vec index) new)))))
          (vec (make-array (array-total-size tab) :displaced-to tab))
          (fnlist (mapcar function (coerce vec 'list))))
    (list levels (make-array (array-dimensions tab) :initial-contents 
fnlist)))))


(defun print-table (table)
"Args: table 
Prints a table produced by table apply, consisting of a two-item
list, the first item of which is a list of lists of dimension names,
the second item of which is an array."
; based on the array-print function from the array function library
(flet ((border-matrix (a b c d)
                      (bind-rows 
                       (concatenate 'list (list d) c )
                       (bind-columns b a))))
  (let* (
         (x (second table))
         (levels (first table))
         (mx (array-dimensions x))
         (nx (array-total-size x))
         )
    (if (= 1 (length mx))
        (print-matrix (bind-rows (first levels) x))
        (dotimes (k nx) 
                 (let* (
                        (sk (array-subscript x k))
                        (si (elt sk 0))
                        (sj (elt sk 1)) 
                        )
                   (if (and (= 0 si) (= 0 sj))
                       (progn
                        (print (cddr (coerce (mapcar #'select levels sk) 
'list)))
                        (terpri)
                        (setf (elt sk 0) nil)
                        (setf (elt sk 1) nil)
                        (print-matrix 
                         (border-matrix (array-slice x sk)
                                        (select levels 0)
                                        (select levels 1)
                                        " "))
                        )))))
    )))



