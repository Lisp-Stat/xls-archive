;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; The call (jack-me 'foo), where foo is any function, produces 
;;; three global function definitions. The function foo must return 
;;; a number, a vector, a matrix or an array, and its argument
;;; must be a list of numbers or vectors, or arrays of the same 
;;; dimension. The three functions jack-pseudo-values-of-foo,
;;; jack-average-of-foo and jack-dispersion-of-foo take the
;;; same argument as foo, and they return, respectively,
;;; the pseudo-values, their average, or their dispersion.
;;;
;;; Jan de Leeuw, 2-18-95
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 


(defun jack-me (func)
(let* (
      (ffunc (symbol-name func))
      (pname (concatenate 'string "JACK-PSEUDO-VALUES-OF-" ffunc))
      (aname (concatenate 'string "JACK-AVERAGE-OF-" ffunc))
      (dname (concatenate 'string "JACK-DISPERSION-OF-" ffunc))
      (pfunc (jack-pseudo-values func))
      (afunc (jack-average func))
      (dfunc (jack-dispersion func))
      (psymb (intern pname))
      (asymb (intern aname))
      (dsymb (intern dname))
      )
  (setf (symbol-function psymb) pfunc)
  (setf (symbol-function asymb) afunc)
  (setf (symbol-function dsymb) dfunc)
(values psymb asymb dsymb)
))

(defun jack-pseudo-values (func)
  #'(lambda (data)
      (let* (
             (n (length data))
             (l (iseq n))
             (f (symbol-function func))
             (p (make-list n))
             )
(dotimes (i n)
(setf (elt p i)
      (funcall f (select data (which (/= i l))))))
(- (* n (make-list n :initial-element (funcall f data)))
   (* (1- n) p))
))
)
 
(defun jack-average (func)
  #'(lambda (data)
      (average (funcall (jack-pseudo-values func) data)))
)

(defun jack-dispersion (func)
  #'(lambda (data)
      (dispersion (funcall (jack-pseudo-values func) data)))
)

(defun average (x)
"Args: list
Takes the average of all elements in a list"
(if (not (listp x)) (error "Argument for average must be a list"))
  (/ (apply #'+ x) (length x)))

(defun dispersion (x)
"Args: list
Computes the dispersion of all elements in a list.
The list must have either numbers or sequences 
of numbers"
(if (not (listp x)) (error "Argument for dispersion must be a list"))
  (let (
        (n (length x))
        (m (average x))
        (s (apply #'+ (mapcar #'(lambda (z) (outer-product z z)) x)))
        )
    (- (/ s n) (outer-product m m))
))

