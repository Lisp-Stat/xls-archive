
(defun write-profiles (x)
"Args: x
The argument is a list of lists of strings. Each
element of the list corresponds with a variable,
the elements of the list corresponding with a
variable are the labels of that variable, which
are either strings or characters or numbers or symbols.
The program returns a matrix of strings coding all
the profiles."
(let* (
      (nvar (length x))
      (ncat (mapcar #'length x))
      (nprf (prod ncat))
      (arry (make-array (list nprf nvar)))
      )
  (dotimes (i nprf arry)
    (let (
          (icat (array-subscript ncat i))
          )
      (setf (select arry i (iseq nvar))
            (make-array (list 1 nvar) :displaced-to (coerce
            (mapcar #'(lambda (k)
                        (elt (to-string-list (elt x k))
                             (elt icat k))) (iseq nvar))
            'vector)))
      ))
  ))
    
(defun array-subscript (m k)
"Args: array integer
Returns the subscript of the element of
an array with array-dimensions mx
having row-major-index K. Inverse of
array-row-major-index."
(let* (
       (dx (length m))
       (ss (make-list dx))
       )
(dotimes (i dx ss)
         (let* (
                (ex (1- (- dx i)))
                (ux (elt m ex))
                (kx (mod k ux))
                (rx (/ (- k kx) ux))
                )
           (setf (elt ss ex) kx)
           (setf k rx)
           ))
))


(defun to-string-list (ls)
"Args: LIST
Converts LIST to list of strings."
(mapcar #'(lambda (x) (format nil "~a" x)) ls)
)
