;; Jan de Leeuw, April 27, 1995

(defun encode (x base)
"Args: number base
Convert decimal NUMBER to NUMBER in base BASE."
  (let* ((nb (length base))
         (lb (make-list nb)))
    (dotimes (i nb lb)
             (let* ((k (1- (- nb i)))
                    (b (elt base k))
                    (y (mod x b)))
               (setf (elt lb k) y)
               (setf x (/ (- x y) b))
               ))
 )) 

(defun decode (x base &key (zero-special nil))
"Args: number base
Converts base BASE number NUMBER to decimal number. Both
NUMBER and BASE are lists. If ZERO-SPECIAL is true, then
all numbers with a zero in it are transformed to zero."
(let* ((y (reverse (accumulate #'* base)))
       (z (rest (combine y 1))))
(if zero-special
    (if (member 0 x) 0 (sum (* x z)))
    (sum (* x z)))
))

(defun all-profile-list (base)
"Args: base
Makes a list of lists, each list of the same length of base,
with all combinations of numbers between zero and base."
  (mapcar #'(lambda (x) (encode x base)) (iseq (prod base)))
)

(defun all-profile-matrix (base)
"Args: base
Makes an array, each row of the same length of base,
with all combinations of numbers between zero and base."
(let ((m (length base))
      (n (prod base)))
(make-array (list n m) :displaced-to
            (coerce (element-seq (all-profile-list base)) 'vector))
))

(defun all-subsets (list)
"Args: list
Power set of list."
(let* ((n (length list))
       (nn (^ 2 n)))
(mapcar #'(lambda (x) (select list (which (= 1 x))))
        (mapcar #'(lambda (x) (encode x (make-list n :initial-element 2)))
                (iseq nn)))
))

(defun is-subsetp (x y)
"Args: list1 list2
Is LIST1 a subset of one of the lists in LIST2 ?"
(nor (mapcar #'(lambda (z) (subsetp x z)) y))
)

(defun nor (list)
"Args: list
Returns true if at least one of the elements is true."
  (if (singlep list) (or (first list))
      (or (first list) (nor (rest list))))
)

(defun nand (list)
"Args: list
Returns true if all elements are true."
  (if (singlep list) (and (first list))
      (and (first list) (nand (rest list))))
)

(defun singlep (x)
"Args: x
Is X a list with a single element ?"
  (equal x (list (first x))))

(provide "decode-encode")