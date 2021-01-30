;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Elementary symmetric functions. Recursive version.
;;
;; Version 1.0 *** Jan de Leeuw and David A. Betz *** 03-18-95
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun symmetric-functions (x)
 (if (single x) (first x)
      (let ((y (symmetric-functions (butlast x))))
    (+ (combine y 0)
       (* (the-last x) (combine 1 y)))))
  )

(defun symmetric-functions-gradient (x)
  (let ((nn (iseq (length x))))
    (mapcar #'(lambda (i)
                (combine 1 (butlast
                            (symmetric-functions
                             (except-for-one x i))))) nn)))

(defun symmetric-functions-hessian (x)
  (let* ((n (length x))
         (nn (iseq n))
         (nm (repeat 0 (- n 1))))
    (outer-product nn nn #'(lambda (i j)
                             (if (= i j) nm
                               (combine 0 1 (butlast
                                             (symmetric-functions
                                              (except-for-two x i j)))))))
    ))
 
(defun except-for-one (x i)
  (let ((n (iseq (length x))))
    (select x (which (/= i n)))))

(defun except-for-two (x i j)
  (let ((n (iseq (length x))))
    (select x (remove-if #'(lambda (k) 
                             (or (= i k) (= j k))) n))
))

(defun single (lst)
  (and (consp lst) (not (cdr lst))))

(defun the-last (lst)
  (first (last lst)))

(provide "symmetric-functions")