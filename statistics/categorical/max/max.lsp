;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Functions to do quantization and cluster analysis in the empirical case
;;; (I will write a small TeX thingy later).
;;;
;;; Jan de Leeuw 2-22-95
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun max-cut-points (ncut &key (func "normal") (epsilon 1e-6) (verbose nil))
(let (
      (ksi (mapcar (symbol-function (intern
                   (concatenate 'string (string-upcase func) "-QUANT")))
                   (/ (1+ (iseq ncut)) (1+ ncut))))
      )
(loop 
       (setf a (max-int-values func ksi verbose))
(let (
     (new-ksi (mapcar 
               #'(lambda (i) (/ (+ (elt a i) (elt a (1+ i))) 2))
               (iseq ncut)))
     )
(if (< (max (abs (- ksi new-ksi))) epsilon)
    (return ksi) (setf ksi new-ksi)))
)))

(defun max-int-values (func ksi verbose)
(let* (
       (f (mapcar (symbol-function (intern 
                  (concatenate 'string (string-upcase func) "-CDF")))
                                   ksi))
       (u (mapcar (symbol-function (intern
                  (concatenate 'string (string-upcase func) "-MDF")))
                                   ksi))
       (m (funcall (symbol-function (intern 
                  (concatenate 'string (string-upcase func) "-MEAN")))))
       (v (funcall (symbol-function (intern 
                  (concatenate 'string (string-upcase func) "-SSQ")))))
       )
(setf f (difference (append (list 0) f (list 1))))
(setf u (difference (append (list 0) u (list m))))
(if verbose
    (format t "~10,6f~%" (- v (sum (/ (* u u) f)))))
(/ u f)
))

(defun empirical-max-cut (ncut data &key (epsilon 1e-6) (verbose nil))
(let (
      (ksi (mapcar #'(lambda (x) (quantile data x))
                             (/ (1+ (iseq ncut)) (1+ ncut))))
      (sdt (sort-data data))
      )
(loop 
       (setf a (empirical-max-int ksi sdt verbose))
(let (
     (new-ksi (mapcar 
               #'(lambda (i) (/ (+ (elt a i) (elt a (1+ i))) 2))
               (iseq ncut)))
     )
(if (< (max (abs (- ksi new-ksi))) epsilon)
    (return ksi) (setf ksi new-ksi)))
)))

(defun empirical-max-int (ksi data verbose)
(let* (
       (f (mapcar #'(lambda (x) (empirical-cdf x data)) ksi))
       (u (mapcar #'(lambda (x) (empirical-mdf x data)) ksi))
       (m (mean data))
       (v (mean (^ data 2)))
       )
(setf f (difference (append (list 0) f (list 1))))
(setf u (difference (append (list 0) u (list m))))
(if verbose
    (format t "~10,6f~%" (- v (sum (/ (* u u) f)))))
(/ u f)
))

(defun normal-mean () 0)

(defun normal-ssq () 1)

(defun normal-mdf (x)
  (- (normal-dens x)))

(defun uniform-cdf (x) x)

(defun uniform-quant (x) x)

(defun uniform-mdf (x) (/ (* x x) 2))

(defun uniform-mean () .5)

(defun uniform-ssq () (/ 3))

(defun empirical-cdf (x data)
  (/ (length (which (< data x))) (length data))
)

(defun empirical-mdf (x data)
  (/ (sum (select data (which (< data x)))) (length data))
)
