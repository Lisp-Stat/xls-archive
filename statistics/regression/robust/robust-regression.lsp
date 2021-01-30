;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This is the Xlisp-Stat version of ROSEPACK, the robust regression
;; package developed by Holland, Welsch, and Klema around 1975. See
;; Holland and Welsch, Commun. Statist. A6, 1977, 813-827. See also
;; the Xlisp-Stat book, pages 173-177, for an alternative approach.
;;
;; The algorithm below leaves all the work to the regression-model-proto.
;; It uses LAV regression for the starting point.
;;
;; Version 1.0 ** March 25 1995 ** Jan de Leeuw
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require "lav-regression")

(defmeth regression-model-proto :med-scale ()
  (let* ((r (send self :raw-residuals))
         (m (median r)))
    (/ (median (abs (- r m))) .6745)))

(defmeth regression-model-proto :reweight (r type)
  (cond ((string= type "A") (andrews-et-al r))
        ((string= type "B") (beaton-and-tukey r))
        ((string= type "T") (hinich-and-talwar r))
        ((string= type "C") (cauchy-or-t r))
        ((string= type "W") (dennis-and-welsch r))
        ((string= type "H") (huber r))
        ((string= type "L") (logistic r))
        ((string= type "F") (fair r))
        (t (error "Unknown Weight Type")))
  )

(defun robust-regression
  (x y &key (eps 1e-6) (display t) (intercept t)
     (type "H") (tuner nil) (printlav nil) (verbose nil))
  (let* ((m (lav-regression x y :display printlav
                           :intercept intercept :verbose nil))
         (b (send m :coef-estimates))
         (s (send m :med-scale)))
    (loop
     (let* ((r (/ (send m :raw-residuals) s)))
       (send m :weights
             (if (numberp tuner)
                 (send m :reweight r type tuner)
               (send m :reweight r type)))
       (let ((c (send m :coef-estimates)))
         (if verbose (format t "~20,10f~%"
                             (send m :residual-sum-of-squares)))
         (if (< (max (abs (- c b))) eps)
             (progn (if display (send m :display))
                    (return m))
         (setf b c)))))
))
       

(defun andrews-et-al (r &optional (A 1.339))
  (map-elements #'(lambda (x)
                    (if (<= (abs x) (* pi A))
                        (/ (sin (/ x A)) (/ pi A)) 0)) r)
  )

(defun beaton-and-tukey (r &optional (B 4.685))
  (map-elements #'(lambda (x)
                    (if (<= (abs x) B)
                        (^ (- 1 (^ (/ x B) 2)) 2) 0)) r)
  )

(defun hinich-and-talwar (r &optional (TT 2.795))
  (map-elements #'(lambda (x)
                    (if (<= (abs x) TT) 1 0)) r)
  )

(defun cauchy-or-t (r &optional (C 2.385))
  (/ (1+ (^ (/ r C) 2)))
  )

(defun dennis-and-welsch (r &optional (W 2.985))
  (exp (- (^ (/ r W) 2)))
  )

(defun huber (r &optional (H 1.345))
  (map-elements #'(lambda (x)
                    (if (<= (abs x) H) 1 (/ H x))) r)
  )

(defun logistic (r &optional (L 1.205))
  (/ (tanh (/ r L)) (/ r L))
  )

(defun fair (r &optional (F 1.400))
  (/ (1+ (/ (abs r) F)))
  )
  