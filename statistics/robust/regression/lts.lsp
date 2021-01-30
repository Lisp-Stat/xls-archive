; David A. Betz 03-21-95

; Based on an algorithm presented in "Exact computation of least trimmed
; squares estimate in simple linear regression" by Ola Hossjer, Computational
; Statistics and Data Analysis, 19 (1995), pp 265-282

; The following data are the lactic acid data, taken from "Statistical
; analysis, a computer oriented approach", by Afifi and Azen
;
;     (setf x '( 1  1  1  1  3  3  3  3  3  5  5  5
;               10 10 10 10 15 15 15 15))
;     (setf y '( 1.1  0.7  1.8  0.4  3.0  1.4  4.9  4.4  4.5  7.3  8.2  6.2
;               12.0 13.1 12.6 13.2 18.7 19.7 17.4 17.1))
;
; These data should give a Dmin=1.5785 and beta=1.3061 for n=20, h=10.
;     (lts x y 10)


; The following data are the stars data, taken from "Robust Regression and
; Outlier Detection", Rousseeuw and Leroy:
;
;    (setf x '(4.37 4.56 4.26 4.56 4.30 4.46 3.84 4.57 4.26 4.37 3.49 4.43
;              4.48 4.01 4.29 4.42 4.23 4.42 4.23 3.49 4.29 4.29 4.42 4.49
;              4.38 4.42 4.29 4.38 4.22 3.48 4.38 4.56 4.45 3.49 4.23 4.62
;              4.53 4.45 4.53 4.43 4.38 4.45 4.50 4.45 4.55 4.45 4.42))
;    (setf y '(5.23 5.74 4.93 5.74 5.19 5.46 4.65 5.27 5.57 5.12 5.73 5.45
;              5.42 4.05 4.26 4.58 3.94 4.18 4.18 5.89 4.38 4.22 4.42 4.85
;              5.02 4.66 4.66 4.90 4.39 6.05 4.42 5.10 5.22 6.29 4.34 5.62
;              5.10 5.22 5.18 5.57 4.62 5.06 5.34 5.34 5.54 4.98 4.50))
;
; These data should give a Dmin of 0.7324 for b=47, h=24
;    (lts x y 24 :intercept)

(defun lts (x y h &key intercept)
  (flet
    ((get-beta (x y)
      (let ((beta nil))
        (dotimes (i (length x) (sort beta #'<))
          (dotimes (j i nil)
            (cond ((not (equalp 0 (- (elt x i) (elt x j))))
                   (setf beta (cons (/ (- (elt y i) (elt y j))
                                       (- (elt x i) (elt x j))) beta)))
                  ((not (equalp 0 (+ (elt x i) (elt x j))))
                   (setf beta (cons (/ (+ (elt y i) (elt y j))
                                       (+ (elt x i) (elt x j))) beta))))))))
     (anti-rank (x)
      (let ((r (coerce (rank x) 'vector))
            (y (coerce x 'vector))
            (n (length x)))
           (dotimes (i n y)
                    (setf (aref y (aref r i)) i)))))
     (let (beta mid-points d-best a-ranks old-a-ranks slope slope-best)
       (setf beta (get-beta x y))
       (setf mid-points (/ (+ (append beta (+ (last beta) 1))
                              (append (list (- (elt beta 0) 1)) beta))
                           2))
       (setf d-best #.POSITIVE-INFINITY)
       (cond (intercept
              (setf aranks nil)
              (dolist (b mid-points (list d-best slope-best))
                (setf residuals (- y (* x b)))
                (setf old-a-ranks aranks)
                (setf a-ranks (anti-rank (abs residuals)))
                (dotimes (i (- (length x) h) nil)
                  (setf sigma (subseq a-ranks i (+ i h)))
                  (cond ((not (eq old-a-ranks nil))
                         (setf old-sigma (subseq old-a-ranks i (+ i h))))
                        (t
                         (setf old-sigma nil)))
                  (cond ((not (equalp sigma old-sigma))
                         (setf x-m (bind-columns (repeat 1 h) (select x sigma)))
                  (setf y-m (bind-columns (select y sigma)))
                  (setf x-t (transpose x-m))
                  (setf slope (matmult (inverse (matmult x-t x-m)) x-t y-m))
                  (setf d (sum (^ (- y-m (matmult x-m slope)) 2)))
                  (cond ((< d d-best)
                         (setf d-best d)
                         (setf slope-best slope)
                         (setf sigma-best sigma))))))))
             (t
              (dolist (b mid-points (list d-best slope-best))
                (setf residuals (- y (* x b)))
                (setf sigma (subseq (anti-rank (abs residuals)) 0 h))
                (setf x-m (bind-columns (select x sigma)))
                (setf y-m (bind-columns (select y sigma)))
                (setf x-t (transpose x-m))
                (setf slope (matmult (inverse (matmult x-t x-m)) x-t y-m))
                (setf d (sum (^ (- y-m (matmult x-m slope)) 2)))
                (cond ((< d d-best)
                       (setf d-best d)
                       (setf slope-best slope)
                       (setf sigma-best sigma)))))))))

(compile 'lts)