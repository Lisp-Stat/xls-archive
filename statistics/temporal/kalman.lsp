;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This is a VERY preliminary implementation of the Kalman filter. Its
;; numerical properties are probably not too good. It does cover both
;; the stationary and nonstationary case. In general
;;       z_t = F_tz_{t-1} + G_te_t
;;       y_t = H_tz_t + d_t 
;; where F_t,G_t,H_t and the dispersion matrices R and Q of d_t and e_t
;; are supposed to be known. The filter then estimates the z_t, starting
;; at z_0.
;;
;; Version 0.1 -- 30/08/95 -- Jan de Leeuw
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun kalman-filter 
  (f g h y r q dim 
     &key (z0 (repeat 0 dim)) (p0 (identity-matrix dim)) (stationary t))
  (let ((pkk p0)
        (zkk z0))    
    (dotimes (k kk)
             (let* ((fk (if stationary f (elt f k)))
                    (gk (if stationary g (elt g k)))
                    (hk (if stationary h (elt h k)))
                    (yk (elt y k))
                    (zkk- (matmult fk zkk))
                    (pkk- (+ (matmult (matmult fk pkk) (transpose fk))
                             (matmult (matmult gk q) (transpose gk))))
                    (vk (- yk (matmult hk zkk-)))
                    (ck (+ (matmult (matmult hk pkk-) (transpose hk)) r))
                    (kk (matmult (matmult pkk- (transpose hk)) 
                                 (inverse ck))))
               (setf zkk (+ zkk- (matmult kk vk)))
               (setf pkk (- pkk- (matmult (matmult  kk hk) pkk-)))
               )
             )
    )
  )