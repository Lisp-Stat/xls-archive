(defun rsmooth-2d (i j x y z b1 b2)
;;; This is a function to kernel-smooth values z at a
;;; particular gridpoint (i,j).
;;; z should be observed over a grid of values x and y.
;;; To make this a density estimate just let z equal:
;;;   (repeat (/ 1 (length x)) (length x)).
;;; The smoothing is done using a Gaussian low-pass filter.
;;; x, y, and z should be lists of equal length, but need not be ordered.
;;; (b1,b2) is the 2-d bandwidth used.
;;;     If x and y belong to the same metric space
;;;     it may be desirable to set b1 = b2.
;;; This function could be improved by adding different kernels.
        (let* ((w (/ (* (exp (/ (- (* (- x i) (- x i))) (* 2 b1 b1)))
                         (exp (/ (- (* (- y j) (- y j))) (* 2 b2 b2))))
                      (* 2 pi b1 b2)))  ;;; w = weights
                )
(/ (sum (* z w)) (sum w))
))

(defun rcontour (x y z
        &key
        (xnum 5) (ynum 5)
        (levels 10)
        (x1 (+ (min x) (/ (* .5 (- (max x) (min x))) xnum)))
        (x2 (- (max x) (/ (* .5 (- (max x) (min x))) xnum)))
        (y1 (+ (min y) (/ (* .5 (- (max y) (min y))) ynum)))
        (y2 (- (max y) (/ (* .5 (- (max y) (min y))) ynum)))
        (b1 (/ (- x2 x1) (log (length z)) 2))
        (b2 (/ (- y2 y1) (log (length z)) 2)))
;;; This is a function to kernel-smooth values z observed over
;;; a grid of values x and y.  The smoothing is done using a
;;; Gaussian low-pass filter.
;;; x, y, and z should be lists of equal length, but need not be ordered.
;;; The smoothed z is computed over an xnum by xnum grid;
;;;     by default xnum=5.
;;; The smoothed z is computed from x1 to x2 and from y1 to y2;
;;;     by default x1 = min(x) + .5(max(x)-min(x))/xnum and
;;;     x2 = max(x) - .5(max(x)-min(x))/xnum.  same for y.
;;; (b1,b2) is the 2-d bandwidth used;
;;;     by default b1 = (x2-x1)/[2log(n)] and b2 = (y2-y1)/[2log(n)],
;;;     where n = length(z).
;;; This function could be improved by letting the user call different
;;; smoothers.
;;; Unfortunately the function is very slow, requiring
;;; a pass through the data for each gridpoint.
;;; I see no easy way to remedy this.
   (defun contoursmooth (i j) (rsmooth-2d i j x y z b1 b2))
   (contour-function 'contoursmooth x1 x2 y1 y2 :levels levels :num-points xnum))

;;;;;;;;;;;;;;;
;;; example ;;;
;;;;;;;;;;;;;;;

(def x (* (uniform-rand 300) 10))
(def y (* (uniform-rand 300) 10))
(def z (+ (* (- x 4) (- x 4)) (* (- y 5) (- y 5))))
;;; z = (x-3)^2 + (y-5)^2
(rcontour x y z :xnum 10 :ynum 10 :levels 10)
