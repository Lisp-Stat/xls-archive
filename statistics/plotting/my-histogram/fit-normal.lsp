;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This is inspired by Brown and Hwang, American Statistician, 1993,
;; 251-255, see also, 1994, 353-354. It is far more general, however.
;; It takes a scatterplot-proto, extracts all the linestarts, then
;; reconstructs from this all the line segments in the plot, eliminating
;; the vertical segments in the process. These segments can result from
;; a (normalized) histogram, or from a function plot, or just from
;; an arbitrary bunch of linestarts. Some lines can be horizontal,
;; some can run from right to left. Segments are coded by their
;; endpoints on the horizontal axes, intercepts, and slopes.
;;
;; The program then minimizes
;;
;; sum_{i=1}^n int_{x_i}^{y_i}(a_i+b_i z-cf(z,m,s))^2dz
;;
;; where x_i,y_i are the endpoints of segment i, a_i is the
;; intercept of the correspoding line and b_i is its slope,
;; and f(z,m,s) is the density of a normal with mean m and
;; standard-deviation s.
;;
;; We minimize either over m,s,c (default), i.e. we also
;; scale the fitted density, or we minimize over m,s fixing
;; c at 1. This can be used to fit a normal to a histogram,
;; but also to fit a normal to the plot of a gamma density,
;; and so on.
;;
;; Version 1.0 ** March 23 1995 ** Jan de Leeuw
;; Version 1.1 ** March 26 1995 **
;;                The draw parameter is handled differently.
;;                Made a defun from the defmeth
;;                Returns two lists for :add-lines
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun fit-normal
  (lst &key (mean 0) (sigma 1) (scale t) (draw t) (verbose nil))
"Args: x
Given a list of linestarts, which define a number of
segments, this fits a normal curve to the segments by minimizing mean
square error."
(let ((arg (list mean sigma)))
  (setf arg (newtonmax #'(lambda (arg)
                           (- (normal-fit-loss-function
                               lst (first arg) (second arg) scale))) arg
                               :verbose verbose))
  (if verbose 
      (format t "~,10f ~%"
              (normal-fit-loss-function lst (first arg) (second arg) scale)))
  (let* ((ccc (normal-fit-collect-coefs lst (first arg) (second arg)))
         (c11 (second ccc))
         (c22 (third ccc))
         (srt (mapcar #'first lst))
         (end (mapcar #'second lst))
         (xes (rseq (min srt) (max end) 50)))
    (list xes (mapcar
               #'(lambda (x)
              (let ((fac (if scale (/ c11 c22) 1))
                    (mn (first arg))
                    (sg (second arg)))
                (* fac (/ (normal-dens (/ (- x mn) sg)) sg))
                )) xes)))
  ))

(defmeth scatterplot-proto :convert-linestarts ()
"Args: lines
The list of linestarts is converted to a list of endpoints,
slopes, and intercepts."
(let* ((nn (send self :num-lines))
       (sg nil))
  (dotimes (i nn)
    (let ((j (send self :linestart-next i)))
      (if j (let ((x0 (send self :linestart-coordinate 0 i))
                  (y0 (send self :linestart-coordinate 1 i))
                  (x1 (send self :linestart-coordinate 0 j))
                  (y1 (send self :linestart-coordinate 1 j)))
              (if (/= x0 x1)
                  (let ((a (/ (- (* y0 x1) (* y1 x0))
                              (- x1 x0)))
                        (b (/ (- y1 y0) (- x1 x0))))
                    (setf sg (append sg (list x0 x1 a b)))))))))
  (split-list sg 4)
  ))

(defun normal-fit-collect-coefs (lst mn sg)
  (let* ((srt (mapcar #'first lst))
         (end (mapcar #'second lst))
         (int (mapcar #'third lst))
         (slp (mapcar #'fourth lst))
         (c00 (sum (+ (* (^ slp 2) (/ (- (^ end 3) (^ srt 3)) 3))
                      (* slp int (- (^ end 2) (^ srt 2)))
                      (* (^ int 2) (- end srt)))))
         (df1 (/ (- srt mn) sg))
         (df2 (/ (- end mn) sg))
         (c11 (sum (- (* (+ int (* mn slp))
                         (- (normal-cdf df2)
                            (normal-cdf df1)))
                      (* sg slp
                         (- (normal-dens df2)
                            (normal-dens df1))))))
         (c22 (sum (/ (- (normal-cdf (* df2 (sqrt 2)))
                         (normal-cdf (* df1 (sqrt 2))))
                      (* 2 sg (sqrt pi))))))
    (list c00 c11 c22)
    ))

(defun normal-fit-loss-function (lst mn sg sc)
  (let* ((ccc (normal-fit-collect-coefs lst mn sg))
         (c00 (first ccc))
         (c11 (second ccc))
         (c22 (third ccc)))
    (if sc (- c00 (/ (^ c11 2) c22))
      (- (+ c00 c22) (+ c11 c11)))
    ))

(provide "fit-normal")