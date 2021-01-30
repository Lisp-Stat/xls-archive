

#|
Here're two possibilities.

1. I wrote some smoothing code in lisp which I think is basically what
you'd do if you if the lisp code is fast enough.  It's simple code for
computing the convolution of a set of data points against a kernel.
I'm appending it to the end of this message.  This code is *very*
slow, but easy to use.  It also can be modified to be much faster in
special cases.  For example, if your data is 1 dimensional, and you're
using a box shaped kernel, you can sort the data first, and only work
with the points within the box.

2. If you're running xlispstat on a platform which supports dynamic
linking and loading of object code (running under Unix, say),
you could modify the above to work with kernels coded in Fortran (for
example).  Then, you could code up the kernels in Fortran, link them
on the fly into a running interpreter, and let the lisp code use the
Fortran kernels.  I can send you some modifications to xsdynload.c to
make this a little faster and easier, if your interested in this
route.  It gave me a factor of 10 speedup when I used this approach
with a nonlinear regression.

Good luck,

Dr. Harvey J. Stein
Berger Financial Research
abel@netvision.net.il
|#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Simple convolution code.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Copyright (c) 1996, Harvey J. Stein <abel@netvision.net.il>
;;; Under GNU GPL
;;;
;;; Usage:
;;; A data point is a list of numbers of length N.  Let DP be a list of
;;; data points, FV be a list of function values at the data points, and
;;; F be a function of N variables (to be used as a kernel function).
;;; Let SF be (MAKE-DATA-SMOOTHER (transpose DP) FV F).  Then SF is also a
;;; function of N variables.  Its value at a point X is (please excuse
;;; the mixed notation):
;;;
;;;    (length DP)-1
;;;       SUM       FV_i * F(DP_i - X)
;;;       i=0
;;;    -------------------------------
;;;    (length DP)-1
;;;       SUM       F(DP_i - X)
;;;       i=0
;;;
;;; The routines BOX-KERNEL and BVN-KERNEL generate commonly used
;;; kernel functions.  (BOX-KERNEL da db) returns a function which is
;;; 1   -da < x < da and -db < y < db, and 1 outside.  (BVN-KERNEL sa sb)
;;; returns a bivariate normal function with standard deviations sa &
;;; sb & rho = 0.
;;;
;;;
;;; So, for example, one could do the following:
;;;
;;; 1. Make a random array of x & y coordinates ranging from -3 to 3.
;;;
;;;   (setq x (* 6 (- (uniform-rand 300) .5)))
;;;   (setq y (* 6 (- (uniform-rand 300) .5)))
;;;
;;; 2. Let the function values at the points be the function
;;; x^2+y^2 + normally distributed noise.
;;;
;;;   (setq FV (+ (mapcar (lambda (x y) (+ (* x x) (* y y)))
;;;                       x y)
;;;               (normal-rand 300)))
;;;
;;; 3. Smooth the data by averaging points in 2x2 squares (i.e. - do a
;;; convolution with the fcn that's 1 for x & y btw -1 & 1, and 0
;;; elsewhere.
;;;
;;;    (setq F (make-data-smoother (list x y) FV (box-kernel 1 1)))
;;;
;;; 4. Plot the results.
;;;
;;;      The data (including noise)
;;;    (setq p (spin-plot (list x y fv)))
;;;
;;;      The original function
;;;    (send p :add-function (lambda (x y) (+ (* x x) (* y y))) -3 3 -3 3)
;;;
;;;      The smoothed function
;;;    (send p :add-function F -3 3 -3 3)


;;; The following 3 functions should be compiled before being used.
(defun make-data-smoother (predict response kernel)
  "Args: (predict response kernel)
Returns smoother fcn f of (length predict) args s.t. f(x,y,...)
is kernel convoluted with the specified data points."
  (lambda (&rest l)
    (let* ((w (apply #'mapcar
                     (lambda (&rest p) (apply kernel (- l p)))
                     predict))
           (tw (sum w)))
      (if (< (abs tw) 1d-30)            ; Avoid division by zero...
          0.0
        (/ (sum (* response w))
           tw)))))

(defun box-kernel (da db)
  (let ((mda (- da))
        (mdb (- db)))
    (lambda (x y) (if (and (< mda x da) (< mdb y db))
                      1
                    0))))

(defun bvn-kernel (s1 s2)
  (let ((scale (/ 1 2 pi s1 s2)))
    (lambda (x y)
      (let ((xmu (/ x s1))
            (ymu (/ y s2)))
        (* scale (exp (/ (+ (* xmu xmu) (* ymu ymu))
                         -2)))))))


