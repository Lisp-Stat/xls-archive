;;
(provide "kde-opts")

;; These are default values used in the code. All these can be
;;    adjusted using the options list anytime.

;; The default Kernel to use: Gaussian.
(defvar *default-kernel* 'G)

;; The default interval used for slider controlling kernel bin width.
(defvar *default-kernel-window-width-slider-interval* '(0 2)) 

;; The default number of slider stops in that interval.
(defvar *default-kernel-window-width-slider-stops* 51)

;; The default number of points at which the kde is to be computed.
(defvar *default-kernel-no-of-pts* 30)

(defvar *kde-plot-size* '(300 265))
(defvar *kde-code* "kde.o")
