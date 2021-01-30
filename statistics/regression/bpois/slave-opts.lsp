;;
(provide "slave-opts")

;; These are default values used in the code. All these can be
;;    adjusted using the options list anytime.

;; What statistics must be dynamically calculated. 
;;   Default: Mean and standard deviation.
;;   This also means that the code for calculating these should be 
;;   written for each such statistic.
;;(defvar *stats-to-show* (list "mean" "stddev" "mean-sq"))
(defvar *stats-to-show* (list "mean" "stddev"))

;; The formats using which the stats are to be displayed. 
;; Each sublist consists of two items, the field width and the 
;; number of places after the decimal point.
;;(defvar *stats-print-formats* (list (list 6 3) (list 6 3) (list 6 3)))
(defvar *stats-print-formats* (list (list 6 3) (list 6 3)))


