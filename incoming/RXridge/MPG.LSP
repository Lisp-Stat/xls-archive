;; Data on 32 automobiles from Motor Trends magazine used by
;; Hocking(1976), "The Analysis and Selection of Variables in
;; Regression," Biometrics 32, 1-51.
;;
;; Fetch softRX shrinkage routines if they haven't already been loaded.
;;

(require "RXridge")

;;
;; Load data columnwise from ASCII format file into the variable mpg-dat.
;;

(def mpg-dat (read-data-columns "mpg.dat" 6))

;;
;; Save regressor/response coordinates in individual variable names.
;;

(def cyl     (select mpg-dat 0))               ; get single variables
(def cub     (select mpg-dat 1))               ; out of read data
(def hp      (select mpg-dat 2))
(def wght    (select mpg-dat 3))
(def mpg     (select mpg-dat 4))

;;
;; Then construct a XLisp-Stat regression object...
;;

(def mpg-reg (regression-model (list cyl cub hp wght) mpg
                               :predictor-names '("CYL" "CUB" "HP" "WGHT" )
                               :response-name "MPG"
             )
)

;;
;; Inform user of the NAME of the model object ...so that he/she can send
;; his/her own messages to the object!!!
;;

(format t "(The RXridge regression model object is 'mpg-reg') ~%")

;;
;; Create a MENU for the model.
;;

(send mpg-reg :RXridge-menu "MPG")
(send mpg-reg :RXridge-compute-summary)

;;
;; Everything else can be done interactively with the "MPG" MENU!!!
;;
