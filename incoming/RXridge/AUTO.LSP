;; Data from Henderson and Velleman (1981), "Building Regression Models
;;          Interactively", Biometrics 37, 391-411. (Table 3, page 400.)
;;
;; Characteristics of 38 1978-79 model automobiles; data collected from
;;                                                  Consumer Reports.
;; Note: Observations ordered here by predicted GPHM at (M=0.25;Q=0.0).
;;
;;  CYLNDS = number of cylinders
;;  CUBINS = cubic inches of engine displacement
;;  OVRP   = horsepower * 100 / weight = measure of extent overpowered
;;  WEIGHT = total auto weight in pounds
;;  GPHM   = gallons per hundred miles
;;  COUNTRY= country of manufacture (string of 4 characters)
;;  CARID  = car identification (variable length string)
;;
;; Fetch softRX shrinkage routines if they haven't already been loaded.
;;

(require "RXridge")

;;
;; Load data columnwise from ASCII format file into the variable auto-dat.
;;

(def auto-dat (read-data-columns "auto.dat" 7))

;;
;; Save regressor/response coordinates in individual variable names.
;;

(def cyl     (select auto-dat 0))               ; get single variables
(def cub     (select auto-dat 1))               ; out of read data
(def ovrp    (select auto-dat 2))
(def wght    (select auto-dat 3))
(def gphm    (select auto-dat 4))

;;
;; Then construct a XLisp-Stat regression object...
;;

(def auto-reg (regression-model (list cyl cub ovrp wght) gphm
                               :predictor-names '("CYL" "CUB" "OVRP" "WGHT" )
                               :response-name "GPHM"
             )
)

;;
;; Inform user of the NAME of the model object ...so that he/she can send
;; his/her own messages to the object!!!
;;

(format t "(The RXridge regression model object is 'auto-reg') ~%")

;;
;; Create a MENU for the model.
;;

(send auto-reg :RXridge-menu "AUTO")
(send auto-reg :RXridge-compute-summary)

;;
;; Everything else can now be done interactively with the "AUTO" MENU!!!
;;