;;  The ill-conditioned regression "benchmark" dataset of Longley(1967),
;;  J. Amer. Statist. Assoc. 62, 819-841.
;;
;; Fetch softRX shrinkage routines if they haven't already been loaded.
;;

(require "RXridge")

;;
;; Load data columnwise from ASCII format file into the variable longley-dat.
;;

(def longley-dat (read-data-columns "longley.dat" 7))

;;
;; Save regressor/response coordinates in individual variable names.
;;

(def gnpdef  (select longley-dat 0))               ; get single variables
(def gnp     (select longley-dat 1))                 ; out of read data
(def unemp   (select longley-dat 2))
(def afsize  (select longley-dat 3))
(def pop     (select longley-dat 4))
(def year    (select longley-dat 5))
(def emp     (select longley-dat 6))

;;
;; Then construct a XLisp-Stat regression object...
;;

(def longley-reg (regression-model (list gnpdef gnp unemp afsize pop year) emp
             :predictor-names '("GNPDEF" "GNP" "UNEMP" "AFSIZE" "POP" "YEAR" )
             :response-name "EMP"
      )
)

;;
;; Inform user of the NAME of the model object ...so that he/she can send
;; his/her own messages to the object!!!
;;

(format t "(The RXridge regression model object is 'longley-reg') ~%")

;;
;; Create a MENU for the model.
;;

(send longley-reg :RXridge-menu "Longley")
(send longley-reg :RXridge-compute-summary)

;;
;; Everything else can be done interactively with the "Longley" MENU!!!
;;