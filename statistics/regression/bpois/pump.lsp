;;;
;;; This example tests our routines on data from 3 MC's.
;;;

(defvar *reweighting-code* '("pump_weights.o" "pump_hmix.o"))
(require "master")

(defvar *param-labels* 
  (list "Theta(1)" "Theta(2)" "Theta(3)" "Theta(4)"
	"Theta(5)" "Theta(6)" "Theta(7)" "Theta(8)" "Theta(9)"
	"Theta(10)" "Alpha" "Beta"))


(def my-files (list "pump1.out" "pump2.out" "pump3.out")) ; data files

(def hlist (list (list (list 50.0) (list 0.5 0.5))
		 (list (list 90.0) (list 0.25 1.5))
		 (list (list 0.5) (list 0.8 0.08))))

(def rlist (list (list 1 100) (list 0 1) (list 0 1)))

(def hnames (list "Alpha-Mean" "Beta-Shape" "Beta-Scale"))

(def wlist (list t nil nil nil nil nil nil nil nil nil 
		 t t))
(def constants '(1 0.31198 10.8124))
(def init '(1.0 0.1 1.0))

(setf pump-obj (send master-proto :new 
		     :data my-files
                     :constants constants
		     :p 12 :initial-hyper-vals init
		     :hyper-param-names hnames
		     :identifier "Pump Data"
		     :hyper-vals hlist
		     :hyper-range-list rlist
		     :create-window-list wlist))


