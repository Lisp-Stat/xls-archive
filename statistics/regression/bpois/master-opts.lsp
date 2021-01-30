(provide "master-opts")

(defvar *hyper-val-steps* 0.05) ; Used in constructing ranges 
(defvar *hyper-val-stops* 101)   ;   for hyper-parameters.

(defvar *default-val-width* 5) ; How wide a field for the hyper-param value to be displayed?
(defvar *weight-watcher* nil)

(defvar *reweighting-code* '("ship_weights.o" "ship_hmix.o"))
