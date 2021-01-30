(def y (list  .750  .770  .840   1.120 1.100 1.120
              .082  .085  .096    .160  .110  .260
              .082  .076  .077    .150  .120  .120
              
              .600  .680  .870    .910  .830  .950
             4.100 5.000 1.800    .660  .830  .610
             1.000 1.800 2.700   2.170 1.520 1.580))

(def filter (repeat (iseq 6) (repeat 6 6)))
(def aerosol (repeat (list -1 -1 -1 1 1 1) 6))
(def manuf (list -1 -1 -1 1 1 1))
(def intercept (repeat 1 36))

;(require "terrace")
;(setf aero (terrace-mlf-model (list intercept aerosol) y filter manuf
;             '((0 1) (0)) '(0)
;             :var-labels '("intercept" "aerosol")))
;
;(setf x (list -1 -1 -1 1 1 1))
;(defun sr (y) (send (regression-model x y) :studentized-residuals))
;(defun cd (y) (send (regression-model x y) :cooks-distances))
 
(load "ter2")
(setf aero (make-terrace (bind-columns (iseq 6) manuf)
                         (bind-columns filter aerosol y)
             :x-labels '("Intercept" "Aerosol")
             :z-labels '("Intercept" "Manufacturer")))
