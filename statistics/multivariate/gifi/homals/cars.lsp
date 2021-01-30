;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This is an example of a multivariable-proto, it can also be used by
;; homals.lsp
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def body-style
     '("0" "0" "2" "1" "0" "0" "0" "0" "1" "1" "0" "0"
       "1" "0" "0" "1" "2" "2" "0" "0" "0" "0" "1" "0"))
(def driver-protection
     '("0" "1" "4" "3" "0" "2" "1" "2" "2" "4" "3" "1"
       "3" "2" "3" "4" "3" "4" "2" "0" "0" "2" "2" "4"))
(def passenger-protection
     '("0" "0" "1" "1" "0" "1" "0" "2" "0" "2" "2" "0"
       "1" "0" "0" "1" "0" "1" "1" "0" "0" "0" "0" "0"))
(def structural-integrity
     '("1" "1" "1" "0" "0" "1" "2" "1" "1" "1" "0" "0"
       "0" "0" "1" "1" "0" "1" "1" "2" "0" "0" "1" "2"))

(def cars (transpose
           (make-array '(4 24) :initial-contents 
                       (list body-style driver-protection 
                             passenger-protection structural-integrity))))

(def variable-labels '("body-style"
                       "driver-protection"
                       "passenger-protection"
                       "structural-integrity"))

(def category-labels '(("2-door" "4-door" "wagon")
                       ("no injury" "moderate injury" "certain injury"
                        "severe injury" "fatal injury")
                       ("no injury" "moderate injury" "certain injury"
                        "severe injury" "fatal injury")
                        ("much better" "better" "average")))

(def object-labels '("acura integra"	
                     "daihatsu charade"	
                     "dodge colt"	
                     "eagle summit"	 			
                     "ford escort"						
                     "ford festiva"	
                     "honda civic"
                     "hyundai excel"
                     "hyundai excel"
                     "isuzu i-mark"
                     "mazda 323"
                     "mazda rx-7"
                     "mitsubishi mirage"
                     "mitsubishi starion"
                     "nissan pulsar nx"
                     "nissan sentra"
                     "nissan sentra"
                     "plymouth colt"
                     "pontiac lemans"
                     "subaru justy"
                     "toyota celica"
                     "toyota tercel"
                     "volkswagon golf"
                     "yugo gv"))