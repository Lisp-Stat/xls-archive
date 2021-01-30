;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                        ;;
;;                      pHdrt Menu                        ;;
;;                                                        ;;
;;                  Heng-hui Lue, 1/1/94                  ;;
;;                                                        ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(undef '(pHdrt-menu))
(defvar pHdrt-menu (send menu-proto :new "pHdrt"))

(setf Open-Data-menu-item 
      (send menu-item-proto :new "Open Data" 
           :action #'(lambda () (load "phdrt-data.lsp")) ))

(setf IDA-menu-item
      (send menu-item-proto :new "IDA-options" 
           :action #'(lambda () (load "phdrt-ida.lsp"))
           :enabled nil))

(setf Regression-menu-item
      (send menu-item-proto :new "Regression-Tree" 
           :action #'(lambda () (load "phdrt-run.lsp"))
           :enabled nil))

(setf DASH1-menu-item-pHdrt
      (send dash-item-proto :new))

(setf DASH2-menu-item-pHdrt
      (send dash-item-proto :new))

(setf DASH3-menu-item-pHdrt
      (send dash-item-proto :new))

(setf DASH4-menu-item-pHdrt
      (send dash-item-proto :new))

(setf CAP-menu-item-pHdrt
      (send menu-item-proto
            :new "Close All Plots"
            :action #'(lambda () (close-all-plots))
            :enabled nil))

(setf CONT-menu-item-pHdrt
      (send menu-item-proto
            :new "Continue"
            :action #'(lambda () (continue))
            :enabled t))

(setf Remove-menu-item-pHdrt
      (send menu-item-proto 
            :new "Remove Menu"
            :action #'(lambda ()
                        (send pHdrt-menu :delete-items
                              Open-Data-menu-item
                              DASH1-menu-item-pHdrt
                              IDA-menu-item
                              DASH2-menu-item-pHdrt
                              Regression-menu-item
                              DASH3-menu-item-pHdrt
                              CONT-menu-item-pHdrt
                              DASH4-menu-item-pHdrt
                              CAP-menu-item-pHdrt
                              Remove-menu-item-pHdrt
                              )
                        (send pHdrt-menu :remove))))

                        (send pHdrt-menu :append-items
                              Open-Data-menu-item
                              DASH1-menu-item-pHdrt
                              IDA-menu-item
                              DASH2-menu-item-pHdrt
                              Regression-menu-item
                              DASH3-menu-item-pHdrt
                              CONT-menu-item-pHdrt
                              DASH4-menu-item-pHdrt
                              CAP-menu-item-pHdrt
                              Remove-menu-item-pHdrt
        )

(send pHdrt-menu :install)
;(load "SPScreenType.lsp")
(expand 100)
(load "sir-program")
(load "phd-model")
(load "phd-sub1")
(load "phdrt-model1")
(load "reg-model")
(load "regression-tree-auto")
(load "regression-tree-int1")
(require "information-model")
(require "phd-class")
(require "tree")
;(require "smooth")
(require "class-output")
(require "Lue-graph")
(require "plot-tools")
(require "tour-plot")
(require "plot-axis-rotate")
(require "pca-plot")
