;;***************************************************************************
;;
;; SIRCT Menu
;;
;; Chun-houh Chen, 1/1/91
;;

(undef '(SIRCT-menu))
(defvar SIRCT-menu (send menu-proto :new "SIRCT"))

(setf open-data-menu-item 
      (send menu-item-proto :new "Open Data" 
           :action #'(lambda () (load "SIRCT-Data.lsp"))
))

(setf IDA-menu-item
      (send menu-item-proto :new "IDA-options" 
           :action #'(lambda () (load "SIRCT-IDA.lsp"))
           :enabled nil
))

(setf SIR-menu-item
      (send menu-item-proto :new "SIR-model"
           :action #'(lambda () (load "SIR-run.lsp"))
           :enabled nil
))

(setf SIR-II-menu-item
      (send menu-item-proto :new "SIR-II-model" 
           :action #'(lambda () (load "SIR-II-run.lsp"))
           :enabled nil
))

(setf SIR-I-II-menu-item
      (send menu-item-proto :new "SIR-I-II-model" 
           :action #'(lambda () (load "SIR-I-II-run.lsp"))
           :enabled nil
))

(setf Linear-menu-item
      (send menu-item-proto :new "Linear Disc" 
           :action #'(lambda () (load "Linear-run.lsp"))
           :enabled nil
))

(setf Quadratic-menu-item
      (send menu-item-proto :new "Quadratic Disc" 
           :action #'(lambda () (load "Quadratic-run.lsp"))
           :enabled nil
))

(setf Classification-menu-item
      (send menu-item-proto :new "Tree Classifier" 
           :action #'(lambda () (load "SIRCT-run.lsp"))
           :enabled nil
))

(setf DASH1-menu-item-SIRCT
      (send dash-item-proto :new)
)
(setf DASH2-menu-item-SIRCT
      (send dash-item-proto :new)
)
(setf DASH3-menu-item-SIRCT
      (send dash-item-proto :new)
)
(setf DASH4-menu-item-SIRCT
      (send dash-item-proto :new)
)

(setf DASH5-menu-item-SIRCT
      (send dash-item-proto :new)
)

(setf CAP-menu-item-SIRCT
      (send menu-item-proto
            :new "Close All Plots"
            :action #'(lambda () (close-all-plots))
            :enabled nil
))

(setf CONT-menu-item-SIRCT
      (send menu-item-proto
            :new "Continue"
            :action #'(lambda () (continue))
            :enabled t
))

(setf Remove-menu-item-SIRCT
      (send menu-item-proto 
            :new "Remove Menu"
            :action #'(lambda ()
                        (send SIRCT-menu :delete-items
                              open-data-menu-item
                              DASH1-menu-item-SIRCT
                              IDA-menu-item
                              DASH2-menu-item-SIRCT
                              SIR-menu-item
                              SIR-II-menu-item
                              SIR-I-II-menu-item
                              DASH3-menu-item-SIRCT
                              Linear-menu-item
                              Quadratic-menu-item
                              Classification-menu-item
                              DASH4-menu-item-SIRCT
                              CONT-menu-item-SIRCT
                              DASH5-menu-item-SIRCT
                              CAP-menu-item-SIRCT
                              Remove-menu-item-SIRCT
                              )
                        (send SIRCT-menu :remove))))

(send SIRCT-menu :append-items
      open-data-menu-item
      DASH1-menu-item-SIRCT
      IDA-menu-item
      DASH2-menu-item-SIRCT
      SIR-menu-item
      SIR-II-menu-item
      SIR-I-II-menu-item
      DASH3-menu-item-SIRCT
      Linear-menu-item
      Quadratic-menu-item
      Classification-menu-item
      DASH4-menu-item-SIRCT
      CONT-menu-item-SIRCT
      DASH5-menu-item-SIRCT
      CAP-menu-item-SIRCT
      Remove-menu-item-SIRCT
)

(send SIRCT-menu :install)

(load "SPScreenType.lsp")
(load "sir-program")
(load "classification")
(load "classification-auto")
(load "classification-inter")
(require "information-model")
(require "information-model-1")
(require "sir-I-II-class")
(require "sir-class-I-II-model")
(require "easy-lda")
(require "easy-qda")
(require "tree")
(require "class-out-put")
(require "plot-tools")
(require "hand-axis-rotate")
(require "Chen-graph")
(require "parallel-plot")
(require "pca-plot")

