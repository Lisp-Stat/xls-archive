#|

ex-dlm.lsp

A simple example to illustrate the use of BAYES-LIN.
See the BAYES-LIN documentation for an explanation.

|# 

;; First make sure BAYES-LIN is loaded
(require "bayeslin")

;; Put a junction tree together
(create-tree-node 'tX1 '(t1 x1) #(1 1) #2a((1 1) (1 2)) '(tT12))
(create-tree-node 'tX2 '(t2 x2) #(1 1) #2a((2 2) (2 3)) '(tT23))
(create-tree-node 'tX3 '(t3 x3) #(1 1) #2a((3 3) (3 4)) '(tT23))
(create-tree-node 'tT12 '(t1 t2) #(1 1) #2a((1 1) (1 2)) '(tX1 tT23))
(create-tree-node 'tT23 '(t2 t3) #(1 1) #2a((2 2) (2 3)) '(tT12 tX2 tX3))

;; Create some moral graph nodes
(create-moral-node 'mX1 '(x1) 'tX1 "mX1" '(mT1))
(create-moral-node 'mX2 '(x2) 'tX2 "mX2" '(mT2))
(create-moral-node 'mX3 '(x3) 'tX3 "mX3" '(mT3))
(create-moral-node 'mT1 '(t1) 'tX1 "mT1" '(mX1 mT2))
(create-moral-node 'mT2 '(t2) 'tX2 "mT2" '(mX2 mT1 mT3))
(create-moral-node 'mT3 '(t3) 'tX3 "mT3" '(mX3 mT2))

;; Plot locations for the nodes of the moral graph
(send mX1 :location '(0.2 0.8))
(send mX2 :location '(0.5 0.8))
(send mX3 :location '(0.8 0.8))
(send mT1 :location '(0.2 0.2))
(send mT2 :location '(0.5 0.2))
(send mT3 :location '(0.8 0.2))

;; Create a plot object
(create-moral-plot 'myplot)
(create-global-moral-plot 'myplot2)

;; Make some observations
(send mx1 :observe '(x1) #(3))
(send myplot :record)
(print (send mx1 :ex))
(print (send mx1 :var))
(print (send mx1 :aex))
(print (send mx1 :avar))
(print (send mx3 :ex))
(print (send mx3 :var))
(print (send mx3 :aex))
(print (send mx3 :avar))
(send mx1 :absorb)
(print (send mx3 :ex))
(print (send mx3 :var))

(send mx2 :observe '(x2) #(-1))
(send myplot :record)
(send mx2 :absorb)

(print (send mx3 :var))
(print (send mx3 :ex))
(terpri)


;; end

