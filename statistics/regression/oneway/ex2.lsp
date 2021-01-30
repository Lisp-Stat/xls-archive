;;;
;;; Example from the Minitab Manual.
;;;
;;;  Flammability data. The length of charred portion of fabric in a
;;;  flammability test was recorded at 5 different labs.  Levels are
;;;  different labs. There are 10 observations in each lab.
;;;


(require "oneway")
(def x '((2.9 3.1 3.1 3.7 3.1 4.2 3.7 3.9 3.1 3.0 2.9)
	 (2.7 3.4 3.6 3.2 4.0 4.1 3.8 3.8 4.3 3.4 3.3)
	 (3.3 3.3 3.5 3.5 2.8 2.8 3.2 2.8 3.8 3.5 3.8)
	 (3.3 3.2 3.4 2.7 2.7 3.3 2.9 3.2 2.9 2.6 2.8)
	 (4.1 4.1 3.7 4.2 3.1 3.5 2.8 3.5 3.7 3.5 3.9)))

(def w (oneway-model x :print nil))

(send w :display)

(send w :all-paired-comparisons)

(send w :individual-ci)  

(send w :boxplots)
