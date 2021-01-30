;;;
;;; Example from Devore's book "Probability and Statistics for
;;; Engineering and the Sciences" Third edition, Section 10.3, Problem
;;; #25, Page 395.  

;;; Six different types of diet/imitation margarine were analyzed to
;;; determine the level of phyiologically active polyunsaturated fatty
;;; acids (PAPFUA, in percentages), resulting in the following data.
;;;
;;; B. Narasimhan naras@euler.bd.psu.edu

(require "oneway")

(def x '((14.1 13.6 14.4 14.3) 
	 (12.8 12.5 13.4 13.0 12.3)
	 (13.5 13.4 14.1 14.3)
	 (13.2 12.7 12.6 13.9)
         (16.8 17.2 16.4 17.3 18.0)
	 (18.1 17.2 18.7 18.4)))

(def w (oneway-model x :print nil))

;; Display all the statistics and Tukey Comparisons.
(send w :display)

;;; Display all the Paired CI's.
(send w :all-paired-comparisons)

;; Display all individual CI's.
(send w :individual-ci)
