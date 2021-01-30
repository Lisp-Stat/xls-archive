;;;
;;; This example runs the reweighting mixtures method on data from
;;; four Markov Chains.  The data is Ship Data from McCullagh and Nelder.
;;;

(require "master")

(defvar *param-labels* '("Beta(0)" "Beta(2)" "Beta(3)" 
			 "Beta(4)" "Beta(5)" "Beta(7)"
			 "Beta(8)" "Beta(9)" "Beta(11)"))

(def d-files '("ship1.out" "ship2.out" 
	       "ship3.out" "ship4.out"))

(def hlist '(((-10 10) (-3 9) (-3 9)
	      (-3 8) (-3 8) (3 9)
	      (3 9) (3 9) (3 8))
	     ((-10 10) (-3 9) (3 9)
	      (3 8) (3 8) (3 9)
	      (3 9) (3 9) (3 8))
	     ((5.0 10) (3 9) (3 9)
	      (3 8) (3 8) (-3 9)
	      (-3 9) (-3 9) (-3 8))
	     ((5.0 10) (3 9) (-3 9)
	      (-3 8) (-3 8) (-3 9)
	      (-3 9) (-3 9) (-3 8))))

(def rlist '((-10 10) (0 10) 
	     (-3 3) (0 9)
	     (-3 3) (0 9)
	     (-3 3) (0 8)
	     (-3 3) (0 8)
	     (-3 3) (0 9)
	     (-3 3) (0 9)
	     (-3 3) (0 9)
	     (-3 3) (0 8)))

(def hnames '("Beta( 0)-Mean" "Beta( 0)-Variance" 
	      "Beta( 2)-Mean" "Beta( 2)-Variance" 
	      "Beta( 3)-Mean" "Beta( 3)-Variance"
	      "Beta( 4)-Mean" "Beta( 4)-Variance"
	      "Beta( 5)-Mean" "Beta( 5)-Variance"
	      "Beta( 7)-Mean" "Beta( 6)-Variance" 
	      "Beta( 8)-Mean" "Beta( 8)-Variance"
	      "Beta( 9)-Mean" "Beta( 9)-Variance"
	      "Beta(11)-Mean" "Beta(11)-Variance"))

(def wlist '(t t nil nil nil nil nil t nil))
(def slist '((t t) (t t) nil nil nil nil nil nil 
	       nil nil nil nil (t t) nil nil))

(def constants '(1.0 0.9691209006034817719 
                0.43044277534447189515 0.44891891242724912825))

(debug)
(setf posterior (send master-proto :new 
		     :data d-files
                     :constants constants
		     :p 9
		     :hyper-param-names hnames
		     :identifier "ShipData"
		     :hyper-vals hlist
		     :hyper-range-list rlist
		     :hyper-show-list slist
		     :create-window-list wlist))


