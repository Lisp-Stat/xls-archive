(require "master.lsp")
(defvar *param-labels* '("Beta(0)" "Beta(2)" "Beta(3)" "Beta(4)" "Beta(5)" "Beta(7)" "Beta(8)" "Beta(9)" "Beta(11)"))
(defvar d-files '("ship1.out" "ship2.out" "ship3.out" "ship4.out"))
(defvar hlist '(((-10 10) (-3 9) (-3 9) (-3 8) (-3 8) (3 9) (3 9) (3 9) (3 8)) ((-10 10) (-3 9) (3 9) (3 8) (3 8) (3 9) (3 9) (3 9) (3 8)) ((5 10) (3 9) (3 9) (3 8) (3 8) (-3 9) (-3 9) (-3 9) (-3 8)) ((5 10) (3 9) (-3 9) (-3 8) (-3 8) (-3 9) (-3 9) (-3 9) (-3 8))))
(defvar rlist '((-10 10) (0 10) (-3 3) (0 9) (-3 3) (0 9) (-3 3) (0 8) (-3 3) (0 8) (-3 3) (0 9) (-3 3) (0 9) (-3 3) (0 9) (-3 3) (0 8)))
(defvar hnames '("Beta(0)-Mean" "Beta(0)-Variance" "Beta(2)-Mean" "Beta(2)-Variance" "Beta(3)-Mean" "Beta(3)-Variance" "Beta(4)-Mean" "Beta(4)-Variance" "Beta(5)-Mean" "Beta(5)-Variance" "Beta(7)-Mean" "Beta(7)-Variance" "Beta(8)-Mean" "Beta(8)-Variance" "Beta(9)-Mean" "Beta(9)-Variance" "Beta(11)-Mean" "Beta(11)-Variance"))
(defvar wlist '(T T NIL NIL NIL NIL NIL T NIL))
(defvar slist '((T T) (T T) (T T) (T T) (T T) (T T) (T T) (T T) (T T)))
(defvar constants '(1.0 .969121 .430443 .448919))
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
