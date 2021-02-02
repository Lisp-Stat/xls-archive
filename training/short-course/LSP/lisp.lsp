(new-section "Lisp" "lisp.scr")

(def times '(6 6 6 6 7 9 10 10 11 13 16 17 19 20 22 23 25 32 32 34 35
             1 1 2 2 3 4  4  5  5  8  8  8  8 11 11 12 12 15 17 22 23))
(def status '(0 1 1 1 1 0 0 1 0 1 1 0 0 0 1 1 0 0 0 0 0
             1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1))
(def x (bind-columns (repeat 1 42) (repeat '(0 1) '(21 21))))
