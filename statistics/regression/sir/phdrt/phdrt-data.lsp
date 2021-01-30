(send Open-Data-menu-item :enabled T)

(def data-load(1+ (choose-item-dialog
                   "Which Data Set do you want to use:"
                   '("Baseball."
                     "Ozone."
                     "Boston Housing."
                     "Cars."
                     "Simu-osin2b."
                     "Simu-rtri."
                     "Simu-hinge."
                     "Baseball All."
                     "Boston All."
                     "Simu-multi-dirs."
                             ))))

 (when (= data-load 1)  (load "baseball-1set")
                        (def data-file "Baseball"))
 (when (= data-load 2)  (load "ozset")
                        (def data-file "Ozone"))
 (when (= data-load 3)  (load "boston-2set")
                        (def data-file "Boston"))
 (when (= data-load 4)  (load "mpgset")
                        (def data-file "Cars"))
 (when (= data-load 5)  (load "osin2bx") (load "osin2by")
                        (def data-file "Osin2b"))
 (when (= data-load 6)  (load "rtrix") (load "rtriy")
                        (def data-file "r-tri"))
 (when (= data-load 7)  (load "o-hinge")
                        (def data-file "o-hinge"))
 (when (= data-load 8)  (load "baseball-all")
                        (def data-file "Baseball-all"))
 (when (= data-load 9)  (load "boston-all")
                        (def data-file "Boston-all"))
 (when (= data-load 10) (load "dirs2-1x") (load "dirs2-1y")
                        (def data-file "multi-dirs"))

(send IDA-menu-item :enabled T)
(send Regression-menu-item :enabled T)
(message-dialog "The data set has been loaded")

