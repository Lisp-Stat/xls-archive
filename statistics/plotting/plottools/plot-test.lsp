(def x1 (normal-rand 50))

(def x2 (normal-rand 50))

(def x3 (normal-rand 50))

(def p1 (histogram x1
                   :location '(10 40)
                   :size '(220 120)
                  ))

(def p2 (plot-points x1 x2
                     :location '(10 190)
                     :size '(220 220)
                     ))

(def p3 (spin-plot (list x1 x2 x3)
                   :location '(250 280)
                   :size '(220 220)
                    ))

(def p4 (scatterplot-matrix (list x1 x2 x3)
                            :location '(250 40)
                            :size '(220 220)
                            ))

(send p1 :linked t)
(send p2 :linked t)
(send p3 :linked t)
(send p4 :linked t)

