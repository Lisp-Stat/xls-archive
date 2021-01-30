
    (def x1(normal-rand 150))
    (def x2(normal-rand 150))

    (def x1(+ (* 2 (/ (- x1 (min x1))
                       (- (max x1) (min x1)))) -1))                          
    (def x2(+ (* 2 (/ (- x2 (min x2))
                       (- (max x2) (min x2)))) -1))
;    (def y(+ (bivnorm-cdf x1 x2 0) (* .2 (nth 2 x))))

    (def y(exp (* (+ (^ x1 2) (^ x2 2)) -.5)))

    (spin-plot (list x1 y x2))

