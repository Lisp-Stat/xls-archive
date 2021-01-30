
    (def x(g-normal 10 400))
    (def exp_x1(exp (car x)))
    (def y(+ (abs (car x)) (* .5 (abs (nth 1 x)) exp_x1)))
