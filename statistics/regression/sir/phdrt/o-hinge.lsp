
    (def x(g-normal 10 400))

    (def y(repeat 0 400))

    (setf (select y (which (>= (car x) .25))) (select

           (nth 1 x)

           (which (>= (car x) .25))))

    (setf (select y (which (< (car x) .25))) (select

           (- (+ (nth 1 x) .5) (* (car x) 2))

           (which (< (car x) .25))))

