
    (def x(g-normal 10 400))

    (def e(normal-rand 400))

    (def y(repeat 0 400))

    (setf (select y (which (>= (car x) 1))) (select

           (+ (nth 1 x) (* e .5))

           (which (>= (car x) 1))))

    (setf (select y (which (< (car x) 1))) (select

           (- (+ (nth 1 x) 2 (* e .5)) (* (car x) 2))

           (which (< (car x) 1))))

