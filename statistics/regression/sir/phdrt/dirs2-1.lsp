
    (def x(g-normal 10 400))

    (def sign_x1(repeat 0 400))

    (dotimes (i 400) (if (>= (nth i (car x)) 0) (setf (nth i sign_x1) 1)
                                                (setf (nth i sign_x1) -1)))
    (def mean_absx2(mean (abs (nth 1 x))))

    (def y(+ (abs (car x)) (* .5 (- (abs (nth 1 x)) mean_absx2) sign_x1)))
