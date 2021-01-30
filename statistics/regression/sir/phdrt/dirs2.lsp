
    (def x(g-normal 10 400))
    (def sign_x1(repeat 0 400))
    (dotimes (i 400) (if (>= (nth i (car x)) 0) (setf (nth i sign_x1) 1)
                                                (setf (nth i sign_x1) -1)))
    (def y(+ (abs (car x)) (* .5 (abs (nth 1 x)) sign_x1)))
