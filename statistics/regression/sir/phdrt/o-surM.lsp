    (def x(g-normal 2 350))
    (def y(repeat 0 350))

    (setf (select y (which (>= (car x) 1))) (select

           (+ (nth 1 x) (* (car x) -2) 3)
           (which (>= (car x) 1))))

    (setf (select y (intersection (which (< (car x) 1)) 
                                  (which (>= (car x) 0)))) (select

           (- (+ (* (car x) 2) (nth 1 x)) 1)
           (intersection (which (< (car x) 1))
                                  (which (>= (car x) 0)))))

    (setf (select y (intersection (which (< (car x) 0)) 
                                  (which (>= (car x) -1)))) (select

           (- (nth 1 x) (+ (* (car x) 2) 1))
           (intersection (which (< (car x) 0))
                                  (which (>= (car x) -1)))))

    (setf (select y (which (< (car x) -1))) (select

           (+ (* (car x) 2) (nth 1 x) 3)
           (which (< (car x) -1))))
