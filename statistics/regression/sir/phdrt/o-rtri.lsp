    (def x(g-normal 2 150))
 
    (setf z1(* (car x) (sqrt 3)))
    (setf z2(* (nth 1 x) (sqrt 3)))

    (def y(repeat 0 150))
    (setf (select y (intersection (which (>= (nth 1 x) 0))
                                  (which (>= (+ z1 (nth 1 x)) 0))))
           (select
           (- 1 (+ (car x) z2))
           (intersection (which (>= (nth 1 x) 0))
                         (which (>= (+ z1 (nth 1 x)) 0)))))

    (setf (select y (intersection (which (< (nth 1 x) 0))
                                  (which (>= (- z1 (nth 1 x)) 0)))) 
           (select
           (- (+ 1 z2) (car x))
           (intersection (which (< (nth 1 x) 0))
                         (which (>= (- z1 (nth 1 x)) 0)))))
           
    (setf (select y(intersection (which (< (+ z1 (nth 1 x)) 0))
                                 (which (< (- z1 (nth 1 x)) 0)))) 
           (select
           (+ (* (car x) 2) 1)
           (intersection (which (< (+ z1 (nth 1 x)) 0))
                         (which (< (- z1 (nth 1 x)) 0)))))
