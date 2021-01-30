
    (def x(g-uniform 10 400))

    (dotimes (i 10) (setf (nth i x) (- (nth i x) .5)))

    (setf z1(+ (car x) (nth 1 x)))
 
    (setf z2(+ (nth 2 x) (nth 3 x) (nth 4 x)))
    
    (def y(* z1 (sin (* z2 2))))
