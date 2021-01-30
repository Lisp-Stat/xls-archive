(dyn-load "./libzero.so")

(defun f1 (x) (- (* x (- (* x x) 2)) 5))
(def r (call-lfun "fminbr" 0 1 #'f1))
(format t "A min of (x^3 - 2x - 5)^2  in [0, 1] is ~g~%~%" r)

(defun f1prime (x) (^ (f1 x) 2))
(def r (call-lfun "fminbr" 2 3 #'f1prime))
(format t "A min of (x^3 - 2x - 5)^2  in [2, 3] is ~g~%~%" r)

(defun f2 (x) (- (^ (- (cos x) x) 2) 2))
(def r (call-lfun "fminbr" 2 3 #'f2))
(format t "A min of (cos(x) - x)^2-2 in [2, 3] is ~g~%~%" r)

(def r (call-lfun "fminbr" -1 3 #'f2))
(format t "A min of (cos(x) - x)^2-2 in [-1, 3] is ~g~%~%" r)

(defun f3 (x) (1+ (^ (- (sin x) x) 2)))
(def r (call-lfun "fminbr" -1 3 #'f3))
(format t "A min of (sin(x) - x)^2+1 in [-1, 3] is ~g~%~%" r)


