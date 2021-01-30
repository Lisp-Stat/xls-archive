(dyn-load "./libzero.so")

(defun f1 (x) (- (* x (- (* x x) 2)) 5))

(def r (call-lfun "zeroin" 2 3 #'f1))
(format t "A root of x^3 - 2x - 5 in [2, 3] is ~g~%~%" r)

(defun f2 (x) (- (cos x) x))
(def r (call-lfun "zeroin" 2 3 #'f2))
(format t "A root of cos(x) - x in [2, 3] is ~g~%~%" r)

(def r (call-lfun "zeroin" -1 3 #'f2))
(format t "A root of cos(x) - x in [-1, 3] is ~g~%~%" r)

(defun f3 (x) (- (sin x) x))
(def r (call-lfun "zeroin" -1 3 #'f3))
(format t "A root of sin(x) - x in [-1, 3] is ~g~%~%" r)


