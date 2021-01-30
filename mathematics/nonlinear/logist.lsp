(defun logist ()
"Plots the attractors of the logistic iteration over the
parameter range 2.9-4.0."
(let (
     (arg (rseq 2.9 4.0 100))
     (plt (plot-points nil nil))
     )
(dotimes (i 100)
     (let* (
           (a (elt arg i))
           (y (l-attract a))
           (x (repeat a 100))
           )
     (send plt :add-points x y)
     (send plt :adjust-to-data)
     )
)))

(defun l-attract (a)
(let (
     (xlist nil)
     (x (uniform-rand 1)) 
     )
(dotimes (i 500)
     (setf x (* a x (- 1 x)))
     (setf xlist (append xlist x))
)
(select xlist (+ 400 (iseq 100)))
))

