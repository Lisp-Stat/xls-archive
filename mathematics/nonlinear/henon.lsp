(defun henon ()
"Plots the iterates of the Henon iteration."
(let (
     (plt (plot-points (list 0) (list 0)))     
     (x '(0 0))
     )
(loop (setf x (henon-update x))      
      (send plt :add-points (list (elt x 0)) (list (elt x 1)))
      (send plt :adjust-to-data)
)
))

(defun henon-update (x)
   (let* (
         (p (elt x 0))
         (q (elt x 1))
         (a (+ 1 q (- (* 1.4 p p))))
         (b (* .3 p))
         )
(list a b)
   )
)


   