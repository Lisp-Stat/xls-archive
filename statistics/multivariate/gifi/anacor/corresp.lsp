(defun anacor (c)
"Args: cross-table
A correspondence analysis of CROSS-TABLE is returned."
(let* (
      (d (mapcar #'sum (row-list c)))
      (e (mapcar #'sum (column-list c)))
      (nr (length d))
      (nc (length e))
      (n (sum d))
      (t (/ c (sqrt (outer-product d e))))
      (u (sv-decomp t))
      (v (list 1 2 3))
      (p (length (elt u 1)))
      )
(setf (elt v 0) (elt u 1))
(setf (elt v 1) 
(* (sqrt n) (* (elt u 0) (outer-product (/ (sqrt d)) (elt u 1)))))
(setf (elt v 2) 
(* (sqrt n) (* (elt u 2) (outer-product (/ (sqrt e)) (elt u 1)))))
(cond 
      ((> p 3)  (let* (
                      (xx (list 
                          (coerce (select (elt v 1) (iseq nr) 1) 'vector)
                          (coerce (select (elt v 1) (iseq nr) 2) 'vector)
                          (coerce (select (elt v 1) (iseq nr) 3) 'vector)))
                      (yy (list 
                          (coerce (select (elt v 2) (iseq nc) 1) 'vector)
                          (coerce (select (elt v 2) (iseq nc) 2) 'vector)
                          (coerce (select (elt v 2) (iseq nc) 3) 'vector)))
                      (aa (spin-plot xx))
                      )
                  (send aa :add-points yy) 
                  ))
      ((> p 2) (let* (
                      (xx (list 
                          (coerce (select (elt v 1) (iseq nr) 1) 'vector)
                          (coerce (select (elt v 1) (iseq nr) 2) 'vector)))
                      (yy (list 
                          (coerce (select (elt v 2) (iseq nc) 1) 'vector)
                          (coerce (select (elt v 2) (iseq nc) 2) 'vector)))
                      (aa (plot-points xx))
                      )
                (send aa :add-points yy)
                ))
)
v))
