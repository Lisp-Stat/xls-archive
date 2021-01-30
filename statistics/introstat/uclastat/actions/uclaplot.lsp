(defmeth data-multivariable-proto :plot-dialog ()
"Args: None
Dialog to select which plots to display."
(let* (
     (cancel (send modal-button-proto :new "Cancel"))
     (options (send modal-button-proto :new "Options"
              :action #'(lambda () (send self :plot-parm))))
     (tlist (list "Histogram-Plot" "Box-Plot" "Kernel-Plot" "Probability-Plot"
                  "Quantile-Plot" "Spike-Plot" "EDF-Plot" 
                  "Time-Plot" "Stem-Plot"))
     (vlist (mapcar #'(lambda (x) (send toggle-item-proto :new x)) tlist))
     (ok (send modal-button-proto :new "OK"
         :action #'(lambda () (mapcar #'(lambda (x)
                   (send x :value)) (adjoin vf vlist)))))
     (vf (send choice-item-proto :new (list "Show" "Save") :value 0))
     (top (send text-item-proto :new "Type of Plot"))
     (tip (send text-item-proto :new "Type of Action"))
     (pdiag (send modal-dialog-proto :new 
                 (list (list (adjoin top vlist) (list tip vf)) 
                       (list ok options cancel)) 
                  :default-button ok))
     )
(send pdiag :modal-dialog)
))

(defmeth data-multivariable-proto :plot (ivar)
"Args: None
Displays plots."
(let (
     (res (send self :plot-dialog))
     )
(if res 
(let* (
      (vt (elt (send self :data) ivar))
      (act (first res))
      (ind (which (rest res)))
      (num (send vt :numerical))
      (cmp (send vt :compar))
      (lab (send self :case-labels))
      (plist (select (send self :plots) ind))
      )
(mapcar #'(lambda (x) (send x :plot num)) plist)
;(if (= act 0)
;(mapcar #'(lambda (x) (send x :visible t)) plist)
;(mapcar #'(lambda (x) (send x :save)) plist))
))))

(defmeth data-multivariable-proto :plot-parm-diag (ivar)
(let* (
      (vt (elt (send self :data) ivar))
      (cancel (send modal-button-proto :new "Cancel"))
      (uv (if (find ':macintosh *features*) 155 200))
      (ok (send modal-button-proto :new "OK"
                :action #'(lambda()
                            (list (send set :text)
                            (send det :text)
                            (send xet :text)
                            (send ket :value)))))
      (l1 (list "Gaussian" "Triangular" "Uniform" "Bisquare"))
      (sit (send text-item-proto :new "Stem Size" :location '(10 10)))
      (dit (send text-item-proto :new "Comparison DF" :location '(10 40)))
      (xit (send text-item-proto :new "Points for Line Plots"
                 :location '(10 70)))
      (kit (send text-item-proto :new "Kernel"))
      (set (send edit-text-item-proto :new (send vt :stem)
                 :location (list uv 10)))
      (det (send edit-text-item-proto :new (send vt :compar) 
                 :location (list uv 40)))
      (xet (send edit-text-item-proto :new (send vt :numlines)
                 :location (list uv 70)))
      (ket (send choice-item-proto :new l1
              :value (position (send self :kernel) l1 :test #'string-equal)))
      (ppd (send modal-dialog-proto :new
          (list
          (list (list (list sit set) 
          (list dit det)
          (list xit xet))
          (list kit ket))
          (list ok cancel)) :default-button ok))
      )
(send ppd :modal-dialog)
))

(defmeth data-multivariable-proto :plot-parm (ivar)
"Args: None
Updates plot parameters"
(let (
     (vt (elt (send self :data) ivar))
     (lp (send self :plot-parm-diag))
     (l1 (list "Gaussian" "Triangular" "Uniform" "Bisquare"))     
     )
(send vt :stem (elt lp 0))
(send vt :compar (elt lp 1))
(send vt :numlines (elt lp 2))
(send vt :kernel (elt l1 (elt lp 3)))
(send vt :refresh-plots)
))


(defmeth data-multivariable-proto :refresh-plots (ivar)
(let* (
      (vt (elt (send self :data) ivar))
      (ll (send self :plots))
      (vlist (mapcar #'(lambda (x) (consp 
                     (send x :slot-value 'hardware-address))) ll))
      (ind (which vlist))
      (num (send self :numerical))
      (plist (select ll ind))
      )
(mapcar #'(lambda (x) (send x :clear) (send x :remove-menu-items)
            (send x :plot num)) plist)
))
