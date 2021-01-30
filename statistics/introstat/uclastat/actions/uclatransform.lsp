(defmeth data-multivariable-proto :transform-dialog ()
(let* (
       (l1 (list "neg" "abs" "sign" "ln" "exp" "squareroot"
                 "cuberoot" "square" "cube" "reciprocal"
                 "logit" "probit" "arcsin" "arctanh" "other"))
       (l2 (send list-item-proto :new l1 :action
                 #'(lambda (x)
                     (if x 
                         (send tdiag :modal-dialog-return
                               (list (send l3 :value)
                               (elt l1 (send l2 :selection))))))))
       (l3 (send choice-item-proto :new (list "Replace" "Append")
                 :value 0))
     (cancel (send modal-button-proto :new "Cancel"))
     (tdiag (send modal-dialog-proto :new
                  (list (list l2 l3) cancel) :default-button cancel))
     )
(send tdiag :modal-dialog)
))

(defmeth data-multivariable-proto :transform (ivar)
(let (
     (sel (send self :transform-dialog))
     )
(if sel
    (let* (
           (vt (elt (send self :data) ivar))
           (fs (first sel))
           (gs (second sel))
           (dt (send vt :data))
           (fc (intern (string-upcase gs)))
           (ff (funcall fc (send vt :numerical)))
           (id (select-number-indices dt))
           (et (copy-list dt))
           (ft (to-string-list ff))
           )
(if (= 0 fs)
    (setf (select dt id) ft)
    (let (
          (et (copy-list dt))
          )
      (setf (select et id) ft)
      (send self :data 
            (combine (send self :data) 
                     (send data-variable-proto :new et 
                           :title (concatenate 'string (send vt :title)))))) 
)))))
