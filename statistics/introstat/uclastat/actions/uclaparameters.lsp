(defmeth multi-variable-proto :parameter-dialog ()
(let* (
     (ok (send modal-button-proto :new "OK"
               :action #'(lambda () 
                           (list (send ce :text)
                                 ))))
     (cancel (send modal-button-proto :new "Cancel"))
     (ct (send text-item-proto :new "Missing Data Code"))
     (ce (send edit-text-item-proto :new (send self :code)))
     (pdiag (send modal-dialog-proto :new 
                 (list 
                 (list
                  (list ct ce)
                  )
                 (list ok cancel)) :default-button ok))
     )
(send pdiag :modal-dialog)
))

(defmeth multi-variable-proto :set-parameters ()
(let (
     (col (send self :parameter-dialog))
     )
(send self :code (first col))
))


