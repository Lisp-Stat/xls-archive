(defmeth multi-variable-proto :impute-dialog ()
"Args: None
Dialog to select imputation type."
(let* (
     (cancel (send modal-button-proto :new "Cancel"))
     (ok (send modal-button-proto :new "OK"
         :action #'(lambda () (send chc :value))))
     (top (send text-item-proto :new "Type of Imputation"))
     (chc (send choice-item-proto :new 
               (list "Simple Mean" "Simple Random") :value 0))
     (idiag (send modal-dialog-proto :new 
                 (list top chc (list ok cancel))
                  :default-button ok))
     )
(send idiag :modal-dialog)
))

(defmeth multi-variable-proto :impute ()
"Args: None
Imputes the missing data."
(let (
     (sel (send self :impute-dialog))
     )
(if sel
(progn (case sel
  (0 (send self :impute-mean))
  (1 (send self :impute-random))
  )
))))

(defmeth multi-variable-proto :impute-mean ()
(let* (
      (dat (send self :data))
      (mis (send self :missing))
      (nms (length mis))
      (men (num-to-string (mean (send self :numerical))))
      )
(setf (select dat (send self :missing)) (repeat men nms))
))

(defmeth multi-variable-proto :impute-random ()
(let* (
      (dat (send self :data))
      (mis (send self :missing))
      (nms (length mis))
      (rep (to-string-list (sample (send self :numerical) nms t)))
      )
(setf (select dat (send self :missing)) rep)
))
