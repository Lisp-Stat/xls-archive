(defmeth data-multivariable-proto :recode-dialog ()
"Args: None
Dialog to select recoding type and parameters."
(let* (
     (cancel (send modal-button-proto :new "Cancel"))
     (ok (send modal-button-proto :new "OK"
         :action #'(lambda () (send chc :value))))
     (top (send text-item-proto :new "Type of Recode"))
     (chc (send choice-item-proto :new 
               (list "Recode Equal Freq" "Recode Equal Interval"
                     "Recode Value" "Recode Interval") :value 0))
     (rdiag (send modal-dialog-proto :new 
                 (list top chc (list ok cancel))))
     )
(send rdiag :modal-dialog)
))

(defmeth data-multivariable-proto :recode (ivar)
"Args: None
Recodes the data."
(let (
      (sel (send self :recode-dialog))
     )
(case sel
(0 (send self :recode-uniform ivar))
(1 (send self :recode-equal ivar))
(2 (send self :recode-discrete ivar))
(3 (send self :recode-interval ivar))
)))

(defmeth data-multivariable-proto :recode-uniform (ivar)
(let* (
     (text (send text-item-proto :new "Number of Intervals"))
     (numb (send edit-text-item-proto :new "fill-me-in"))
     (vt  (elt (send self :data) ivar))
     (dat (send vt :data))
     (ok   (send modal-button-proto :new "OK" :action #'(lambda ()
                 (setf (select dat (select-number-indices dat)) 
                 (to-string-list (recode-uniform (send vt :numerical)
                 (eval-string-input (send numb :text))))))))
     (dg   (send modal-dialog-proto :new 
                 (list (list (list text numb ok)))))
     )
(send dg :modal-dialog)
))

(defmeth data-multivariable-proto :recode-equal (ivar)
(let* (
     (text (send text-item-proto :new "Width of Intervals"))
     (numb (send edit-text-item-proto :new "fill-me-in"))
     (vt  (elt (send self :data) ivar))
     (dat (send vt :data))
     (ok   (send modal-button-proto :new "OK" :action #'(lambda ()
                 (setf (select dat (select-number-indices dat)) 
                 (to-string-list (recode-equal (send vt :numerical)
                 (eval-string-input (send numb :text))))))))
     (dg   (send modal-dialog-proto :new 
                 (list (list (list text numb ok)))))
     )
(send dg :modal-dialog)
))

(defmeth data-multivariable-proto :recode-discrete (ivar)
(let* (
     (tex1 (send text-item-proto :new "Old Value"))
     (num1 (send edit-text-item-proto :new "" :location '(125 10)))
     (tex2 (send text-item-proto :new "New Value"))
     (num2 (send edit-text-item-proto :new "" :location '(125 40)))
     (vt  (elt (send self :data) ivar))
     (dat (send vt :data))
     (ok   (send modal-button-proto :new "OK" :action #'(lambda ()
                 (setf (select dat (select-number-indices dat)) 
                 (to-string-list (recode-discrete (send vt :numerical)
                 (eval-string-input (send num1 :text))
                 (eval-string-input (send num2 :text))))))))
     (dg   (send modal-dialog-proto :new 
                 (list (list tex1 num1) (list tex2 num2) ok)))
     )
(send dg :modal-dialog)
))

(defmeth data-multivariable-proto :recode-interval (ivar)
(let* (
     (tex1 (send text-item-proto :new "Lower Bound"))
     (num1 (send edit-text-item-proto :new "" :location '(125 10)))
     (tex2 (send text-item-proto :new "Upper Bound"))
     (num2 (send edit-text-item-proto :new "" :location '(125 40)))
     (tex3 (send text-item-proto :new "New Value"))
     (num3 (send edit-text-item-proto :new "" :location '(125 70)))
     (vt  (elt (send self :data) ivar))
     (dat (send vt :data))
     (ok   (send modal-button-proto :new "OK" :action #'(lambda ()
                 (setf (select dat (select-number-indices dat)) 
                 (to-string-list (recode-interval (send vt :numerical)
                 (eval-string-input (send num1 :text))
                 (eval-string-input (send num2 :text))
                 (eval-string-input (send num3 :text))))))))
     (dg   (send modal-dialog-proto :new 
                 (list (list tex1 num1) (list tex2 num2) 
                       (list tex3 num3) ok)))
     )
(send dg :modal-dialog)
))
