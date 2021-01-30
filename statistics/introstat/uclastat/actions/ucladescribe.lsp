(defmeth multi-variable-proto :describe-dialog ()
"Args: None
Dialog to select which descriptive statistics to display."
(let* (
      (t1 (list "Length" "Mean" "Median" "Min" "Max"))
      (t2 (list "Range" "Standard-Deviation" "Interquartile-Range"
                   "Skewness" "Kurtosis"))
      (v1 (mapcar #'(lambda (x) (send toggle-item-proto :new x))
                     t1))
      (v2 (mapcar #'(lambda (x) (send toggle-item-proto :new x))
                     t2))
      (v3 (send toggle-item-proto :new "Other"))
      (cancel (send modal-button-proto :new "Cancel"))
      (ok (send modal-button-proto :new "OK"
         :action #'(lambda () (mapcar #'(lambda (x)
                     (send x :value)) (combine v1 v2 v3)))))
     (top (send text-item-proto :new "Descriptive Statistics"))
     (options (send modal-button-proto :new "Options"))
     (ddiag (send modal-dialog-proto :new 
                  (list (list top)
                  (list v1 v2 v3) (list ok options cancel))
                  :default-button ok))
     )
(send ddiag :modal-dialog)
))

(defmeth multi-variable-proto :describe ()
"Args: None
Displays descriptive statistics."
(let (
      (ivar (first (which (send self :select))))
      (sel (send self :describe-dialog))
      )
(if sel
(if (which sel)
      (let* 
      (
      (vt (elt (send self :data) ivar))
      (h (if (first (last sel)) 
             (get-string-dialog "Other Statistic:") "Other"))
      (tlist (select (list "Length" "Mean" "Median" "Min" "Max"
                   "Range" "Standard-Deviation" "Interquartile-Range"
                   "Skewness" "Kurtosis" h) (which sel)))
      (flist (mapcar #'(lambda (x) (intern (string-upcase x))) tlist))
      (slist (mapcar #'(lambda (x)  
                       (funcall x (numerical vt (send self :code)))) flist))
      (ilist (mapcar #'(lambda (x) (send text-item-proto :new x)) tlist))
      (vlist (mapcar #'(lambda (x) (send text-item-proto :new
                       (format nil "~,6g" x))) slist))
      (ok (send modal-button-proto :new "OK"))
      (sdiag (send modal-dialog-proto :new
      (list (list ilist vlist) ok) :default-button ok))
      )
(send sdiag :modal-dialog))))
))
