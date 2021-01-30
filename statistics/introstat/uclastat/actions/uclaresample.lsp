(defmeth data-variable-proto :resample-dialog ()
"Args: none
Dialog to select user parameters" 
(let* (
  	 (first-label (send text-item-proto :new "Function to be Bootstrapped"))
  	 (second-label (send text-item-proto :new "Number of Bootstraps"))
  	 (third-label (send text-item-proto :new "Number of Full Displays"))
  	 (first-reply (send edit-text-item-proto :new "mean"))
  	 (second-reply (send edit-text-item-proto :new "100"))
  	 (third-reply (send edit-text-item-proto :new "3"))
  	 (cancel (send modal-button-proto :new "Cancel"))
  	 (ok (send modal-button-proto :new "OK" 
           :action #'(lambda()
                     (list
                     (send first-reply :text)
                     (dconvert (coerce (send second-reply :text) 'list))
                     (dconvert (coerce (send third-reply :text) 'list))))
           ))
  (choice-dialog (send modal-dialog-proto :new
         (list 
         (list
         (list first-label second-label third-label)
		       (list first-reply second-reply third-reply))
         (list ok cancel)
         )
         :default-button ok)))
  (send choice-dialog :modal-dialog)
))

(defmeth data-variable-proto :resample ()
"Args: none
Graphics driver for one-dimensional bootstrap program. Puts up
dialogs for choosing the function to be bootstrapped and the
number of samples."
(let* (
     (args (send self :resample-dialog))
     (f (intern (string-upcase (elt args 0))))
     (n (elt args 1))
     (p (elt args 2))
     (x (send self :numerical))
     (h (histogram nil))
     (y (make-list n))
     )
(send h :location 250 250)
(send h :title "Bootstrap Distribution")
  (dotimes (i n)
    (setf (nth i y) (funcall f (if (<= i p)
	(boot-histogram x) 
	(boot-plain x))))
    (send h :add-points (list (nth i y)))
   (send h :adjust-to-data)
    )
(send h :add-lines (kernel-dens y))
(send h :add-lines (fit-normal y))
))
