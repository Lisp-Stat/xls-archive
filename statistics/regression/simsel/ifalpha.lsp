(defun ifalpha (alpha x)
"Box-Cox transformation g(x,alpha)"

   (cond (( = alpha -999) x)
         ((= alpha 0) (log x))
 	 ((/= alpha 0) (/ (- (** x alpha) 1) alpha)))
)