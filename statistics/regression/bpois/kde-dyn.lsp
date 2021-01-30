(provide "kde-dyn")
;;;;
;;;; This file contains code that can be used when dynamic loading
;;;; is available.  The advantage is speed.
;;;;

(dyn-load *kde-code*)

(defmeth kde-proto :kde ()
  (let ((x (send self :x-vals)))
    (list x (call-lfun "kde" 
		       (send self :data) 
		       (send self :kernel-type) x 
		       (send self :kernel-window-width) 
		       (send self :weights)))))
		   

		     

