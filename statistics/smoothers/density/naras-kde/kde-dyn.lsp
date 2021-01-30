(provide "kde-dyn")
;;;;
;;;; This file contains code that can be used when dynamic loading
;;;; is available.  The advantage is speed.
;;;;

(dyn-load "kde.o")

(defmeth kde-proto :kde ()
  (call-lfun "kde" (send self :data) (send self :kernel-type)
	     (send self :kernel-points) (send self :kernel-window-width)
	     (send self :weights)))
		   

		     

