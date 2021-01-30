(provide "slave-stats")
;;;
;;; Code for calculation of the stats.
;;;

(defmeth slave-proto :mean (&optional current)
  (sum (* (send self :data) (send self :weights))))
  
(defmeth slave-proto :mean-sq (&optional current)
  (let ((d (send self :data)))
  (sum (* d d (send self :weights)))))

(defmeth slave-proto :stddev (&optional current)
  (let* ((d (send self :data))
	 (w (send self :weights))
	 (devs (- d (sum (* d w)))))
    (sqrt (sum (* devs devs w)))))
  
(defmeth slave-proto :mean-print-format (&optional ilist)
  (if ilist
      (let ((bc (send self :back-color))
	    (dc (send self :draw-color)))
	(send self :draw-color bc)
	(send self :redraw-stats)
	(setf (select (slot-value 'stats-print-formats) 0) ilist)
	(send self :draw-color dc)
	(send self :redraw-stats))
    (select (slot-value 'stats-print-formats) 0)))

(defmeth slave-proto :stddev-print-format (&optional ilist)
  (if ilist
      (let ((bc (send self :back-color))
	    (dc (send self :draw-color)))
	(send self :draw-color bc)
	(send self :redraw-stats)
	(setf (select (slot-value 'stats-print-formats) 1) ilist)
	(send self :draw-color dc)
	(send self :redraw-stats))
    (select (slot-value 'stats-print-formats) 1)))
      
(defmeth slave-proto :choose-mean-print-format ()
  (let* ((int (send self :mean-print-format))
	 (response (get-value-dialog 
		    "Mean Print Format?"
		    :initial `(list ,(select int 0) ,(select int 1))
		    :title (send self :identifier))))
    (when response
          (send self :mean-print-format (select response 0)))))

(defmeth slave-proto :choose-stddev-print-format ()
  (let* ((int (send self :stddev-print-format))
	 (response (get-value-dialog
		    "Stddev Print Format?"
		    :initial `(list ,(select int 0) ,(select int 1))
		    :title (send self :identifier))))
    (when response
          (send self :stddev-print-format (select response 0)))))


