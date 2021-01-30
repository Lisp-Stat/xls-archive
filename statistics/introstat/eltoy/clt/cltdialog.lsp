;;; Copyright 1991 Russell G. Almond
;;; License is granted to copy this program under the GNU library
;;; public licence.  This program comes with NO WARANTEE.  See file
;;; COPYRIGHT for details.


;;; This creates a menu of distribution display tools for a variety of
;;; distributions.  It is started by the function (launch-dist-toy).
;;;

(defproto clt-dialog-proto
  '(text1-item text2-item text3-item text4-item text5-item
    n-item reps-item name-item
    add-qq-plot-button  more-button
    histogram)
  '() (list dialog-proto)
"This display explains what the clt demos are doing and allows for
limited control."
)

(defmeth clt-dialog-proto :isnew (h &key name)
  (setf (slot-value 'histogram) h)
  (setf (slot-value 'text1-item)
	(send text-item-proto :new "Histogram of "))
  (setf (slot-value 'text2-item)
	(send text-item-proto :new " samples"))
  (setf (slot-value 'text3-item)
	(send text-item-proto :new "Each sample is the total of"))
  (setf (slot-value 'text4-item)
	(send text-item-proto :new " observations from the"))
  (setf (slot-value 'text5-item)
	(send text-item-proto :new " distribution."))
  (setf (slot-value 'n-item)
	(send text-item-proto :new
	      (format nil " ~5d " (send h :slot-value 'sample-size))
	      :text-length 7))
  (setf (slot-value 'reps-item)
	(send text-item-proto :new
	      (format nil " ~4d " (send h :slot-value 'reps))
	      :text-length 6))
  (setf (slot-value 'name-item)
	(send text-item-proto :new
	      (format nil "~A " name)))
  
  (setf (slot-value 'add-qq-plot-button)
	(send button-item-proto :new "Add QQ-plot"
	      :action #'(lambda () (clt-demo-add-qqplot h))))
  (setf (slot-value 'more-button)
	(send button-item-proto :new "More observations"
	      :action #'(lambda ()
			  (send h :more :reps 1))))
  
  (call-next-method			;dialog-proto
   (list (list (slot-value 'text1-item) (slot-value 'n-item)
	       (slot-value 'text2-item))
	 (list (slot-value 'text3-item))
	 (list (slot-value 'reps-item) (slot-value 'text4-item))
	 (list (slot-value 'name-item) (slot-value 'text5-item))
	 (list (slot-value 'more-button) (slot-value 'add-qq-plot-button)))
   :title (format nil "~A Control" name)))


(defmeth clt-dialog-proto :add-points ()
  (send (slot-value 'reps-item)
	:text 
	(format nil " ~4d " (send (slot-value 'histogram) :slot-value
				  'reps)) )
  )

(new-provide :el-clt-dialog)
