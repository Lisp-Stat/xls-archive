(defproto dummydata '(data edit-dialog) '() ())

(defmeth dummydata :isnew (&optional default-data)
  (setf (slot-value 'data) default-data)
  (setf (slot-value 'edit-dialog)
	(send edit-value-dialog-proto :new
	      "Data (Enter expression):"
	      :owner self :message :data
	      :name "Data Dialog")))
  

(defmeth dummydata :data (&optional (data nil set))
  (if set (setf (slot-value 'data) data)
    (slot-value 'data)))


(def d (send dummydata :new nil))
