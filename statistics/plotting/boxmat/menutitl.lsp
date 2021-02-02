(setf *data-set-menu* (send menu-proto :new "Data-Set-Name"))
(send *data-set-menu* :append-items
      (let ((new-name-item (send menu-item-proto :new "New Name")))
	(defmeth new-name-item :do-action ()
		 (let ((df (get-string-dialog "Data set Name:"
			    :initial (send  *data-set-menu* :title))))
		      (when df (send *data-set-menu* :new-title df))
		 )
	)
	new-name-item
      )
)

(defmeth *data-set-menu* :new-title (new-title)
 (send self :remove)
 (send self :title new-title)
 (send self :install)
)

(send *data-set-menu* :install)
