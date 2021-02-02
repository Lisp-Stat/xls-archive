;:
;; redraw a frame -- for dynamic plots
;:

(defmeth graph-proto :draw-next-frame (axis-list data 
    &optional (id-num (iseq (length (select data 0)))))
"
Message args: axis-list data &optional (id-num (iseq (length (select data 0))))
For use in animation.  The list axis-list contains the indices of the axes 
to be updated, and data, a list of lists, gives the corresponding data.  For
2-D plots, axis-list will typically be '(0 1) and data will have two lists.
Before updating, the functions in start-next-frame are executed; after
updating, the functions in finish-next-frame are executed."
  (mapcar #'funcall (send self :start-next-frame))
  (send self :point-coordinate axis-list 
        (repeat (list id-num) (length axis-list)) data)
  (mapcar #'funcall (send self :finish-next-frame)))

(defmeth graph-proto :start-next-frame (&optional function)
  (when (not (send self :has-slot 'start-next-frame)) 
        (send self :add-slot 'start-next-frame (list #'(lambda()))))
  (when function (setf (slot-value 'start-next-frame) 
         (append (slot-value 'start-next-frame) (list function) )))
  (slot-value 'start-next-frame))

(defmeth graph-proto :finish-next-frame (&optional function)
  (when (not (send self :has-slot 'finish-next-frame)) 
        (send self :add-slot 'finish-next-frame (list #'(lambda()))))
  (when function (setf (slot-value 'finish-next-frame) 
         (append (list function) (slot-value 'finish-next-frame))))
  (slot-value 'finish-next-frame))

;;;
;;;  Methods to restore/delete points from a model via a graph
;;;

(defmeth graph-proto :menu-delete-points ()
"Message args: ()
Adds menu items to a graphics menu if the object has a slot
owner."
(cond ((send self :has-slot 'owner)
  (let* ((delete-selection (send mytoggle-item-proto :new 
                                 #'(lambda () (send self :selection)) 
                                 "Delete Selection" 
                                 #'(lambda ()(send self :delete-selection))
                                 "Select Deletions" 
                                 #'(lambda() (send self :select-deletions))))
         (restore-selection (send mytoggle-item-proto :new 
                 #'(lambda () (send self :selection))
                                 "Restore Selection"
                                 #'(lambda ()(send self :restore-selection))
                                 "Restore All"
                                 #'(lambda() (send self :restore-all)))))
    (send (send self :menu) :append-items delete-selection 
          restore-selection)))))

(defmeth graph-proto :delete-selection ()
"Message args: ()
Deletes selected points (cases) from a model unless they are already deleted."
  (send (slot-value 'owner) :included
          (mapcar #'(lambda (a b) (and a b)) 
               (send (slot-value 'owner) :included) 
               (mapcar 'not (send self :point-selected 
                   (iseq 0 (1- (send (slot-value 'owner) :num-cases))))))))

(defmeth graph-proto :restore-all ()
"Message args: ()
Restores all deleted cases to a model."
  (send (slot-value 'owner) :toggle-cases))

(defmeth graph-proto :restore-selection ()
"Message args: ()
Restores selected deleted cases to a model."
(send (slot-value 'owner) :included
          (mapcar #'(lambda (a b) (if (and (not a) (not b)) nil t)) 
                (send (slot-value 'owner) :included) 
                (send self :point-selected 
                  (iseq 0 (1- (send (slot-value 'owner) :num-cases)))))))

(defmeth graph-proto :select-deletions ()
"Message args: ()
Highlights deleted cases."
  (send self :point-selected 
            (iseq 0 (1- (send (slot-value 'owner) :num-cases))) 
            (mapcar 'not (send (slot-value 'owner) :included))))

(defmeth spin-proto :use-color (&optional (toggle nil set))
"
This override method makes sure that color points are visible on a 
black spinning plot."
  (cond
    ((not set) (call-next-method))
    ((screen-has-color) (call-next-method toggle))
    ((eq (send self :back-color) 'white) (call-next-method toggle))
    (t (call-next-method nil))))

(defmeth graph-proto :get-color (color)
"Message args: (j)
Returns a color, depending on the :back-color."
  (let* ((j (position color *colors*))
         (black-colors '(0 0 2 7 5 5 6 7))
         (white-colors '(1 1 2 3 4 4 6 3)))
    (select *colors* (if (eq (send self :back-color) 'white)
                         (select white-colors j)
                         (select black-colors j)))))
;;;
;;;  num-point-variables returns num-variables for everything but histograms
;;;

(defmeth graph-proto :num-point-variables ()
"Message args: ()
Returns the number of axes."
  (send self :num-variables))

(defmeth histogram-proto :num-point-variables ()
"Message args: ()
Returns the number of axes is a histogram."
  (- (send self :num-variables) 1))

(defmeth graph-proto :tile (&optional where )
"Message args: ()
This message moves a plot to one of four locations.  Sending this message to
four different plots will fill a workstation computer screen."
  (when (null (send self :has-slot 'tile-loc))
        (send self :add-slot 'tile-loc 2))
  (if where
      (setf (slot-value 'tile-loc) where)
      (setf (slot-value 'tile-loc) (rem (+ 1 (slot-value 'tile-loc)) 4)))
  (case (slot-value 'tile-loc)
       (0 (send self :location 1 40))
       (1 (send self :location 515 40))
       (2 (send self :location 1 480))
       (3 (send self :location 515 480)))
  (slot-value 'tile-loc))

(defmeth graph-proto :dec-size ()
"This resizes a plot to be one forth of a computer screen on a 16 in. Sony 
color monitor."
  (send self :size 510 365))








