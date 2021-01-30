;;;;
;;;;     Simulation Cells  --  Special recorders for simulations
;;;; 
;;;;                  27 May 92 ... Cut from old simobjs.lsp.
;;;;

(provide "simcell")

;;;;

#||

A simulation cell is a collection of labelled recorders, and hence is
a dictionary.

NOTES:

(1)  Use the features option to build recorders of objects within
     the cell that you are not likely to compare directly.  Example,
     use one recorder for one feature of a model, use a second for
     a different feature.

(2)  Features led to separate plots by method :plot-summary.  Hence if
     location is a feature, then plot summary will isolate the several
     loc estimates together.  Use compare-features to evaluate the 
     relationship among features.

||#

;;;     __________  SIMULATION CELL OBJECT  __________

(defproto sim-cell-proto
  '(simulator ; to which cell belongs
    label     ; dot-pair labelling for block of the simulation object
    recorder  ; one/list of labelled features and associated recorders
   ))


;;     _____  CREATOR  _____

(defun MAKE-SIM-CELL (simulator
                     &key label features (recorderProto condenser-proto))
  (send sim-cell-proto :new simulator
        :label label :features features :recorderProto recorderProto))


(defmeth sim-cell-proto :ISNEW (simulator &key label features recorderProto)
  (setf (slot-value 'simulator) simulator)
  (setf (slot-value 'label) label)
  (setf (slot-value 'recorder)
        (if features
            (mapcar #'cons features
                  (if (listp recorderProto)
                      (mapcar #'(lambda(r f) (send r :new :desc f))
                              recorderProto features)
                      (mapcar #'(lambda(f) (send recorderProto :new :desc f))
                              features))   )
            (send recorderProto :new)  ))
  )


;;  _____  ACCESSORS  _____


(defmeth sim-cell-proto :SIMULATOR ()
  (slot-value 'simulator))

(defmeth sim-cell-proto :LABEL ()
  (slot-value 'label))

(defmeth sim-cell-proto :HAS-FEATURES? ()
  "Does this cell house several recorders?"
  (listp (slot-value 'recorder))  )

(defmeth sim-cell-proto :HAS-FEATURE (aPair)
  (let ((mine (assoc (car aPair) (send self :label)))  )
    (if mine
        (equalp (cdr mine) (cdr aPair))
        mine)))

(defmeth sim-cell-proto :RECORDER (&key of)
  (if of
      (if (listp of) ; assume list of items to store matches recorders
          (slot-value 'recorder)
          (cdr (assoc of (slot-value 'recorder)))  )
      (slot-value 'recorder)  ))

(defmeth sim-cell-proto :ADD (this &key to)
  "Add item to appropriate recorder.  If recorder chosen is list,
   maps the add over the recorder collection."
  (let ((rec (send self :recorder :of to)))
    (if (listp rec)
        (mapcar #'(lambda (r x) (send (cdr r) :add x)) rec this)
        (send rec :add this)
        )))


;;     _____  OPERATIONS ON RECORDERS  _____


(defmeth sim-cell-proto :LOCATION (&key of)
  (send (send self :recorder :of of) :location))

(defmeth sim-cell-proto :SCALE    (&key of)
  (send (send self :recorder :of of) :scale))

(defmeth sim-cell-proto :LANDS    (&key of)
  (send (send self :recorder :of of) :lands))

(defmeth sim-cell-proto :DATA    (&key of)
  (send (send self :recorder :of of) :list))

(defmeth sim-cell-proto :DO (func &key to)
  (if to
      (funcall func (send self :recorder :of to))
      (if (listp (slot-value 'recorder))  ; labelled
          (mapcar #'(lambda (r) (funcall func (cdr r)))
                  (slot-value 'recorder))
          (funcall func (slot-value 'recorder)))    ))
  
(defmeth sim-cell-proto :PRINT-SUMMARY (&key of)
  (format t "Summary of cell ~a.~%" (send self :label))
  (send self :do #'(lambda (r) (send r :print-summary))
        :to of))

(defmeth sim-cell-proto :PLOT-SUMMARY (&key of)
  (let ((sim (slot-value 'simulator))
        (func #'(lambda (i)   ; function to build sample block i
                  (send (slot-value 'simulator) :recover-sample i
                        (send self :label))))   )
    (send self
          :do #'(lambda (r)
                  (send r :plot-summary
                        :title (send self :label)
                        :recoverFun func
                        :labels (mapcar #'(lambda (l) (format nil "~a" l))
                                        (send sim :treatment-labels))  ))              
                  :to of)
    ))

(defmeth sim-cell-proto :WRITE-TO-FILE (fileName)
  "Opens the file and writes the contents of the cell."
  (with-open-file (fStream fileName :direction :output)
                  (send self :append-to-stream stream)
                  ))
  
(defmeth sim-cell-proto :APPEND-TO-STREAM (&optional stream)
  "Write label values and contents to the given stream."
  (setf stream (if stream stream *standard-output*)) ; used for debugging
  (let* ((tags  (mapcar #'(lambda (l) (format nil " ~s" (cdr l)))
                       (send self :label)))
         (prefix (apply #'strcat tags)))
    (send self :do #'(lambda (r)
                       (send r :append-to-stream stream prefix)))
    ))

