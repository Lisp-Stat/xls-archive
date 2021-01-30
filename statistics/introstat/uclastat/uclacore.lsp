
(defproto multi-variable-proto 
  '(data 
  	title 
  	legend 
  	weights 
  	case-labels 
  	variable-labels 
  	backup 
  	parameters) () () 
  "Prototype for a generic multivariable")   

(defmeth multi-variable-proto :isnew
  (data &key title legend weights case-labels variable-labels backup parameters)
(send self :data data)
(send self :title 
      (if title title (send self :make-title)))
(send self :legend
      (if legend legend (send self :make-legend)))
(send self :weights
      (if weights weights (send self :make-weights)))
(send self :case-labels
      (if case-labels case-labels (send self :make-case-labels)))
(send self :variable-labels
      (if variable-labels variable-labels (send self :make-variable-labels)))
(send self :backup
      (if backup backup (send self :make-backup)))
(send self :parameters
      (if parameters parameters (send self :make-parameters)))
	  )

(defmeth multi-variable-proto :make-case-labels ()
(mapcar #'(lambda (x) (format nil "~a" x))
                      (iseq 0 (- (send self :nobs) 1)))
)

(defmeth multi-variable-proto :make-variable-labels ()
(mapcar #'(lambda (x) (format nil "~a" x))
                      (iseq 0 (- (send self :nvar) 1)))
)

(defmeth multi-variable-proto :make-weights ()
"Args: NONE
Sets the default weights (a vector of ones)."
(to-string-list (repeat 1 (send self :nobs)))
)

(defmeth multi-variable-proto :make-backup ()
"Args: NONE
Sets the default backup."
(copy-list (send self :data))
)

(defmeth multi-variable-proto :make-parameters ()
"Args: NONE
Sets the default parameters."
  (send self :parameters 
        (list (list 'code "NaN")
              (list 'stem "1")
              (list 'leaf "10")
              (list 'compar "normal")
              (list 'kernel "bisquare")
              (list 'numlines "30")))
)

(defmeth multi-variable-proto :make-legend ()
"Args: NONE
Sets the default legend."
(send self :legend "This is supposed to describe the multi-variable")
)

(defmeth multi-variable-proto :make-title ()
"Args: NONE
Sets the default title."
(send self :title "Anonymous")
)

(defmeth multi-variable-proto :data (&optional (data nil set))
"Message args: (&optional DATA)
With no argument returns the list of data values. Otherwise
sets the data equal to DATA."
  (if set (setf (slot-value 'data) data))
  (slot-value 'data))

(defmeth multi-variable-proto :backup (&optional (backup nil set))
"Message args: (&optional DATA)
With no argument returns the list of original data values. Otherwise
sets the backup equal to DATA."
  (if set (setf (slot-value 'backup) backup))
  (slot-value 'backup))

(defmeth multi-variable-proto :legend (&optional (legend nil set))
"Message args: (&optional LEGEND)
With no argument returns the legend string. Otherwise
sets the string equal to LEGEND and returns the new value."
  (if set (setf (slot-value 'legend) legend))
  (slot-value 'legend))

(defmeth multi-variable-proto :weights (&optional (weights nil set))
"Message args: (&optional WEIGHTS)
With no argument returns the list of weights. Otherwise
sets the list equal to WEIGHTS."
  (if set (setf (slot-value 'weights) weights))
  (slot-value 'weights))

(defmeth multi-variable-proto :parameters (&optional (parameters nil set))
"Message args: (&optional PARAMETERS)
With no argument returns the parameter list. Otherwise
sets the list equal to PARAMETERS."
  (if set (setf (slot-value 'parameters) parameters))
  (slot-value 'parameters))

(defmeth multi-variable-proto :title (&optional (title nil set))
"Message args: (&optional TITLE)
With no argument returns the title string. Otherwise
sets the title equal to TITLE."
  (if set (setf (slot-value 'title) title))
  (slot-value 'title))

(defmeth multi-variable-proto :code (&optional (code nil set))
"Message args: (&optional CODE)
With no argument returns the missing data code. Otherwise
sets the code equal to CODE."
(let (
     (s (slot-value 'parameters))
     )
(if set (setf (second (assoc 'code s)) code))
  (second (assoc 'code s))))

(defmeth multi-variable-proto :stem (&optional (stem nil set))
(let (
     (s (slot-value 'parameters))
     )
(if set (setf (second (assoc 'stem s)) stem))
  (second (assoc 'stem s))))

(defmeth multi-variable-proto :leaf (&optional (leaf nil set))
(let (
     (s (slot-value 'parameters))
     )
(if set (setf (second (assoc 'leaf s)) leaf))
  (second (assoc 'leaf s))))

(defmeth multi-variable-proto :compar (&optional (compar nil set))
(let (
     (s (slot-value 'parameters))
     )
(if set (setf (second (assoc 'compar s)) compar))
  (second (assoc 'compar s))))

(defmeth multi-variable-proto :kernel (&optional (kernel nil set))
(let (
     (s (slot-value 'parameters))
     )
(if set (setf (second (assoc 'kernel s)) kernel))
  (second (assoc 'kernel s))))

(defmeth multi-variable-proto :numlines (&optional (numlines nil set))
(let (
     (s (slot-value 'parameters))
     )
(if set (setf (second (assoc 'numlines s)) numlines))
  (second (assoc 'numlines s))))

(defmeth multi-variable-proto :data (&optional (data nil set))
"Message args: (&optional DATA)
With no argument returns the variable list.
Otherwise sets the variables equal to DATA and returns the new value."
  (if set (setf (slot-value 'data) data))
  (slot-value 'data)
)

(defmeth multi-variable-proto :case-labels (&optional (labels nil set))
"Message args: (&optional LABELS)
With no argument returns the case-labels.
Otherwise sets the case-labels equal to LABELS and returns the new value."
  (if set (setf (slot-value 'case-labels) labels))
  (slot-value 'case-labels))

(defmeth multi-variable-proto :variable-labels (&optional (labels nil set))
"Message args: (&optional LABELS)
With no argument returns the variable-labels.
Otherwise sets the variable-labels equal to LABELS and returns the new value."
  (if set (setf (slot-value 'variable-labels) labels))
  (slot-value 'variable-labels))

(defmeth multi-variable-proto :nobs ()
  (length (first (send self :data)))
)

(defmeth multi-variable-proto :nvar ()
   (length (send self :data))
)

