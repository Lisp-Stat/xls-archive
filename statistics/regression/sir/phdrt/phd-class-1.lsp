(defproto phd-class-model-proto
      '(level children terminal)
       ()
       *object*
       "pHd Regression Model")

(defun phd-class-model (level children terminal)
  (let (
        (m (send phd-class-model-proto :new))
        )
    (send m :level level)
    (send m :children children)
    (send m :terminal terminal)
    m))
 
(defmeth phd-class-model-proto :isnew ()
;   (send self :needs-computing t)
 )
 
(defmeth phd-class-model-proto :save ()
  `(phd-class-model',(send self :level)
                   ',(send self :children)
                   ',(send self :terminal)
                    ))
 
(defmeth phd-class-model-proto :needs-computing (&optional set)
  (if set (setf (slot-value 'case) nil))
  (null (slot-value 'case)))

(defmeth phd-class-model-proto :level (&optional new-level)
  (when new-level
        (setf (slot-value 'level) new-level))
  (slot-value 'level))

(defmeth phd-class-model-proto :children (&optional new-children)
  (when new-children
        (setf (slot-value 'children) new-children))
  (slot-value 'children))

(defmeth phd-class-model-proto :terminal (&optional new-terminal)
  (when new-terminal
        (setf (slot-value 'terminal) new-terminal))
  (slot-value 'terminal))

;(defmeth phd-class-model-proto :case (&optional new-case)
;  (when new-case
;        (setf (slot-value 'case) new-case))
;  (slot-value 'case))

