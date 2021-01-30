
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Methods for regression-model-proto to allow non-numeric 
;;;  characters to exist in the design matrix and y vector.  
;;;  Cases with any non-numeric values are excluded from regression
;;;  computations but all data is still stored (and may be retrived)
;;;  from the x and y slots by directly accessing them 
;;;  (e.g. (send the-model :slot-value 'x) or 
;;;        (send the-model :slot-value 'y))
;;;
;;;  Questions, comments to:
;;;    Jason Bond  (jbond@stat.ucla.edu)
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmeth regression-model-proto :isnew () 
   (send self :needs-computing t)
   (send self :add-slot 'remove-missing)
   (send self :add-slot 'good-rows))

(defmeth regression-model-proto :remove-missing (&optional (val nil set))
 (if set (setf (slot-value 'remove-missing) val)
   (slot-value 'remove-missing))
)
(defmeth regression-model-proto :good-rows (&optional (val nil set))
 (if set (setf (slot-value 'good-rows) val)
   (slot-value 'good-rows))
)


(defmeth regression-model-proto :find-good-rows ()
  (let* (
         (xy (row-list (bind-columns (slot-value 'x) (slot-value 'y))))
         (good-rows (mapcar #'(lambda (z) 
                     (if (every #'(lambda (x) (if x t nil))
                         (mapcar #'numberp (coerce z 'list)))
                             t nil)) xy))
        )
    (setf (slot-value 'good-rows) (which good-rows))
   )
)

(defmeth regression-model-proto :get-nonmissing-x ()
  (let (
        (x (row-list (slot-value 'x)))
        (good-rows (send self :good-rows))
       )
    (apply #'bind-rows (select x good-rows))
  )
)
        
(defmeth regression-model-proto :get-nonmissing-y ()
  (let (
        (y (slot-value 'y))
        (good-rows (send self :good-rows))
       )
    (select y good-rows)
  )
)
       

(defmeth regression-model-proto :x (&optional new-x)
"Message args: (&optional new-x)
With no argument returns the x matrix as supplied to m. With an argument
NEW-X sets the x matrix to NEW-X and recomputes the estimates."
  (when (and new-x (matrixp new-x))
        (setf (slot-value 'x) new-x)
        (when (slot-value 'y)
              (send self :find-good-rows)
            (if (> (length (mapcar #'not (send self :good-rows))) 0)
                  (send self :remove-missing t)
                  (send self :remove-missing nil)))
        (send self :needs-computing t))
  (if (send self :remove-missing)
        (send self :get-nonmissing-x)
        (slot-value 'x)))

(defmeth regression-model-proto :y (&optional new-y)
"Message args: (&optional new-y)
With no argument returns the y sequence as supplied to m. With an argument
NEW-Y sets the y sequence to NEW-Y and recomputes the estimates."
  (when (and new-y (or (matrixp new-y) (sequencep new-y)))
        (setf (slot-value 'y) new-y)
        (send self :find-good-rows)
        (if (> (length (mapcar #'not (send self :good-rows))) 0)
            (send self :remove-missing t)
            (send self :remove-missing nil))
        (send self :needs-computing t))
  (if (send self :remove-missing)
        (send self :get-nonmissing-y)
        (slot-value 'y)))




