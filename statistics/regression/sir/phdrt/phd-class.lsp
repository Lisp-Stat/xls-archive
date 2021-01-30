(defproto phd-class-model-proto
       '(level children terminal case parent ancestor phd-evct res 
         r2 p1x p2x f-test-val phd-eval error-before error-after 
         node-label child-case count phd-proj-var case-neighbor
         p-vals r2-list sh-list case-list f-v error-ratio)
       ()
       *object*
       "pHd Regression Model")

(defun phd-class-model (parent case)
  (let ( (m (send phd-class-model-proto :new))
        )
       (send m :parent parent)
       (send m :case case)
    m))
 
(defmeth phd-class-model-proto :isnew ()
       (send self :needs-computing t))
 
(defmeth phd-class-model-proto :save ()
       `(phd-class-model',(send self :case)
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

(defmeth phd-class-model-proto :case (&optional new-case)
  (when new-case
        (setf (slot-value 'case) new-case))
  (slot-value 'case))

(defmeth phd-class-model-proto :parent (&optional new-parent)
  (when new-parent
        (setf (slot-value 'parent) new-parent))
  (slot-value 'parent))

(defmeth phd-class-model-proto :ancestor (&optional new-ancestor)
  (when new-ancestor
        (setf (slot-value 'ancestor) new-ancestor))
  (slot-value 'ancestor))

(defmeth phd-class-model-proto :phd-evct (&optional new-phd-evct)
  (when new-phd-evct
        (setf (slot-value 'phd-evct) new-phd-evct))
  (slot-value 'phd-evct))

(defmeth phd-class-model-proto :res (&optional new-res)
  (when new-res
        (setf (slot-value 'res) new-res))
  (slot-value 'res))

(defmeth phd-class-model-proto :r2 (&optional new-r2)
  (when new-r2
        (setf (slot-value 'r2) new-r2))
  (slot-value 'r2))
 
(defmeth phd-class-model-proto :p1x (&optional new-p1x)
  (when new-p1x
        (setf (slot-value 'p1x) new-p1x))
  (slot-value 'p1x))

(defmeth phd-class-model-proto :p2x (&optional new-p2x)
  (when new-p2x
        (setf (slot-value 'p2x) new-p2x))
  (slot-value 'p2x))

(defmeth phd-class-model-proto :f-test-val (&optional new-f-test-val)
  (when new-f-test-val  
        (setf (slot-value 'f-test-val) new-f-test-val))
  (slot-value 'f-test-val))

(defmeth phd-class-model-proto :phd-eval (&optional new-phd-eval)
  (when new-phd-eval
        (setf (slot-value 'phd-eval) new-phd-eval))
  (slot-value 'phd-eval))

(defmeth phd-class-model-proto :error-before (&optional new-error-before)
  (when new-error-before
        (setf (slot-value 'error-before) new-error-before))
  (slot-value 'error-before))
 
(defmeth phd-class-model-proto :error-after (&optional new-error-after)
  (when new-error-after
        (setf (slot-value 'error-after) new-error-after))
  (slot-value 'error-after))
 
(defmeth phd-class-model-proto :node-label (&optional new-node-label)
  (when new-node-label
        (setf (slot-value 'node-label) new-node-label))
  (slot-value 'node-label))

(defmeth phd-class-model-proto :child-case (&optional new-child-case)
  (when new-child-case
        (setf (slot-value 'child-case) new-child-case))
  (slot-value 'child-case))

(defmeth phd-class-model-proto :count (&optional new-count)
  (when new-count
        (setf (slot-value 'count) new-count))
  (slot-value 'count))

(defmeth phd-class-model-proto :phd-proj-var (&optional new-phd-proj-var)
  (when new-phd-proj-var
        (setf (slot-value 'phd-proj-var) new-phd-proj-var))
  (slot-value 'phd-proj-var))

(defmeth phd-class-model-proto :case-neighbor (&optional new-case-neighbor)
  (when new-case-neighbor
        (setf (slot-value 'case-neighbor) new-case-neighbor))
  (slot-value 'case-neighbor))

(defmeth phd-class-model-proto :p-vals (&optional new-p-vals)
  (when new-p-vals
        (setf (slot-value 'p-vals) new-p-vals))
  (slot-value 'p-vals))

(defmeth phd-class-model-proto :r2-list (&optional new-r2-list)
  (when new-r2-list
        (setf (slot-value 'r2-list) new-r2-list))
  (slot-value 'r2-list))

(defmeth phd-class-model-proto :sh-list (&optional new-sh-list)
  (when new-sh-list
        (setf (slot-value 'sh-list) new-sh-list))
  (slot-value 'sh-list))

(defmeth phd-class-model-proto :case-list (&optional new-case-list)
  (when new-case-list
        (setf (slot-value 'case-list) new-case-list))
  (slot-value 'case-list))

(defmeth phd-class-model-proto :f-v (&optional new-f-v)
  (when new-f-v
        (setf (slot-value 'f-v) new-f-v))
  (slot-value 'f-v))

(defmeth phd-class-model-proto :error-ratio (&optional new-error-ratio)
  (when new-error-ratio
        (setf (slot-value 'error-ratio) new-error-ratio))
  (slot-value 'error-ratio))
