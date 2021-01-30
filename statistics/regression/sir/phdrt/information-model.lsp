(defproto information-model-proto
      '(x y node intermediate-nodes terminal-nodes test menu-title)
       ()
      *object*
      "Regression Information Model")

(defun information-model (x y node intermediate-nodes terminal-nodes 
                          test menu-title)
  (let (
        (m (send information-model-proto :new))
        )
      (send m :x x)
      (send m :y y)
      (send m :node node)
      (send m :intermediate-nodes intermediate-nodes)
      (send m :terminal-nodes terminal-nodes)
;     (send m :sub_node sub_node)
      (send m :test test)
      (send m :menu-title menu-title)
      (send m :compute)
      m))

(defmeth information-model-proto :isnew () (send self :needs-computing t))

(defmeth information-model-proto :save ()
    `(information-model',(send self :x)
                       ',(send self :y)
                       ))

(defmeth information-model-proto :compute ()
  (let*(
        (x (send self :x))
        (y (send self :y))
        (node (send self :node))
        (intermediate-nodes (send self :intermediate-nodes))
        (terminal-nodes (send self :terminal-nodes))
;        (sub_node (send self :sub_node))
        (test (send self :test))
        (menu-title (send self :menu-title))
        )
    (setf (slot-value 'x) x)
    (setf (slot-value 'y) y)
    (setf (slot-value 'node) node)
    (setf (slot-value 'intermediate-nodes) intermediate-nodes)
    (setf (slot-value 'terminal-nodes) terminal-nodes)
;    (setf (slot-value 'sub_node) sub_node)
    (setf (slot-value 'test) test)
    (setf (slot-value 'menu-title) menu-title)
    ))

(defmeth information-model-proto :needs-computing (&optional set)
    (if set (setf (slot-value 'test) nil))
    (null (slot-value 'test)))


(defmeth information-model-proto :x (&optional new-x)
  (when new-x
        (setf (slot-value 'x) new-x)
        (send self :needs-computing t))
  (slot-value 'x))

(defmeth information-model-proto :y (&optional new-y)
  (when new-y
        (setf (slot-value 'y) new-y)
        (send self :needs-computing t))
  (slot-value 'y))

(defmeth information-model-proto :node (&optional new-node)
  (when new-node
        (setf (slot-value 'node) new-node)
        (send self :needs-computing t))
  (slot-value 'node))

(defmeth information-model-proto :intermediate-nodes (&optional new-intermediate-nodes)
  (when new-intermediate-nodes
        (setf (slot-value 'intermediate-nodes) new-intermediate-nodes)
        (send self :needs-computing t))
  (slot-value 'intermediate-nodes))

(defmeth information-model-proto :terminal-nodes (&optional new-terminal-nodes)
  (when new-terminal-nodes
        (setf (slot-value 'terminal-nodes) new-terminal-nodes)
        (send self :needs-computing t))
  (slot-value 'terminal-nodes))

;(defmeth information-model-proto :sub_node (&optional new-sub_node)
;  (when new-sub_node
;        (setf (slot-value 'sub_node) new-sub_node)
;        (send self :needs-computing t))
;  (slot-value 'sub_node))

(defmeth information-model-proto :test (&optional new-test)
  (when new-test
        (setf (slot-value 'test) new-test)
        (send self :needs-computing t))
  (slot-value 'test))

(defmeth information-model-proto :menu-title (&optional new-menu-title)
  (when new-menu-title
        (setf (slot-value 'menu-title) new-menu-title)
        (send self :needs-computing t))
  (slot-value 'menu-title))

(defmeth information-model-proto :tree ()
  (let*(
        (node (send self :node))
        )
  (tree node)))

(defmeth information-model-proto :basic-information ()
  (let*(
        (node (send self :node))
        (l-node (length node))
        (intermediate-nodes (send self :intermediate-nodes))
        (terminal-nodes (send self :terminal-nodes))
        )
    (format t "~%====================================")
    (format t "~2%  total # of nodes       : ~s" l-node)
    (format t "~2%  intermediate nodes are : ~s" intermediate-nodes)
    (format t "~2%  terminal nodes are     : ~s" terminal-nodes)
    (format t "~%====================================~2%")
    ))

(defmeth information-model-proto :node-information ()
  (let*(
        (n1 (car (get-value-dialog "Which node do you want ?" )))
        (node (send self :node))
         plot
        (color-list (repeat (list (nth 2 *colors*) (nth 5 *colors*)
                                  (nth 7 *colors*) (nth 3 *colors*)
                                  (nth 6 *colors*) (nth 4 *colors*)
                                  (nth 0 *colors*)) 4))
        )
         (format t "~%===========================================")
         (format t "~%  Information at node # ~s" n1)
         (format t "~%  nobs :  ~s" (length (send (nth n1 node) :case)))
         (when (send (nth n1 node) :terminal)
               (format t "~2%  This is a terminal node.")
               (format t "~%  R-squared at this node :   ~s"
                               (send (nth n1 node) :r2))
               (format t "~%  error-reduced-rate at this node :   ~s"
                               (send (nth n1 node) :error-ratio))
               (format t "~%  the value of F statistics at this node :   ~s"
                               (send (nth n1 node) :f-test-val))
               (format t "~%  the value of F-test at this node :   ~s"
                               (send (nth n1 node) :f-v))
         (format t "~%===========================================~%")
         )
         (unless (send (nth n1 node) :terminal)
                 (format t "~2% This is an intermediate node.")
                 (format t "~2% the p-values at this node :")
                 (format t "~2% ~s" (send (nth n1 node) :p-vals))
                 (format t "~2% the companion output eigenvalues of pHd :")
                 (format t "~2% ~s" (send (nth n1 node) :phd-eval))
                 (format t "~2% pHd direction used at this node is :")
                 (format t "~2% ~s" (cdr (send (nth n1 node) :phd-evct)))
                 (format t "~2% cut-point for this node is :")
                 (format t "~2% ~s" (car (send (nth n1 node) :phd-evct)))
                 (format t "~2% project-variable in pHd at this node :")
                 (format t "~2% ~s" (send (nth n1 node) :phd-proj-var))
                 (format t "~2% splitted node :")
                 (format t "~2% ~s" (send (nth n1 node) :case-list))
                 (format t "~2% the r-squared values of splitted subnodes :")
                 (format t "~2% ~s" (send (nth n1 node) :r2-list))
                 (format t "~2% the sigma-hat values of splitted subnodes :")
                 (format t "~2% ~s" (send (nth n1 node) :sh-list))
                 (format t "~% R-squared at this node :   ~s"
                                (send (nth n1 node) :r2))
                 (format t "~% error-before at this node :        ~s"
                                (send (nth n1 node) :error-before))
                 (format t "~% error-after split at this node  :  ~s"
                                (send (nth n1 node) :error-after))
                 (format t "~% error-reduced-rate :  ~s"
                                (send (nth n1 node) :error-ratio))
                 (format t "~% the value of F-statistics :   ~s"
                                (send (nth n1 node) :f-test-val))
                 (format t "~% the value of F-test :   ~s"
                                (send (nth n1 node) :f-v))
         (format t "~%===========================================~%")
          )
                 (setf plot(spin-plot (list (send (nth n1 node) :p1x)
                                            (send (nth n1 node) :res)
                                            (send (nth n1 node) :p2x))
                           :variable-labels (list "phd1" "residuals" "phd2")
                           :title "pHd1-res-pHd2"
                           :location '(618 310) :size '(280 160)))
                 (send plot :linked t)
         ))

(defmeth information-model-proto :weighted-error ()
  (let*(
        (node (send self :node))
        (l-node (length node))
        (l-error nil)
        (l-subcase nil)
         w-error
        )
    (dotimes (i (length node))
             (when (send (nth i node) :terminal)
                   (setf l-error(append l-error (list (send (nth i node) 
                                                 :error-before))))
                   (setf l-subcase(append l-subcase (list (length (send 
                                                 (nth i node) :case)))))
                 ))
   (setf w-error(sq-sum l-error l-subcase))
   (format t "~%==========================================================")
   (format t "~2% total # of nodes           : ~s" l-node)
   (format t "~2% terminal nodes' errors are : ~s" l-error)
   (format t "~2% terminal nodes' nobs are   : ~s" l-subcase)
   (format t "~2% weighted error is          : ~s" w-error) 
   (format t "~%==========================================================~2%")
    ))

(defmeth information-model-proto :modify-tree ()
  (let*(
        (num (nth 0 (get-value-dialog "Which node do you want to rebuild ?" )))
        (case (send (nth num (send self :node)) :case))
        (x (send self :x))
        (y (send self :y))
        (data (cons y x))
        (data1 (transpose (select (transpose data) case)))
        )
    (setf modified-node (phd-rt (cdr data1) (car data1)))
    modified-node))


(defmeth information-model-proto :menu ()
  (setf menu-title (send self :menu-title))
  (if menu-title
      (setf regression-menu (send menu-proto :new menu-title))
      (setf regression-menu (send menu-proto :new "Regression"))
      )
  (setf tree-item
        (send menu-item-proto :new "Show Tree"
              :action #'(lambda ()
                          (send self :tree))))

;  (setf smooth-item
;        (send menu-item-proto :new "Smooth"
;              :action #'(lambda ()
;                          (send self :smooth))))
  (setf basic-item
        (send menu-item-proto :new "Basic Information"
              :action #'(lambda ()
                          (send self :basic-information))))
  (setf node-item
        (send menu-item-proto :new "Node Information"
              :action #'(lambda ()
                          (send self :node-information))))
 
  (setf weighted-error-item
        (send menu-item-proto :new "Weighted Error"
              :action #'(lambda ()
                          (send self :weighted-error))))

  (setf modify-item
        (send menu-item-proto :new "Modify Tree"
              :action #'(lambda ()
                          (send self :modify-tree))))
  (setf dash-item-1 (send dash-item-proto :new))
  (setf remove-item
        (send menu-item-proto :new "Remove Menu"
              :action #'(lambda ()
                          (send regression-menu :remove))))

  (send regression-menu :append-items tree-item 
;smooth-item 
      basic-item node-item weighted-error-item modify-item 
      dash-item-1 remove-item)
  (send regression-menu :install)
  )

;(defmeth information-model-proto :smooth ()
;  (let*(
;        (x  (send self :x))
;        (y  (send self :y))
;        (node (send self :node))
;        (terminal-nodes (send self :terminal-nodes))
;        (sub_node (send self :sub_node))
;        )
;   (smooth x y node terminal-nodes sub_node)
;    ))



(defmeth information-model-proto :modify-tree-1 (num)
  (let*(
        (case (send (nth num (send self :node)) :case))
        (x (send self :x))
        (y (send self :y))
        (data  (cons y x))
        (data1 (transpose (select (transpose data) case)))
        )
    (setf modified-node (phd-rt (cdr data1) (car data1)))
    modified-node))

(defmeth information-model-proto :node-information-1 (num1)
  (let*(
;        (inf-level (1+ (choose-item-dialog
;                          "What kind of information do you want:"
;                          '(" 1 : plot(s) only."
;                            " 2 : basic information only."
;                            " 3 : SIR and (or) SIRII directions used."
;                            " 1 & 2."
;                            " 1 & 3."
;                            " 1 & 2 & 3."
;                            " 2 & 3."
;                            " All information." ))))
;        (x (send self :x))
;        (y (send self :y))
        (node (send self :node))
        (n1 (car num1))
        plot
        (color-list (repeat (list (nth 2 *colors*) (nth 5 *colors*) 
                    (nth 7 *colors*) (nth 3 *colors*) (nth 6 *colors*) 
                    (nth 4 *colors*) (nth 0 *colors*)) 4))
        )
     (format t "~%======================================================")
            (format t "~%  information at node # ~s" n1)
            (format t "~%  nobs :  ~s" (length (send (nth n1 node) :case)))
            (when (send (nth n1 node) :terminal)
               (format t "~2%  This is a terminal node.")
               (format t "~%  R-squared at this node :   ~s"
                               (send (nth n1 node) :r2))
               (format t "~%  error-reduced-rate at this node :   ~s"
                               (send (nth n1 node) :error-ratio))
               (format t "~%  the value of F statistics at this node :   ~s"
                               (send (nth n1 node) :f-test-val))
               (format t "~%  the value of F-test at this node :   ~s"
                               (send (nth n1 node) :f-v))
     (format t "~%======================================================~%")
               )

         (unless (send (nth n1 node) :terminal)
                 (format t "~2% This is an intermediate node.")
                 (format t "~2% the p-values at this node :")
                 (format t "~2% ~s" (send (nth n1 node) :p-vals))
                 (format t "~2% the companion output eigenvalues of pHd :")
                 (format t "~2% ~s" (send (nth n1 node) :phd-eval))
                 (format t "~2% pHd direction used at this node is :")
                 (format t "~2% ~s" (cdr (send (nth n1 node) :phd-evct)))
                 (format t "~2% cut-point for this node is :")
                 (format t "~2% ~s" (car (send (nth n1 node) :phd-evct)))
                 (format t "~2% project-variable in pHd at this node :")
                 (format t "~2% ~s" (send (nth n1 node) :phd-proj-var))
                 (format t "~2% splitted node :")
                 (format t "~2% ~s" (send (nth n1 node) :case-list))
                 (format t "~2% the r-squared values of splitted subnodes :")
                 (format t "~2% ~s" (send (nth n1 node) :r2-list))
                 (format t "~2% the sigma-hat values of splitted subnodes :")
                 (format t "~2% ~s" (send (nth n1 node) :sh-list))
                 (format t "~% R-squared at this node :   ~s"
                                (send (nth n1 node) :r2))
                 (format t "~% error-before at this node :        ~s"
                                (send (nth n1 node) :error-before))
                 (format t "~% error-after split at this node  :  ~s"
                                (send (nth n1 node) :error-after))
                 (format t "~% error-reduced-rate :  ~s"
                                (send (nth n1 node) :error-ratio))
                 (format t "~% the value of F-statistics :   ~s"
                                (send (nth n1 node) :f-test-val))
                 (format t "~% the value of F-test :   ~s"
                                (send (nth n1 node) :f-v))
         (format t "~%===========================================~%")
                 )
               (setf plot(spin-plot (list (send (nth n1 node) :p1x) 
                                          (send (nth n1 node) :res)
                                          (send (nth n1 node) :p2x))
                          :variable-labels (list "phd1" "residuals" "phd2")
                          :title "pHd1-res-pHd2"
                          :location '(618 310) :size '(280 160)))
                     (send plot :linked t)
    ))
