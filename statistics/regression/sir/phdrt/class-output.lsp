 (defun class-output (node)
   (let* (
         (node-length (length node))
         (color-list  (list (nth 2 *colors*) (nth 5 *colors*) (nth 7 *colors*) 
                            (nth 3 *colors*)
                            (nth 6 *colors*) (nth 4 *colors*) (nth 0 *colors*)
                            (nth 2 *colors*) (nth 5 *colors*) (nth 7 *colors*) 
                            (nth 3 *colors*)
                            (nth 6 *colors*) (nth 4 *colors*) (nth 0 *colors*)
                            (nth 2 *colors*) (nth 5 *colors*) (nth 7 *colors*) 
                            (nth 3 *colors*)
                            (nth 6 *colors*) (nth 4 *colors*) (nth 0 *colors*)))
         intermediate-nodes
         terminal-nodes
;        pre-terminal
;        sub_node
        )
    (dotimes (i node-length)
             (if (send (nth i node) :terminal)
                 (setf terminal-nodes (append terminal-nodes (list i)))
                 (setf intermediate-nodes (append intermediate-nodes (list i)))
                 ))
;   (dolist (i terminal-nodes)
;    (setf pre-terminal(append pre-terminal (list (send (nth i node) :parent))))
;           )
;   (setf sub_node(remove-duplicates pre-terminal))

       (dotimes (i node-length)
           (format t "~%=================================================")
                (format t "~%  # of node :  ~s" i)
                (format t "~%  nobs :  ~s" (length (send (nth i node) :case)))
                (when (send (nth i node) :terminal)
                  (format t "~2%  This is a terminal node")
                  (format t "~%  R-squared at this node :   ~s"
                                 (send (nth i node) :r2))
                  (format t "~%  Error-reduced-rate at this node :   ~s"
                                 (send (nth i node) :error-ratio))
                  (format t "~%  the value of F statistics at this node :   ~s"
                                 (send (nth i node) :f-test-val))
                  (format t "~%  the value of F-test at this node :   ~s"
                                 (send (nth i node) :f-v))
           (format t "~%=================================================~%")
                         )
           (unless (send (nth i node) :terminal)
                 (format t "~2% This is an intermediate node.")
                 (format t "~2% the p-values at this node :")
                 (format t "~2% ~s" (send (nth i node) :p-vals))
                 (format t "~2% the companion output eigenvalues of pHd :")
                 (format t "~2% ~s" (send (nth i node) :phd-eval))
                 (format t "~2% pHd direction used at this node is :")
                 (format t "~2% ~s" (cdr (send (nth i node) :phd-evct)))
                 (format t "~2% cut-point for this node is :")
                 (format t "~2% ~s" (car (send (nth i node) :phd-evct)))
                 (format t "~2% project-variable in pHd at this node :")
                 (format t "~2% ~s" (send (nth i node) :phd-proj-var))
                 (format t "~2% splitted node :")
                 (format t "~2% ~s" (send (nth i node) :case-list))
                 (format t "~2% the r-squared values of splitted subnodes :")
                 (format t "~2% ~s" (send (nth i node) :r2-list))
                 (format t "~2% the sigma-hat values of splitted subnodes :")
                 (format t "~2% ~s" (send (nth i node) :sh-list))
                 (format t "~% R-squared at this node :   ~s"
                                (send (nth i node) :r2))
                 (format t "~% error-before at this node :        ~s"
                                (send (nth i node) :error-before))
                 (format t "~% error-after split at this node  :  ~s"
                                (send (nth i node) :error-after))
                 (format t "~% error-reduced-rate :  ~s"
                                (send (nth i node) :error-ratio))
                 (format t "~% the value of F-statistics :   ~s"
                                (send (nth i node) :f-test-val))
                 (format t "~% the value of F-test :   ~s"
                                (send (nth i node) :f-v))
          (format t "~%=================================================~%")
                   )
         )
;    (list intermediate-nodes terminal-nodes sub_node)
    (list intermediate-nodes terminal-nodes)
    ))
