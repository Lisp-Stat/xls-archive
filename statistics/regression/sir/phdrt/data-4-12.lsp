   (def node (make-list 12))
   (def level-list'(0 1 1 2 2 2 2 2 3 3 3 3))
   (def children-list (list '(1 2) '(3 4 5) '(6 7) '(8 9) nil '(10 11) nil 
                            nil nil nil nil nil))
   (def terminal-list '(nil nil nil nil t nil t t t t t t))
 

  (dotimes (i (length node))
;(print (nth i node))
(print (nth i level-list))
(print (nth i children-list))
(print (nth i terminal-list))
(print '***)
           (setf (nth i node) (phd-class-model (nth i level-list) 
                              (nth i children-list) (nth i terminal-list)))
           )
