   (def node (make-list 7))
   (def level-list '(0 1 1 2 2 3 3))
   (def children-list (list '(1 2) '(3 4) nil nil '(5 6) nil nil))
   (def terminal-list '(nil nil t t nil t t))
 

  (dotimes (i 7)
(print (nth i level-list))
(print (nth i children-list))
(print (nth i terminal-list))
(print '***)

           (setf (nth i node) (phd-class-model (nth i level-list) 
                      (nth i children-list) (nth i terminal-list)))
           )
