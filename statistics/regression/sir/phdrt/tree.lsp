(defun tree(node)
  (let* (
         (node-length (length node))
         (no-terminal 0)
         (loop-no-terminal 0)
         y-axis-list
         level-list
         subnode
         subnode1
         subnode2
         test-flag
         (aa 0)
         (jump 0)
         (dd 0)
         loop-x-axis
         x-axis-list
         (l-sub 0)
         (c 4)
        )
    (dotimes (i node-length)
             (when (eql T (send (nth i node) :terminal))
                   (setf no-terminal (+ no-terminal 1))
             ))

    (dotimes (i node-length)
             (setf loop-y-axis (- (send (nth i node) :level)))
             (setf y-axis-list (append y-axis-list (list loop-y-axis)))
             )
             (setf level-list(abs y-axis-list))
             (setf max-level-list(max level-list))
;(print level-list)

    (dotimes (i max-level-list)
             (setf subnode (which (= i level-list)))
             (setf l-subnode (length subnode))
             (setf subnode1(which (= (+ i 1) level-list)))
             (setf l-subnode1 (length subnode1))
             (when (= i 0)
                   (dotimes (j l-subnode1)
                            (setf loop-x-axis(- (/ (* 8 (+ j 1))
                                             (+ l-subnode1 1)) 4))
                            (setf x-axis-list(append x-axis-list
                                             (list loop-x-axis)))
                            )
             )
;(print (list 'i i))
             (unless (= i 0)
                     (setf subnode2(which (= (- i 1) level-list))) 
                     (setf l-subnode2(length subnode2))
                     (when (eql T (send (nth (car subnode2) node) :terminal))
                        (do*
                        ((count 0 (setq count (+ 1 count))))
                        ((eql T test-flag))
                        (dotimes (nc l-subnode2)
                              (unless (eql T (send (nth (nth nc subnode2) 
                                              node) :terminal))
                                      (setf test-flag T)
                                      )
                              (when (eql T (send (nth (nth nc subnode2) 
                                              node) :terminal))
                                      (setf aa (+ aa 1))
                                      )
                              )
                         )
                      )
                      (setf test-flag nil)
                      (setf jump (+ jump aa))
                      (dolist (k subnode)
                            (when (eql T (send (nth k node) :terminal))
                                  (setf loop-no-terminal(+ loop-no-terminal 1))
                                  )
                            )
                       (dolist (l subnode)
                          (when (eql T (send (nth l node) :terminal))
                                (setf dd(+ dd 1))
                                )
                          (unless (eql T (send (nth l node) :terminal))
                                  (setf l-sub(length (send (nth l node) 
                                                      :children)))
                                (dotimes (j l-sub)
                                  (setf loop-x-axis(- (/ (* 8 (+ j dd jump 1))
                                         (+ loop-no-terminal l-subnode1 1)) c))
                                  (setf x-axis-list(append x-axis-list
                                         (list loop-x-axis)))
                                  )
                          (setf c (- loop-x-axis))
                          (setf dd 0)
                          )
                       )
                       (setf c 4)
                       (setf dd 0)
             )
    )
             (setf x-axis-list (cons 0 x-axis-list))
;(print x-axis-list)
;(print y-axis-list)

   (def tree-plot-1 (plot-points (append (list 0 5.2) x-axis-list (list -5))
        (append (list 0.2 0.2) y-axis-list (list (- (min y-axis-list) 0.2)))
                             :location (list 402 36)
                             :size (list 240 300)))

    (dotimes (i node-length)
             (unless (send (nth i node) :terminal)
                     (setf child-list (send (nth i node) :children))
                     (setf no-child (length child-list))
                     (dotimes (j no-child) (send tree-plot-1 :add-lines
                              (list (nth i x-axis-list)
                                    (nth (nth j child-list) x-axis-list))
                              (list (nth i y-axis-list)
                              (nth (nth j child-list) y-axis-list))
                                    ))))
 
    (send tree-plot-1 :clear-points) 
    (send tree-plot-1 :add-points x-axis-list y-axis-list) 
    (send tree-plot-1 :title "Regression Tree") 
    (send tree-plot-1 :showing-labels T) 
    (send tree-plot-1 :selection (iseq node-length)) 
    (send tree-plot-1 :x-axis nil t 0 :draw nil) 
    (send tree-plot-1 :y-axis nil t 0 :draw nil) 

    (defmeth tree-plot-1 :do-click (x y m1 m2)
      (let* ((cr (send self :click-range))
             (p (first (send self :points-in-rect
                             (- x (round (/ (first cr) 2)))
                             (- y (round (/ (second cr) 2)))
                             (first cr)
                             (second cr))))
            )
             (cond (m1 (send phdrt-out :modify-tree-1 p))
                   (m2 (send phdrt-out :modify-tree-1 p))
                   (t  (send phdrt-out :node-information-1 (list p)))
              )
    ))
 ))
