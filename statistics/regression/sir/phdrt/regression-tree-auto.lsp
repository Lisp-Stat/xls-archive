(defun regression-tree-auto(x y)
   (let* (
; nobs        -  total # of observations
; dim         -  total # of variables
; node        -  the list of all node object to store all information
; nc          -  node count
; loop-case   -  the list of cases label of observations at this node
; loop-y-list -  the list of index for cases at this node (loop)
; loop-x-list -  a list of lists of indep. variables for cases at this node
; loop-error-before -  error before splitting
; loop-error-after  -  error after splitting
; loop-child-case -  list of cases to be assigned to each child
; loop-no-child   -  # of children at this node
; test-flag       -  T : end loop, NIL : continue loop
; node-count      - a list for all node labels being used
; l-node-count    - length of node-count

       (nobs (length y))
       (dim  (length x))
       (case (list (iseq nobs)))
       (node (repeat (list nil) 40))
       (alpha(nth 0 (get-value-dialog "fractional reduction in weighted sigma-hat:":initial .1)))
       reg1
       phd-var
       f-test
       (loop-r2-list nil)
       (loop-sh-list nil)
       (node-count (list 0))
       (l-node-count 0)
       (do-list   (list (list 0 0)))
       test-flag
       c
;       (menu-title (get-string-dialog "Please name the Regression menu:"
;                                      :initial "Regression" ))
       (menu-title data-file)
       class-out-info
       tree-node
       node-out
       )

;       (out-put-option (- (+ 1 (choose-item-dialog
;                    "Out put options :"
;                    '("No output."
;                      "Print only final result."
;                      "Print information at each node."
;                      "Print and plot information at each node."))
;                        ) 1))
  (do* 
   ((count 0 (setq count (+ 1 count))))
   ((eql T test-flag))
   (setf cc 0)
   (setf dd 0)
   (setf child-label nil)
   (setf child-label1 nil)
   (setf loop-terminal nil)
   (setf no-terminal 0)
   (setf parent-list nil)
   (setf loop-child-case1 nil)
   (setf lp nil)

 (dolist (l-p do-list)
 (print l-p)
   (setf loop-ancestor-nodes nil)
   (setf loop-case nil)
   (setf loop-x-list nil)
   (setf loop-y-list nil)
   (setf loop-phd-out nil)
   (setf loop-child-case nil)
   (setf loop-no-child nil)
   (setf loop-f-test nil)
   (setf loop-f-v nil)
   (setf loop-error-ratio nil)
   (setf nc (car l-p))
   (print (list 'case-length (length (nth nc case))))

   (setf (nth nc node) (phd-class-model (nth 1 l-p) (nth nc case)))

         (setf loop-case (send (nth nc node) :case))
         (setf loop-x-list (transpose (select (transpose x) loop-case)))
         (setf loop-y-list (select y loop-case))
         (send (nth nc node) :node-label nc)

   (when (= nc 0)
         (send (nth nc node) :ancestor nil)
         (send (nth nc node) :level 0)
         (setf reg1(reg-model loop-x-list loop-y-list :print nil))
         (setf temp-r2 (send reg1 :r-squared))
         (setf temp-sh (send reg1 :sigma-hat))
         (send (nth nc node) :r2 temp-r2)
         (send (nth nc node) :error-before temp-sh) 
         (setf loop-r2-list (list temp-r2))
         (setf loop-sh-list (list temp-sh))
         )
   (unless (= nc 0)
           (setf loop-ancestor-nodes (append (list 
                      (send (nth nc node) :parent))
                      (send (nth (send (nth nc node) :parent) node) 
                                          :ancestor)))
           (send (nth nc node) :ancestor loop-ancestor-nodes)
           (send (nth nc node) :level (length loop-ancestor-nodes))
           (send (nth nc node) :r2 (nth nc loop-r2-list))
           (send (nth nc node) :error-before (nth nc loop-sh-list))
           )

   (when (or (< (length loop-case) 40) 
             (>= (send (nth nc node) :r2) .90))
             (send (nth nc node) :terminal T))

   (unless (send (nth nc node) :terminal)
           (setf phd-var (car (proj-var loop-x-list loop-y-list)))
           (setf loop-phd-out (phdrt-model (select loop-x-list phd-var)
                                           loop-x-list loop-y-list :print nil))

           (send (nth nc node) :res (send loop-phd-out :resid))
           (send (nth nc node) :p1x (send loop-phd-out :phd1))
           (send (nth nc node) :p2x (send loop-phd-out :phd2))
           (send (nth nc node) :phd-proj-var phd-var)
           (send (nth nc node) :phd-evct (car (send loop-phd-out :directions)))
           (send (nth nc node) :phd-eval (send loop-phd-out :eigen-values))
           (send (nth nc node) :p-vals   (send loop-phd-out :p-val))
           (send (nth nc node) :r2-list (send loop-phd-out :r-squares))
           (send (nth nc node) :sh-list (send loop-phd-out :sig-hat))
           (send (nth nc node) :case-list (send loop-phd-out :node-size))
           (send (nth nc node) :error-after (send loop-phd-out :t-sighat2))
           (setf loop-f-test (send loop-phd-out :f-test))
           (send (nth nc node) :f-test-val loop-f-test)
           (setf loop-f-v (f-quant .999 (nth 1 loop-f-test)(nth 2 loop-f-test)))
           (send (nth nc node) :f-v loop-f-v)
           (setf loop-error-ratio (/ (- (send (nth nc node) :error-before)
                                        (send (nth nc node) :error-after))
                                     (send (nth nc node) :error-before)))
           (send (nth nc node) :error-ratio loop-error-ratio)

           (when (or (<= (car loop-f-test) loop-f-v)
                     (<  loop-error-ratio alpha))
;                     (<= (* (- 1 alpha) (send (nth nc node) :error-before)) 
;                                        (send (nth nc node) :error-after))) 
                 (send (nth nc node) :terminal T))

;     (print (list 'F-test test-f (f-quant .999 (nth 1 test-f) (nth 2 test-f))))
;     (print (list 'error-reduced-rate (/ (- (send (nth nc node) :error-before)
;                                            (send (nth nc node) :error-after))
;                                         (send (nth nc node) :error-before))))
        )

   (unless (send (nth nc node) :terminal)
;          (send (nth nc node) :phd-proj-var phd-var)
;          (send (nth nc node) :phd-evct (car (send loop-phd-out :directions)))
;          (send (nth nc node) :phd-eval (send loop-phd-out :eigen-values))

        (setf loop-r2-list(append loop-r2-list (send loop-phd-out :r-squares)))
        (setf loop-sh-list(append loop-sh-list (send loop-phd-out :sig-hat)))
           (setf loop-child-case (send loop-phd-out :case-2))
           (send (nth nc node) :child-case loop-child-case)
           (setf loop-child-case1(append loop-child-case1 loop-child-case))

           (setf loop-no-child (length loop-child-case))
           (when (= nc 0)
                 (send (nth nc node) :children (iseq 1 loop-no-child))
                 )
           (when (/= nc 0)
                 (send (nth nc node) :children
                       (iseq l-node-count (+ l-node-count loop-no-child -1)))
                 ) 
           (setf node-count (append node-count (send (nth nc node) :children)))
;           (print (list 'node-count node-count))
           (setf l-node-count (length node-count))
           (setf child-label1 (send (nth nc node) :children)) 
           (setf parent-list (append parent-list
                                     (repeat nc (length child-label1))))
           (setf child-label(append child-label child-label1))
          )

     (setf loop-terminal(append loop-terminal 
                                (list (send (nth nc node) :terminal))))
    ) 
;(print (list 'count count))
;(print '===)
;(print (list 'loop-terminal loop-terminal))

      (setf case(append case loop-child-case1))
      (when (= nc 0)
            (when (eql T (send (nth nc node) :terminal))
                  (setf test-flag T))
            (unless (eql T (send (nth nc node) :terminal))
                    (setf lp (repeat 0 (length child-label)))
                    (dotimes (i (length lp))
                        (setf c(list (nth i child-label)(nth i parent-list)))
                        (setf (nth i lp) c)
                        ) 
            )
      )
      (unless (= nc 0)
          (setf dd(set-difference node-count child-label))
          (dolist (i dd)
                   (when (= (send (nth i node) :level) count)
                         (setf cc(+ cc 1))
                         (when (eql T (send (nth i node) :terminal))
                               (setf no-terminal (+ no-terminal 1))
                         )
                   )
           )
          (when (= no-terminal cc) (setf test-flag T))
          (unless (= no-terminal cc)
                  (setf lp (repeat 0 (length child-label)))
                  (dotimes (i (length lp))
                       (setf c(list (nth i child-label) (nth i parent-list)))
                       (setf (nth i lp) c)
                       )
           )
      )
(print (list 'label-parent lp))
(print '*****)
      (setf do-list lp)
  )

  (def node(remove nil node))
  (setf class-out-info (class-output node))
  (setf tree-node(tree node))
  (def  node-out (information-model x y node (nth 0 class-out-info) 
                                   (nth 1 class-out-info) t menu-title))
  (send node-out :menu)
   node-out))
