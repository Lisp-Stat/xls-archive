(provide 'density)
;
;MEG 8/91
;This function sets up the Kernel density plot to show where the
;  data falls.
;
(defun DENSITY ()
  (setf den (send graph-proto :new 2))
  (send den :size 180 160)
  (send den :location 285 20)
  (send den :x-axis t t 4) (send den :y-axis t t 4)
  (send den :variable-label '(0 1) (list "Values" "Approximate Frequency"))
  (send den :add-lines (list (first (kernel-dens covar)) (second (kernel-dens covar))))
  (send den :title "Kernel Density")
  (send den :use-color t)
  (let ((n (send den :num-lines)))
    (send den :linestart-color (iseq 0 (- n 1)) 'red)
    (send den :add-lines (list (list 2 2) (list 0 2.5)))
    (send den  :linestart-color (iseq n (+ n 1)) 'blue)
    (send den :adjust-to-data)
  
    (defmeth den :adjust-to-density (x) 
      (let* ((xcoord (repeat x 2)))
        (dotimes (i 2)
                 (send self :linestart-coordinate 0 (+ i n) (elt xcoord i)))
        (send self :redraw-content)))))
