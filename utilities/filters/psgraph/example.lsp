   (load "psgraph")
   (setf psgraph:*boxkind* "fill")
   (setf psgraph:*boxgray* ".8")
   (setf psgraph:*fontsize* 8)
   (setf psgraph:*second-fontsize* 6)

   (defun graph (&optional (shrink t))
     (with-open-file (*standard-output* "example.ps"
		      :direction :output
		      :if-exists :supersede)
	  (psgraph:psgraph 'A #'children #'info shrink nil #'eq)))

   (defun children (x)
      (cond ((eq x 'A) '(B C D))
	    ((member x '(B C D)) '(E F G))
	    ((member x '(E F G)) '(H))
	    (t nil)))

   (defun info (x)
      (list (string x)))

   (graph)
