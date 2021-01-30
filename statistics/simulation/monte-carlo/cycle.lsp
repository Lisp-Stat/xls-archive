(defun cycle (flist xlist)
"Args: (flist xlist)
Updates elements of the list XLIST by applying functions
from FLIST to each element in turn (while keeping the
other elements fixed). Can be used for cyclic
coordinate ascend and for Gibbs sampling."
(let (
     (n (length xlist))
     )
(dotimes (i n)
         (setf (elt xlist i) (funcall (elt flist i) (elt xlist i))))
))
