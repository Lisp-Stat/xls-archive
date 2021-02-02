;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Binary Search Function
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun binary-search (search-cum-list data-list rr)
"Args:  SEARCH-CUM-LIST DATA-LIST RR
SEARCH-CUM-LIST is a cumulative-list of length n+1 where DATA-LIST is a 
list of length n.  SEARCH-CUM-LIST serves as bin markers.  Returned is the 
ith element selected from DATA-LIST where i is the bin (0 based) that RR
falls into SEARCH-CUM-LIST."
  (let* (
         (len (length search-cum-list))
         (start-point (ceiling (/ len 2)))
         (step-size (ceiling (/ len 4)))
         (start nil)
         (alt nil)
         (below nil)
          yy 
        )
     (loop
         (cond ((and alt (>= rr (elt search-cum-list start-point)))
                 (setf yy (elt data-list start-point))
                 (return))
               ((and alt (< rr (elt search-cum-list start-point)))
                 (setf yy (elt data-list (1- start-point)))
                 (return))
               ((>= rr (elt search-cum-list start-point))
                 (if start
                      (if (and below (= step-size 1)) (setf alt t))
                      (setf start t))
                 (setf below nil)
                 (setf step-size (ceiling (/ step-size 2)))
                 (setf start-point (+ start-point step-size)))
               ((< rr (elt search-cum-list start-point))
                 (if start
                     (if (and (not below) (= step-size 1)) (setf alt t))
                     (setf start t))
                 (setf below t)
                 (setf step-size (ceiling (/ step-size 2)))
                 (setf start-point (- start-point step-size))))) yy))


