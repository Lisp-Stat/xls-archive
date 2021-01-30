;; Jan de Leeuw, April 27, 1995

(defun lexico-sort (x)
"Args: x
Sorts a list of lists according to length, and
lists of the same length lexicographically with
respect to <."
  (sort x #'lexico-compare)
)

(defun lexico-compare (a b)
(cond 
  ((< (length a) (length b)) t)
  ((> (length a) (length b)) nil)
  (t (cond  
       ((< (first a) (first b)) t)
       ((> (first a) (first b)) nil)
       (t (lexico-compare (rest a) (rest b))))))
)

(provide "lexico-sort")