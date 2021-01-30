; file goldsear.lsp
; golden-search algorithm implemented in xlisp
; because some implementations of xlispstat lack it
; f. udina may 93


#|		(golden-search #'fun 
			       (first ends)
			       (second ends)
			       :tolerance tolerance
			       :verbose verbose)))

> (help 'golden-search)
loading in help file information - this will take a minute ...done
GOLDEN-SEARCH                                                   [function-doc]
Args: (f a b &key start (tolerance .000001) (verbose nil))
F is a real valued function of one argument, A and B are real numbers.
Uses a golden section search to locate the minimum of F. Convergence
occurs roughly when the change in X is less than tolerance * (1 + x).
Returns list of the form (X FX), with FX = (F X). Prints iteration
information if VERBOSE is not NIL.
NIL



|#

(provide "gold-sear")

(defun golden-section (a b)
"returns the two-points golden section of the a b segment"
(let ((gr (* (- b a) .61803)))
 (list (- b gr) (+ a gr))))

(defun golden-search (fun from to &key start (tolerance 0.01) (verbose nil))
"fake golden-search for mac xlispstat"
(if (<= (- to from) tolerance)
    (list from (funcall fun from))
    (let* ((gs (golden-section from to))
           (ygs (mapcar fun gs)))
      (when verbose
            (print (list from gs to ygs)))
      (if (apply #'< ygs)
          (golden-search fun from (second gs)
                         :start start :tolerance tolerance :verbose verbose)
          (golden-search fun (first gs) to
                         :start start :tolerance tolerance
                         :verbose verbose)))))          
