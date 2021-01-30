(provide 'exp-rand)

; ENA - 3/5/91.
;

(defun EXP-RAND (lambda)
  (/ (- (log (uniform-rand (length lambda)))) lambda))
