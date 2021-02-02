;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;
;;;  Code to do Mann-Whitney and Wilcoxon rank signed rank tests.
;;;
;;;  6/19/95 -- Jason Bond (jbond@stat.ucla.edu)
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun avg-rank (data)
  (let* ((ranklist (1+ (rank data)))
         (l1 (map-elements #'(lambda (x y) (which (= x y)))
                           data (repeat (list data) (length data))))
         (l2 (select l1 (which (> (mapcar #'length l1) 1))))
         (l3 (remove-duplicates l2 :test #'equalp)))
    (mapcar #'(lambda (x)
                (let* ((lx (length x))
                       (nx (/ (1- lx) 2))
                       (start (elt ranklist (first x))))
                  (setf (select ranklist x) (repeat (+ start nx) lx)))) l3)
    ranklist))

(defun m-w-test (l1 l2)
  "Args: (l1 l2)
Mann-Whitney test of H0: mu1=mu2
for sequence l1 versus sequence l2."
  (let* ((n1 (length l1))
         (n2 (length l2))
         (nmin (min n1 n2))
         (biglist (combine l1 l2))
         (ranklist (avg-rank biglist))
         (r nil))
    (setf ranklist (list (select ranklist (iseq n1))
                         (select ranklist (iseq n1 (1- (+ n1 n2))))))
    (setf r (if (>= n1 n2) (sum (first ranklist))
                (sum (second ranklist))))
    (min (abs (list r (- (* nmin (1- (+ n1 n2))) r))))))


(defun wilcoxon-test (l1 h0)
  "Args: (l1 l2)
Wilcoxon rank sum test of h0 for sequence l1."
  (sum (select (avg-rank (abs (- l1 h0))) (which (> (- l1 h0) 0)))))


