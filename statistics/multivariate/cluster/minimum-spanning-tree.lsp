;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; mst - calculate the minimum spanning tree
;
; from the paper "Minimum Spanning Trees and Single Linkage Cluster Analysis"
; by J. C. Gower & G. J. S. Ross, Applied Statistics, volume 18, 1969,
; pp. 54-64
;
; written by David A. Betz, UCLA Program In Social Statistics, 1995
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun mst (distances)
  (dotimes (i (array-dimension distances 0) nil)
    (dotimes (j (array-dimension distances 1) nil)
      (cond ((eq i j) (setf (aref distances i j) 0))
            ((not (numberp (aref distances i j)))
             (cond ((numberp (aref distances j i))
                    (setf (aref distances i j) (aref distances j i)))
                   (t (error "mst: distances is non-numerical symmetric")))))))
  (let ((internal-points '(0))
        (external-points (cdr (iseq (array-dimension distances 0))))
        (links nil)
        (connected-to (make-array (array-dimension distances 0)
                       :initial-element nil))
        (closest-internal-distance (make-array (array-dimension distances 0)
                                    :initial-element (+ 1 (max distances))))
        (closest-internal-point (make-array (array-dimension distances 0)
                                 :initial-element nil))
        (max-distance (+ 1 (max distances)))
        next-point
        next-distance
       )
    (dotimes (i (- (array-dimension distances 0) 1) nil)
      (setf next-distance max-distance)
      (setf next-point nil)
      (dolist (j external-points nil)
        (cond ((< (aref distances (car internal-points) j)
                  (aref closest-internal-distance j))
               (setf (aref closest-internal-distance j)
                     (aref distances (car internal-points) j))
               (setf (aref closest-internal-point j)
                     (car internal-points))))
        (cond ((< (aref closest-internal-distance j) next-distance)
               (setf next-distance (aref closest-internal-distance j))
               (setf next-point j))))
      (setf links
            (cons (list next-point (aref closest-internal-point next-point))
                  links))
      (setf internal-points (cons next-point internal-points))
      (setf external-points (remove next-point external-points)))
    links))

(setf shrews
  #2a((  nil   nil   nil   nil   nil   nil   nil   nil   nil   nil)
      ( 1.88   nil   nil   nil   nil   nil   nil   nil   nil   nil)
      ( 2.33  2.54   nil   nil   nil   nil   nil   nil   nil   nil)
      ( 2.26  2.97  3.22   nil   nil   nil   nil   nil   nil   nil)
      ( 1.74  2.05  1.54  2.68   nil   nil   nil   nil   nil   nil)
      ( 2.93  4.00  4.01  4.51  3.84   nil   nil   nil   nil   nil)
      ( 3.30  4.52  4.10  3.46  3.56  3.37   nil   nil   nil   nil)
      (10.73 10.89 11.28 10.01 10.54 10.99 10.44   nil   nil   nil)
      ( 8.83  9.09  9.66  8.20  9.04  9.00  8.96  3.27   nil   nil)
      ( 8.57  8.78  9.21  8.24  8.64  8.74  9.07  3.77  3.00   nil)))

(mst shrews)

