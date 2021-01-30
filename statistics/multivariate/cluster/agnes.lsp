;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Agglomerative nesting (also known as hierachical cluster analysis),
;;  following Kaufman and Rousseeuw (Finding Groups in Data, Chapter 5).
;;  
;;  Version 0.1 -- July 7 1995 -- Only group average linkage, and no
;;              graphics.
;;  Version 0.2 -- July 8 1995 -- Also complete and single linkage. Can
;;              print fitted ultrametric.
;;  Version 0.3 -- July 8 1995 -- Also has Ward, Gower, centroid and
;;              weighted average method.
;;
;;  Jan de Leeuw
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun agnes (diss &key (type "average") (ultrametric nil))
  (let* ((n (array-dimension diss 0))
         (k (mapcar #'list (iseq n)))
         (d (copy-array diss))
         (l (list 0))
         (h (list k)))
    (loop
     (if (= 1 (array-dimension d 1)) 
         (return (values h l))
         (progn 
          (multiple-value-setq (i j m)
                               (minimum-distance d))
          (setf d (merge-distances d k i j type))
          (setf k (merge-clusters k i j))
          (setf l (append l (list m)))
          (setf h (append h (list k)))
          )
         )
     )
    )
  )


(defun minimum-distance (x)
"Args: x
Finds the smallest element of a distance matrix,
and the corresponding indices."
  (let* ((n (array-dimension x 0))
         (r (rest (iseq n)))
         (m positive-infinity)
         im jm)
    (dolist (i r)
            (dotimes (j i)
                     (let ((a (aref x i j)))
                       (if (< a m)
                           (setf im i jm j m a)))))
    (values im jm m)
    )
  )

(defun merge-clusters (l i j)
  (let* ((n (length l))
         (v (delete i (delete j (iseq n)))))
  (append (list (append (elt l i) (elt l j)))
          (select l v))
  )
)
  
(defun merge-distances (d l i j type)
  (if (= 2 (array-dimension d 0)) #2A((0))
      (let* ((n (array-dimension d 0))
             (e (make-array (list (1- n) (1- n))
                            :initial-element 0))
             (k (1+ (iseq (- n 2))))
             (nn (iseq n))
             (v (delete i (delete j (iseq n))))
             (na (length (elt l i)))
             (nb (length (elt l j)))
             (da (select (select d i nn) 0 v))
             (db (select (select d j nn) 0 v))
             (qa (make-array (list 1 (- n 2))
                             :initial-contents 
                             (mapcar #'length (select l v))))
             (dc (aref d i j))
             (av (merge-methods da db dc na nb qa type)))
        (setf (select e k k) (select d v v))
        (setf (select e 0 k) av)
        (setf (select e k 0) (transpose av))
        e
        )
    )
  )

(defun merge-methods (x y z nx ny nq type)
  (let ((nt (+ nx ny)))
    (cond
      ((string-equal type "centroid")
       (sqrt (- (+ (* (/ nx nt) x x)
                   (* (/ ny nt) y y))
                (/ (* nx ny z z) (* nt nt)))))
      ((string-equal type "ward")
       (sqrt (- (+ (* (/ (+ nx nq) (+ nt nq)) x x)
                   (* (/ (+ ny nq) (+ nt nq)) y y))
                (/ (* (/ nq (+ nt nq)) z z)))))
      ((string-equal type "gower")
       (sqrt (- (+ (/ (* x x) 2)
                   (/ (* y y) 2))
                (/ (* z z) 4))))
      ((string-equal type "weighted")
       (/ (+ x y) 2))                     
      ((string-equal type "single")
       (pmin x y))
      ((string-equal type "complete")
       (pmax x y))
      (t
       (+ (* (/ nx nt) x)
          (* (/ ny nt) y)))
      )
    )
  ) 

(defun ultrametric (h v)
  (let* ((n (max (first h)))
         (l (length h))
         (d (make-array (list n n) :initial-element 0))
         (r (rest (iseq n))))
    (dolist (i r d)
            (dotimes (j i)
                     (dotimes (k l)
                              (dolist (f (elt h k))
                                      (if (and (subsetp (list i j) f) 
                                               (= 0 (aref d i j)))
                                          (setf (aref d i j) (elt v k)
                                                (aref d j i) (elt v k)))
                                      )
                              )
                     )
            )
    )
  )

(defun make-ultrametric (dist &key (type "average"))
  (print-matrix
   (multiple-value-call #'ultrametric (agnes dist :type type)))
  )
  
(def r-and-l 
     #2A((0 2 6 10 9) (2 0 5 9 8) (6 5 0 4 5) (10 9 4 0 3) (9 8 5 3 0)))
