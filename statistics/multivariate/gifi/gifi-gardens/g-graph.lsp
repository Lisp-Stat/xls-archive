;;;
;;;     Gifi-Gardens, a place to grow nonlinear multivariate
;;;     analyses of the Gifi variety, by James Hilden-Minton.
;;;

(provide "g-graph")

(require "gifi-gardens")

;;
;;     Plotting methods for gifi-graph-proto
;;

(defmeth gifi-graph-proto :add-obj-points (&key (draw t))
  (let ((done (send self :obj-points))
        (begin (send self :num-points))
        (data (column-list (send self :hub :obj-scores)))
        (labels (send self :hub :obj-labels)))
    (unless done    
      (progn
        (send self :add-points data :point-labels labels :draw draw)
        (setf (slot-value 'obj-points) 
          (list begin (1- (send self :num-points))))))))

(defmeth gifi-graph-proto :add-quant-points (&optional ind &key (draw t))
  (let ((q (length (slot-value 'quant-points))) 
        (m (length (send self :hub :in-analysis))))
    (if (< q m) 
      (setf (slot-value 'quant-points) 
        (append (slot-value 'quant-points) (repeat '(nil) (- m q))))))
  (cond
    ((null ind) 
     (send self :add-quant-points 
       (which (send self :hub :active))
       :draw draw))
    ((listp ind) 
     (dolist (i ind (if draw (send self :redraw-content)))
       (send self :add-quant-points i :draw nil)))
    ((integerp ind)
     (let* ((done (elt (slot-value 'quant-points) ind))
            (begin (send self :num-points))
            (quant (send self :spoke ind :quant-scores))
            (labels (send self :spoke ind :quant-labels))
            (dim (array-dimensions quant))
            (data (column-list
                    (select quant (iseq (length labels))
                                  (iseq (cadr dim))))))
       (unless done
         (progn 
           (send self :add-points data :point-labels labels :draw nil)
           (setf (elt (slot-value 'quant-points) ind)
             (list begin (1- (send self :num-points))))
           (send self :hide-passive-points ind)
           (send self :point-symbol (send self :quant-points ind) 'x)
           (if draw (send self :redraw-content))))))))
      

(defmeth gifi-graph-proto :add-segments (mat1 mat2 &optional rows cols)
  (let* ((dim1 (array-dimensions mat1))
         (rows (if rows rows (iseq (car dim1))))
         (cols (if cols cols (iseq (cadr dim1)))))
    (dolist (i rows)
      (send self :add-lines
        (mapcar #'(lambda (j) (list (aref mat1 i j) (aref mat2 i j)))
          cols)))))

(defmeth gifi-graph-proto :add-quant-stars (&optional ind &key (draw t))
  (let ((q (length (slot-value 'quant-stars)))
        (m (length (send self :hub :in-analysis))))
    (if (< q m)
      (setf (slot-value 'quant-stars)
        (append (slot-value 'quant-stars) (repeat '(nil) (- m q))))))
  (cond
    ((null ind)
     (send self :add-quant-stars 
       (which (send self :hub :active))
       :draw draw))
    ((listp ind)
     (dolist (i ind (if draw (send self :redraw-content)))
       (send self :add-quant-stars i :draw nil)))
    ((integerp ind)
     (let* ((done (elt (slot-value 'quant-stars) ind))
            (begin (send self :num-lines))
            (quant (send self :spoke ind :gy))
            ;(rows (which (send self :spoke ind :active-values)))
            (obj (send self :hub :obj-scores)))
       (unless done
         (progn
           (send self :add-segments quant obj) ; rows) 
           (setf (elt (slot-value 'quant-stars) ind)
             (list begin (1- (send self :num-lines))))
           (send self :hide-passive-lines ind)
           (if draw (send self :redraw-content))))))))

(defmeth gifi-graph-proto :add-loadings-arrows 
  (&optional ind &key (draw t) (scale 2.5))
  (let ((q (length (slot-value 'loadings-arrows)))
        (m (length (send self :hub :in-analysis))))
    (if (< q m)
      (setf (slot-value 'loadings-arrows)
        (append (slot-value 'loadings-arrows) 
                (repeat '(nil) (- m q))))))
  (cond 
    ((null ind)
     (send self :add-loadings-arrows 
       (which (send self :hub :active)) :draw draw :scale scale))
    ((listp ind)
     (dolist (i ind (if draw (send self :redraw-content)))
       (send self :add-loadings-arrows i :draw nil :scale scale)))
    ((and (integerp ind) (send self :spoke ind :loadings))
     (let* ((done (elt (slot-value 'loadings-arrows) ind))
            (begin (send self :num-lines))
            (point (send self :num-points))
            (scale (send self :loadings-scale scale))
            (load (* scale (send self :spoke ind :loadings)))
            (data (transpose (list (* 0 load) load))))
       (unless done
         (progn
           (send self :add-lines data :draw nil)
           (setf (elt (slot-value 'loadings-arrows) ind)
             (list begin 
                   (1- (send self :num-lines))
                   point))
           (send self :add-points (mapcar #'list load) :draw nil)
           (send self :linestart-width begin 2)
           (send self :point-symbol point 'diamond)
           (send self :point-label point 
             (send self :spoke ind :var-label))
           (send self :hide-passive ind)
           (if draw (send self :redraw-content))))))))

(defmeth gifi-graph-proto :add-quant-curves (&optional ind &key (draw t))
  (let ((q (length (slot-value 'quant-curves)))
        (m (length (send self :hub :in-analysis))))
    (if (< q m)
      (setf (slot-value 'quant-curves)
        (append (slot-value 'quant-curves)
                (repeat '(nil) (- m q)))))
    (cond
      ((null ind)
       (dotimes (i m (if draw (send self :redraw-content)))
         (send self :add-quant-curves i :draw nil)))
      ((listp ind) 
       (dolist (i ind (if draw (send self :redraw-content)))
         (send self :add-quant-curves i :draw nil)))
      ((send self :spoke ind :has-method :quant-curve-data)
       (let* ((done (elt (slot-value 'quant-curves) ind))
              (begin (send self :num-lines))
              (data (column-list 
                      (send self :spoke ind :quant-curve-data))))
         (send self :add-lines data :draw draw)
         (setf (elt (slot-value 'quant-curves) ind)
           (list begin (1- (send self :num-lines)))))))))


(defmeth gifi-graph-proto :hide-passive (&optional ind)
  (send self :hide-passive-points ind)
  (send self :hide-passive-lines ind))

(defmeth gifi-graph-proto :hide-passive-points (&optional ind)
  (cond
    ((null ind)
     (dotimes (i (max (length (slot-value 'loadings-arrows))
                      (length (slot-value 'quant-points))))
       (send self :hide-passive-points i)))
    ((listp ind)
     (dolist (i ind) (send self :hide-passive-points i)))
    ((integerp ind)
     (let* ((load (if (< ind (length (slot-value 'loadings-arrows)))
                    (caddr (send self :loadings-arrows ind))))
            (pass-var (null (send self :spoke ind :active)))
            (qp (if (< ind (length (slot-value 'quant-points)))
                  (send self :quant-points ind)))
            (pos (position "PASSIVE" (send self :spoke ind :quant-labels)
                   :test #'equal)))
       (if (and load pass-var)
         (send self :point-state load 'invisible))
       (if (and qp pos)
         (send self :point-state (elt qp pos) 'invisible))))))

(defmeth gifi-graph-proto :hide-passive-lines (&optional ind)
  (let* ((qs (slot-value 'quant-stars))
         (la (slot-value 'loadings-arrows))
         (qs-ind (if qs (which qs)))
         (la-ind (if la (which la)))
         (indices (union qs-ind la-ind)))
    (cond 
      ((and indices (null ind))
       (dolist (i indices) (send self :hide-passive-lines i)))
      ((listp ind) 
       (dolist (i ind) (send self :hide-passive-lines i)))
      ((member ind indices)
       (let* ((starts (send self :quant-stars ind 0))
              (head (send self :loadings-arrows ind))
              (p-ind (which 
                       (mapcar #'null  
                               (send self :spoke ind :active-values))))
              (passive (if starts (select starts p-ind))))
         (dolist (i passive)
           (send self :linestart-next i nil))
         (if (and head (null (send self :spoke ind :active)))
           (send self :linestart-next (car head) nil)))))))

(defmeth gifi-graph-proto :quant-star-type (&optional ind (type 'dashed))
  (send self :linestart-type 
    (remove nil (send self :quant-stars ind)) type))

(defmeth gifi-graph-proto :line-width (width &rest args)
  (send self :linestart-width 
    (remove nil (apply #'send self args)) width))


;;
;;     Utility methods for gifi-graph-proto
;;

(defmeth gifi-graph-proto :hub (&rest args)
  (apply #'send (slot-value 'center) args))

(defmeth gifi-graph-proto :spoke (ind &rest args)
  (cond 
    ((listp ind)
     (mapcar #'(lambda (gv) (apply #'send gv args))
       (select (send self :hub :in-analysis) ind)))
    ((integerp ind) 
     (apply #'send (elt (send self :hub :in-analysis) ind) args))
    (t nil)))

(defmeth gifi-graph-proto :close ()
  (send self :hub :remove-graph self)
  (send self :remove))

(defmeth gifi-graph-proto :clear ()
  (send self :clear-points)
  (send self :clear-lines))

(defmeth gifi-graph-proto :clear-points ()
  (call-next-method)
  (setf (slot-value 'obj-points) nil)
  (setf (slot-value 'quant-points) nil))

(defmeth gifi-graph-proto :clear-lines ()
  (call-next-method)
  (setf (slot-value 'quant-stars) nil)
  (setf (slot-value 'quant-curves) nil)
  (setf (slot-value 'centroid-stars) nil)
  (setf (slot-value 'loadings-arrows) nil))

(defmeth gifi-graph-proto :update (&key (draw t) adjust)
  (send self :update-obj-points :draw nil)
  (send self :update-obj-lines :draw nil)
  (send self :update-quant-points :draw nil)
  (send self :update-quant-lines :draw nil)
  (send self :update-loadings-arrows :draw nil)
  (send self :update-quant-curves :draw nil)
  (if draw (send self :redraw-content))
  (if adjust (send self :adjust-to-data)))

(defmeth gifi-graph-proto :re-coordinate-points (ind mat)
  (let ((n (send self :num-variables)))
    (dotimes (i (length ind))
      (dotimes (j n)
        (send self :point-coordinate j (elt ind i) (aref mat i j))))))

(defmeth gifi-graph-proto :re-coordinate-linestarts  (ind mat)
  (let ((n (send self :num-variables)))
    (dotimes (i (length ind))
      (dotimes (j n)
        (send self :linestart-coordinate j (elt ind i) (aref mat i j))))))

(defmeth gifi-graph-proto :update-obj-points (&key (draw t))
  (if (send self :obj-points)
    (send self :re-coordinate-points 
      (send self :obj-points)
      (send self :hub :obj-scores)))
  (if draw (send self :redraw-content)))

(defmeth gifi-graph-proto :update-quant-points (&key (draw t))
  (if (slot-value 'quant-points)
    (let* ((gv-ind (which (slot-value 'quant-points)))
           (rows (send self :quant-points gv-ind))
           (quant-scores (send self :spoke gv-ind :quant-scores)))
      (dotimes (k (length gv-ind) (if draw (send self :redraw-content)))
        (send self :re-coordinate-points 
          (elt rows k)
          (elt quant-scores k))))))

(defmeth gifi-graph-proto :update-obj-lines (&key (draw t))
  (if (slot-value 'quant-stars)
    (let* ((gv-ind (which (slot-value 'quant-stars)))
           (rows (send self :quant-stars gv-ind 1))
           (obj-scores (send self :hub :obj-scores)))
      (dotimes (k (length gv-ind) (if draw (send self :redraw-content)))
        (send self :re-coordinate-linestarts
          (elt rows k) obj-scores)))))

(defmeth gifi-graph-proto :update-quant-lines (&key (draw t))
  (if (slot-value 'quant-stars)
    (let* ((gv-ind (which (slot-value 'quant-stars)))
           (rows (send self :quant-stars gv-ind 0))
           (quant-scores (send self :spoke gv-ind :gy)))
      (dotimes (k (length gv-ind) (if draw (send self :redraw-content)))
        (send self :re-coordinate-linestarts
          (elt rows k) 
          (elt quant-scores k))))))

(defmeth gifi-graph-proto :update-loadings-arrows (&key scale (draw t))
  (if (slot-value 'loadings-arrows)
    (let* ((gv-ind (which (slot-value 'loadings-arrows)))
           (la (send self :loadings-arrows gv-ind))
           (lines (mapcar #'cadr la))
           (heads (mapcar #'caddr la))
           (scale (send self :loadings-scale scale))
           (data (apply #'bind-rows
                   (* scale (send self :spoke gv-ind :loadings)))))
      (send self :re-coordinate-linestarts lines data)
      (send self :re-coordinate-points heads data)
      (if draw (send self :redraw)))))

(defmeth gifi-graph-proto :update-quant-curves (&key (draw t))
  (if (slot-value 'quant-curves)
    (let* ((gv-ind (which (slot-value 'quant-curves)))
           (rows (send self :quant-curves gv-ind))
           (curves (send self :spoke gv-ind :quant-curve-data))) 
      (dotimes (k (length gv-ind) (if draw (send self :redraw-content)))
        (send self :re-coordinate-linestarts
          (elt rows k)
          (elt curves k))))))

;;
;;     Accessor methods for gifi-graph-proto
;;

(defmeth gifi-graph-proto :gifi-center (&optional (center nil set))
  (if set (setf (slot-value 'center) center)
    (slot-value 'center)))

(defun iseq-alt (x)
  (cond 
    ((null x) nil)
    ((null (car x)) (cdr x))
    (t (apply #'iseq x))))

(defmeth gifi-graph-proto :obj-points ()
  (iseq-alt (slot-value 'obj-points)))

(defmeth gifi-graph-proto :quant-points (&optional ind)
  (cond
    ((null ind) (mapcar #'iseq-alt (slot-value 'quant-points)))
    ((listp ind) (mapcar #'iseq-alt (select (slot-value 'quant-points) ind)))
    ((integerp ind) (iseq-alt (elt (slot-value 'quant-points) ind)))))

(defmeth gifi-graph-proto :quant-stars (&optional ind par)
  (if (slot-value 'quant-stars)
    (cond 
      ((null ind) 
       (mapcar #'(lambda (i) (send self :quant-stars i par)) 
               (iseq (length (slot-value 'quant-stars)))))
      ((listp ind)
       (mapcar #'(lambda (i) (send self :quant-stars i par)) ind))
      ((integerp ind)
       (let ((pre (iseq-alt (elt (slot-value 'quant-stars) ind))))
         (if (null par) pre
           (if pre 
             (select pre (+ par (* 2 (iseq (/ (length pre) 2))))))))))))

(defmeth gifi-graph-proto :loadings-arrows (&optional ind)
  (if ind (select (slot-value 'loadings-arrows) ind)
    (slot-value 'loadings-arrows))) 

(defmeth gifi-graph-proto :loadings-scale (&optional scale)
  (if (numberp scale) (setf (slot-value 'loadings-scale) scale)
    (slot-value 'loadings-scale)))

(defmeth gifi-graph-proto :quant-curves (&optional ind)
  (let ((qc (slot-value 'quant-curves)))
   (if qc
    (cond 
      ((null ind) (mapcar #'iseq-alt qc))
      ((listp ind) (mapcar #'iseq-alt (select qc ind)))
      ((integerp ind) (iseq-alt (elt qc ind)))))))

;;
;;     Graphical methods for gifi-center-proto
;;

(defmeth gifi-center-proto :add-graph (gifi-graph)
  (setf (slot-value 'graphs)
    (append (slot-value 'graphs) (list gifi-graph))))

(defmeth gifi-center-proto :remove-graph (gifi-graph)
  (setf (slot-value 'graphs) 
    (remove gifi-graph (slot-value 'graphs))))

(defmeth gifi-center-proto :plot-obj-scores ()
  (let ((gg (send gifi-graph-proto :new self 
              :title (send self :title))))
    (send gg :add-obj-points)
    (send gg :adjust-to-data)
    gg)) 

(defmeth gifi-center-proto :update-graphs (&rest args)
  (mapcar #'(lambda (gg) (apply #'send gg :update args))
    (send self :graphs)))

(defmeth gifi-center-proto :iterate-graphs (&optional (times 1) &key adjust)
  (dotimes (i times 
             (select (send self :loss-history) (iseq times)))
    (send self :iterate-once)
    (send self :update-graphs :adjust adjust)))

;;
;;     Graphical methods for gifi-var-proto
;;     


