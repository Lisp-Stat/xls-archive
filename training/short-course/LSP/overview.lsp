(new-section "Overview" "overview.scr")

(load "abrasion")
(load "precip")
(load-data "diabetes")

(define-section-item "Linked"
  (let ((h (histogram hardness :title "Hardness"))
        (p (plot-points tensile-strength abrasion-loss 
                        :variable-labels 
                        (list "Tensile Strength" "Abrasion Loss"))))
    (send h :linked t)
    (send p :linked t)))

(define-section-item "Spin"
  (spin-plot (list hardness tensile-strength abrasion-loss)
             :variable-labels (list "H" "TS" "AL")))

(define-section-item "Survival"
  (let ((times '(9 13 13 18 23 28 31 34 45 48 161
                 5 5 8 8 12 16 23 27 30 33 43 45))
        (status '(1 1 0 1 1 0 1 1 0 1 0 1 1 1 1 1 0 1 1 1 1 1 1)))
    (flet ((km (times status)
             (let* ((dt-list (select times (which (= 1 status))))
                    (udt (sort-data (remove-duplicates dt-list)))
                    (d (mapcar #'(lambda (x) (count x dt-list :test #'=))
                               udt))
                    (r (mapcar #'(lambda (x) (count x times :test #'<=))
                               udt)))
               (list udt (accumulate #'* (/ (- r d) r)))))
           (make-steps (xy)
             (let* ((x (first xy))
                    (y (second xy))
                    (n (length x)))
               (list (cons 0 (repeat x (repeat 2 n)))
                     (butlast (repeat (cons 1 y) (repeat 2 (+ 1 n))))))))
      (plot-lines (make-steps (km times status))
                  :title "Survival Fuction"))))


(define-section-item "Power"
  (let ((sx (sort-data abrasion-loss))
        (ns (normal-quant (/ (iseq 1 30) 31))))
    (flet ((bc (x p)
             (let* ((bcx (if (< (abs p) .0001)
                             (log x)
                             (/ (^ x p) p)))
                    (max (max bcx))
                    (min (min bcx)))
               (/ (- bcx min) (- max min)))))
      (let* ((pl (plot-points ns (bc sx 1)))
             (s (interval-slider-dialog
                 '(-1 2) :action
                 #'(lambda (p)
                     (send pl :clear-points :draw nil)
                     (send pl :add-points ns (bc sx p))))))
        (send pl :add-subordinate s)
        (send s :value 1)))))

(define-section-item "Density"
  (let* ((w (plot-lines (kernel-dens precipitation :width 1))))
    (send w :add-slot 'kernel-width 1)

    (defmeth w :kernel-width (&optional width)
      (when width
            (setf (slot-value 'kernel-width) width)
            (send self :set-lines))
      (slot-value 'kernel-width))

    (send w :add-slot 'kernel-data precipitation)

    (defmeth w :kernel-data () (slot-value 'kernel-data))

    (defmeth w :set-lines ()
      (let ((width (send self :kernel-width))
            (data (send self :kernel-data)))
        (send self :clear-lines :draw nil)
        (send self :add-lines
              (kernel-dens data :width width))))

    (let ((sl (interval-slider-dialog
               '(.25 1.5)
               :action 
               #'(lambda (s) (send w :kernel-width s)))))
      (send sl :value 1)
      (send w :add-subordinate sl))

    (defproto run-item-proto '(graph) () menu-item-proto)

    (send run-item-proto :key #\R)

    (defmeth run-item-proto :isnew (graph)
      (call-next-method "Run")
      (setf (slot-value 'graph) graph))

    (defmeth run-item-proto :update ()
      (send self :mark
            (send (slot-value 'graph) :idle-on)))

    (defmeth run-item-proto :do-action ()
      (let ((graph (slot-value 'graph)))
        (send graph :idle-on
              (not (send graph :idle-on)))))

    (send (send w :menu) :append-items (send run-item-proto :new w))

    (send w :add-slot 'xvals
          (rseq (min precipitation) (max precipitation) 30))

    (defmeth w :xvals () (slot-value 'xvals))

    (defmeth w :set-lines ()
      (let ((width (send self :kernel-width))
            (xvals (send self :xvals)))
        (send self :clear-lines :draw nil)
        (send self :add-lines
              (kernel-dens (send self :kernel-data)
                           :width width
                           :xvals xvals))))

    (send w :slot-value 'kernel-data (copy-list precipitation))

    (defmeth w :do-idle ()
      (let ((d (slot-value 'kernel-data))
            (i (random 30))
            (j (random 30)))
        (setf (select d i) (select precipitation j))
        (send self :set-lines)))))

(define-section-item "Regression" (load-example "regdemo"))

(define-section-item "Grand Tour"
  (labels ((sphere-rand (n)
             (loop (let* ((x (- (* 2 (uniform-rand n)) 1))
                          (nx2 (sum (^ x 2))))
                     (if (< nx2 1) (return (/ x (sqrt nx2)))))))

           (tour-plot (&rest args)
             (let ((p (apply #'spin-plot args)))
               (send p :add-slot 'tour-count -1)
               (send p :add-slot 'tour-trans nil)
               (defmeth p :do-idle () (send self :tour-step))
               (defmeth p :tour-step ()
                 (when (< (slot-value 'tour-count) 0)
                       (let ((vars (send self :num-variables))
                             (angle (abs (send self :angle))))
                         (setf (slot-value 'tour-count) 
                               (random (floor (/ pi (* 2 angle)))))
                         (setf (slot-value 'tour-trans) 
                               (make-rotation (sphere-rand vars) 
                                              (sphere-rand vars)
                                              angle))))
                 (send self :apply-transformation (slot-value 'tour-trans))
                 (setf (slot-value 'tour-count)
                       (- (slot-value 'tour-count) 1)))
               (defmeth p :tour-on (&rest args)
                 (apply #'send self :idle-on args))
               (let ((item (send graph-item-proto :new "Touring" p
                                 :tour-on :tour-on :toggle t)))
                 (send item :key #\T)
                 (send (send p :menu) :append-items item))
               p)))
    (tour-plot diabetes :variable-labels dlabs :title "Grand Tour")))
