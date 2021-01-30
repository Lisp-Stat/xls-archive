;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;                                                                ;;
;;;;  Code to show basic hypothesis test for the mean of a single   ;;
;;;;  population using normal data.  The values for the null mean,  ;;
;;;;  alternative mean (from which the data actually come), n,      ;;
;;;;  sigma, and alpha can all be changed.  The power curve can     ;;
;;;;  also be shown.                                                ;;
;;;;  Questions, comments to jbond@laplace.stat.ucla.edu            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



(defproto hypo-proto () () scatterplot-proto)
(defun hypo-test (&optional (h0 0) (ha 1) (sigma 1) (num 50) (alpha .05))
    (def hypo-plot (send hypo-proto :new 2 :title "Data Plot"))
    (send hypo-plot :add-slot 'h0 h0)
    (send hypo-plot :add-slot 'ha ha)
    (send hypo-plot :add-slot 'sigma sigma)
    (send hypo-plot :add-slot 'data)
    (send hypo-plot :add-slot 'num num)
    (send hypo-plot :add-slot 'alpha alpha)
    (send hypo-plot :add-slot 'change)
    (send hypo-plot :add-slot 'hachange t)
    (send hypo-plot :add-slot 'advanced nil)
    (send hypo-plot :slider-dialog-beg)
)

(defmeth hypo-proto :slider-dialog-beg ()
  (let* (
         (askha (send text-item-proto :new "Pick Ha: "))
         (showha (send text-item-proto :new "" :text-length 5))
         (haslider (send interval-scroll-item-proto :new
                           (list -5 5) :points 101
                   :action #'(lambda (x) (send showha :text
                               (format nil "~4,3f" x))
                               (send hypo-plot :ha (send haslider :value))
                               (send hypo-plot :draw-dens)
                               (send hypo-plot :hachange nil)
                               (send hypo-plot :change nil))))
         (advan-butt (send button-item-proto :new "Advanced Options"
                :action #'(lambda () 
                          (def power-plot (send hypo-proto :new 2 
                                            :title "Power Function"
                                            :location (list 600 50)))
                                    (send hypo-plot :advanced t)
                                     (send hypo-plot :slider-dialog))))
        )
       (send hypo-plot :h0 0)
       (send hypo-plot :sigma 1)
       (send hypo-plot :num 25)
       (send hypo-plot :alpha .05)
       (send haslider :value 1)
       (send dialog-proto :new (list
                               (list askha haslider showha)
                               (list advan-butt))
                          :title "Parameter Dialog" :location (list 50 530))
   )
)





(defmeth hypo-proto :slider-dialog ()
  (let* (
         (askh0 (send text-item-proto :new "Pick H0: "))
         (showh0 (send text-item-proto :new "" :text-length 5))
         (h0slider (send interval-scroll-item-proto :new 
                           (list -5 5) :points 101  
                   :action #'(lambda (x) 
                               (send hypo-plot :h0 x)
                               (send hypo-plot :change t)
                               (send hypo-plot :hachange t)
                               (send showh0 :text 
                               (format nil "~4,3f" x)))))

         (asksigma (send text-item-proto :new "Pick Sigma: "))
         (showsig (send text-item-proto :new "" :text-length 5))
         (sigmaslider (send interval-scroll-item-proto :new
                           (list 0 10) :points 101 
                   :action #'(lambda (x) 
                               (send hypo-plot :sigma x)
                               (send hypo-plot :change t)
                               (send hypo-plot :hachange t)
                               (send showsig :text
                               (format nil "~4,3f" x)))))


         (asknum (send text-item-proto :new "How Many Points: "))
         (shownum (send text-item-proto :new "" :text-length 5))
         (numslider (send sequence-scroll-item-proto :new
                           (iseq 1 100) 
                   :action #'(lambda (x) 
                               (send hypo-plot :num x)
                               (send hypo-plot :change t)
                               (send hypo-plot :hachange t)
                               (send shownum :text
                               (format nil "~a" x)))))

         (askalp (send text-item-proto :new "Alpha Level: "))
         (showalp (send text-item-proto :new "" :text-length 5))
         (alpslider (send interval-scroll-item-proto :new
                            (list 0 .5) :points 51 
                   :action #'(lambda (x) 
                               (send hypo-plot :alpha x)
                               (send hypo-plot :change t)
                               (send hypo-plot :hachange t)
                               (send showalp :text
                               (format nil "~3,3f" x)))))


         (do-it (send button-item-proto :new "Get New Sample"
                :action #'(lambda () 
                           (send hypo-plot :h0 (send h0slider :value))
                           (send hypo-plot :sigma (send sigmaslider :value))
                           (send hypo-plot :num (send numslider :value))
                           (send hypo-plot :alpha (send alpslider :value))
                           (send hypo-plot :hachange t)
                           (send hypo-plot :draw-dens)
                           (send hypo-plot :change nil)))) 

        )
       (send h0slider :value 0)
       (send sigmaslider :value 1)
       (send numslider :value 25)
       (send alpslider :value .05)

       (send dialog-proto :new (list 
                               (list askh0 h0slider showh0)
                               (list asksigma sigmaslider showsig)
                               (list asknum numslider shownum) 
                               (list askalp alpslider showalp) (list do-it))
                          :title "Parameter Dialog" :location (list 600 530))
  )
)


(defmeth hypo-proto :draw-dens ()
 (let* (
        (num (send self :num))
        (sqrtn (^ num .5))
        (sigma (send self :sigma))
        (ha (send self :ha))
        (h0 (send self :h0))
        (data (if (send self :hachange) 
              (send self :data (+ ha (* sigma (normal-rand num))))
              (+ ha (- (send self :data) (mean (send self :data))))))
        (y (repeat 0 (length data)))
        (xbar (mean data))
        (sd (standard-deviation data))
        (h0seq (rseq (- h0 sd) (+ h0 sd) 100))
        (h0normal-seq (/ (normal-dens (/ (- h0seq h0) (/ sigma sqrtn))) 
                         (/ sigma sqrtn)))
        (alpha (send self :alpha))
        (adv (send self :advanced))
        (xrej (if (> xbar h0) (find-reject alpha sigma sqrtn h0 t)
                              (find-reject alpha sigma sqrtn h0 nil)))
        (p-val (if (> xbar h0) 
                   (- 1 (normal-cdf (/ (- xbar h0) (/ sigma sqrtn))))
                   (normal-cdf (/ (- xbar h0) (/ sigma sqrtn)))))
        (power (if adv (if (> ha h0) 
                   (- 1 (normal-cdf (/ (+ h0 (- ha) 
                                          (* (/ sigma sqrtn)
                                             (normal-quant (- 1 alpha))))
                                       (/ sigma sqrtn))))
                   (normal-cdf (/ (- h0 ha 
                                          (* (/ sigma sqrtn)
                                             (normal-quant (- 1 alpha))))
                                       (/ sigma sqrtn))))))
        (bottom (- xbar (* (normal-quant alpha) (/ sigma sqrtn))))
        (top (+ xbar (* (normal-quant alpha) (/ sigma sqrtn))))
        (densht (/ (normal-dens 0) (/ sigma sqrtn)))
       )
     (if adv (if (send self :change) (send power-plot :clear-points)))
     (if adv (send power-plot :add-points (list ha) (list power)))
     (if adv (send power-plot :adjust-to-data))
     (send self :clear-points)
     (send self :clear-lines)
     (send self :add-points data y)

     (send self :add-lines h0seq h0normal-seq)
     (send self :adjust-to-data)
     (send self :add-lines (list xrej xrej)
                            (list 0 (/ (normal-dens (/ (- xrej h0)
                                                       (/ sigma sqrtn)))
                                       (/ sigma sqrtn))) :type 'dashed)

     (send self :add-lines (list xbar xbar) 
                            (list 0 (* .75 densht)))

     (send self :add-lines (list bottom top) 
                           (list (* .25 densht) (* .25 densht)))

     (send self :add-lines (list top top) 
                           (list (* .23 densht) (* .27 densht)))

     (send self :add-lines (list bottom bottom)
                           (list (* .23 densht) (* .27 densht)))

     (apply #'send self :draw-string "H0"
                       (send self :real-to-canvas h0 densht))

     (apply #'send self :draw-string "Xbar"
                       (send self :real-to-canvas xbar
                           (* .75 densht)))

     (send self :draw-string (format nil "Your p-value is: ~6,4g" p-val) 75 12)
     (if adv (send power-plot :draw-string (format nil 
                          "The power at ~5,3f is ~5,3f" ha power) 75 15))

     (send self :draw-string (format nil "The Sample Mean is: ~6,4g" xbar)
                                            75 25)
     (send self :draw-string (format nil "The Critical Value is: ~6,4g" xrej)
                                            75 39)
 )
)



(defmeth hypo-proto :h0 (&optional (val nil set))
(if set (setf (slot-value 'h0) val)
        (slot-value 'h0)))

(defmeth hypo-proto :ha (&optional (val nil set))
(if set (setf (slot-value 'ha) val)
        (slot-value 'ha)))

(defmeth hypo-proto :data (&optional (val nil set))
(if set (setf (slot-value 'data) val)
        (slot-value 'data)))

(defmeth hypo-proto :sigma (&optional (val nil set))
(if set (setf (slot-value 'sigma) val)
        (slot-value 'sigma)))

(defmeth hypo-proto :num (&optional (val nil set))
(if set (setf (slot-value 'num) val)
        (slot-value 'num)))

(defmeth hypo-proto :alpha (&optional (val nil set))
(if set (setf (slot-value 'alpha) val)
        (slot-value 'alpha)))

(defmeth hypo-proto :change (&optional (val nil set))
(if set (setf (slot-value 'change) val)
        (slot-value 'change)))

(defmeth hypo-proto :hachange (&optional (val nil set))
(if set (setf (slot-value 'hachange) val)
        (slot-value 'hachange)))

(defmeth hypo-proto :advanced (&optional (val nil set))
(if set (setf (slot-value 'advanced) val)
        (slot-value 'advanced)))


(defun find-reject (alpha sigma sqrtn h0 test)
 (if test (+ h0 (* (/ sigma sqrtn) (abs (normal-quant (- 1 alpha)))))
          (- h0 (* (/ sigma sqrtn) (abs (normal-quant (- 1 alpha))))))
)


(hypo-test)




