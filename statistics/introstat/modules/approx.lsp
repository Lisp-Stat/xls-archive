;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Code to Show the Normal and Poisson Approximations to the
;;  Binomial Distribution.  Questions, Comments to:
;;  
;;  Jason Bond (jbond@stat.ucla.edu)
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defproto approx-proto '(poisson-graph normal-graph slider n p start finish
                         color xseq))
(defproto poisson-graph-proto '(parent) nil scatterplot-proto)
(defproto normal-graph-proto '(parent normal) nil scatterplot-proto)

(defproto normal-overlay-proto nil nil graph-overlay-proto)
(defproto poisson-overlay-proto nil nil graph-overlay-proto)

(defmeth normal-overlay-proto :redraw ()
 (let* (
        (graph (send self :graph))
        (parent (send graph :parent))
       )
   (send graph :frame-rect 10 5 10 10)
   (send graph :draw-string "+/- Normal Density" 25 15)
   (send graph :draw-line 10 25 30 25)
   (send graph :draw-string "Binomial" 38 30)
   (if (send parent :color) (send graph :draw-color 'yellow))
   (send graph :draw-string "--- Normal Approximation" 10 45)
   (if (send parent :color) (send graph :draw-color 'white))
   (send self :bitmap (send graph :normal))))

(defmeth normal-overlay-proto :do-click (x y m1 m2)
 (let ((graph (send self :graph)))
   (when (and graph (and (< 10 x 20) (< 5 y 15)))
         (send graph :normal (not (send graph :normal)))
         (draw-normal-approx graph)
         (send self :bitmap (send graph :normal)))))

(defmeth poisson-overlay-proto :redraw ()
 (let* (
        (graph (send self :graph))
        (parent (send graph :parent))
       )
   (send graph :draw-line 10 10 30 10)
   (send graph :draw-string "Binomial" 38 15)
   (if (send parent :color) (send graph :draw-color 'yellow))
   (send graph :draw-string "--- Poisson" 10 30)
   (if (send parent :color) (send graph :draw-color 'white))))


(defmeth approx-proto :add-poisson (value)
 (if (not value) (progn (send (slot-value 'poisson-graph) :close)
                        (setf (slot-value 'poisson-graph) nil)
                        (send self :update-graphs))
                 (let ((graph (send poisson-graph-proto :new 2 
                                :title "Poisson Approximation to the Binomial"
                                :location
#-unix (list 50 25)
#+unix (list 50 50)
                                :go-away nil)))
                    (send graph :parent self)
                    (if (send self :color) (send graph :back-color 'black))
                    (send graph :add-overlay (send poisson-overlay-proto :new))
                    (send graph :margin 0 50 0 0)
                    (send self :poisson-graph graph)
                    (draw-poisson-approx graph)
                    (send self :update-graphs))))

(defmeth approx-proto :add-normal (value)
 (if (not value) (progn (send (slot-value 'normal-graph) :close)
                        (setf (slot-value 'normal-graph) nil)
                        (send self :update-graphs))
                 (let ((graph (send normal-graph-proto :new 2
                                :title "Normal Approximation to the Binomial"
                                :location
#-unix (list 400 25)
#+unix (list 600 50)
                                :go-away nil)))
                   (send graph :parent self)
                   (if (send self :color) (send graph :back-color 'black))
                   (send graph :add-overlay (send normal-overlay-proto :new))
                   (send graph :margin 0 50 0 0)
                   (send self :normal-graph graph)
                   (draw-normal-approx graph)
                   (send self :update-graphs))))



(defmeth normal-overlay-proto :bitmap (selected)
 (let* ((check-bitmap '#2a((1 1 1 1 1 1 1 1 1 1)
                           (1 1 0 0 0 0 0 0 1 1)
                           (1 0 1 0 0 0 0 1 0 1)
                           (1 0 0 1 0 0 1 0 0 1)
                           (1 0 0 0 1 1 0 0 0 1)
                           (1 0 0 0 1 1 0 0 0 1)
                           (1 0 0 1 0 0 1 0 0 1)
                           (1 0 1 0 0 0 0 1 0 1)
                           (1 1 0 0 0 0 0 0 1 1)
                           (1 1 1 1 1 1 1 1 1 1)))
        (blank-bitmap '#2a((1 1 1 1 1 1 1 1 1 1)
                           (1 0 0 0 0 0 0 0 0 1)
                           (1 0 0 0 0 0 0 0 0 1)
                           (1 0 0 0 0 0 0 0 0 1)
                           (1 0 0 0 0 0 0 0 0 1)
                           (1 0 0 0 0 0 0 0 0 1)
                           (1 0 0 0 0 0 0 0 0 1)
                           (1 0 0 0 0 0 0 0 0 1)
                           (1 0 0 0 0 0 0 0 0 1)
                           (1 1 1 1 1 1 1 1 1 1)))
        (map (if selected check-bitmap blank-bitmap))
        (graph (send self :graph)))
     (if (send (send graph :parent) :color) 
         (send graph :draw-color 'cyan))
     (send graph :draw-bitmap map 10 5)
     (if (send (send graph :parent) :color) 
         (send graph :draw-color 'white))))


(defmeth approx-proto :update-graphs ()
 (let* ( 
        (poisson-graph (slot-value 'poisson-graph))
        (normal-graph (slot-value 'normal-graph))
        (n (send self :n))
        (p (send self :p))
        (binomial (if (or poisson-graph normal-graph) (binomial-rand 500 n p)))
       )
   (if poisson-graph (draw-poisson-approx poisson-graph))
   (if normal-graph (draw-normal-approx normal-graph))))
     


(defun binomial-approx ()
 (let* (
        (approx (send approx-proto :new))
        (ask-type (send text-item-proto :new "Show:"))
        (poisson-toggle (send toggle-item-proto :new "Poisson Approximation"
          :action #'(lambda () (send approx :add-poisson (send self :value)))))
        (normal-toggle (send toggle-item-proto :new "Normal Approximation"
          :action #'(lambda () (send approx :add-normal (send self :value)))))
        (ask-params (send text-item-proto :new "Enter Parameters:"))
        (n-param-ask (send text-item-proto :new "N:"))
        (n-show (send text-item-proto :new "" :text-length 4))
        (n-scroll (send sequence-scroll-item-proto :new (iseq 1 100)
                          :action #'(lambda (x) 
                             (send n-show :text (format nil "~a" x))
                             (send approx :n x)
                             (send approx :update-graphs))))
        
        (p-param-ask (send text-item-proto :new "P:"))
        (p-show (send text-item-proto :new "" :text-length 4))

        (p-scroll (send sequence-scroll-item-proto :new (rseq 0 1 101) 
                          :action #'(lambda (x) 
                             (send p-show :text (format nil "~4,2f" x))
                             (send approx :p x)
                             (send approx :update-graphs))))
       )
   (if (screen-has-color) (send approx :color t)
                          (send approx :color nil))
   (send n-scroll :value 9)
   (send p-scroll :value 50)
   (send n-show :text "10")
   (send p-show :text ".50")
   (send approx :n 10)
   (send approx :p .5)
  (defmeth dialog-proto :close ()
      (if (send approx :slot-value 'poisson-graph)
          (send (send approx :slot-value 'poisson-graph) :close))
      (if (send approx :slot-value 'normal-graph)
          (send (send approx :slot-value 'normal-graph) :close))
      (call-next-method))
   (send dialog-proto :new (list ask-type poisson-toggle normal-toggle
                                 ask-params 
                                 (list n-param-ask n-scroll n-show)
                                 (list p-param-ask p-scroll p-show))
                      :location
#-unix (list 250 300)
#+unix (list 400 550)
)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;   Normal Approx Control
;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 
(defun draw-normal-approx (graph)
 (let* (
        (parent (send graph :parent))
        (p-graph (send parent :poisson-graph))
        (n (send parent :n))
        (p (send parent :p))
        (mean (* n p))
        (sd (sqrt (* n p (- 1 p))))
        (start (if p-graph (send parent :start)
                           (max 0 (floor (- mean (* 4 sd))))))
        (finish (if p-graph (send parent :finish)
                            (min n (floor (+ mean (* 4 sd))))))
        (normal-x-seq (if (send graph :normal) (rseq start finish 100)))
        (normal-y-seq (if (send graph :normal) 
                          (/ (normal-dens (/ (- normal-x-seq mean .5) sd)) sd)))
        (seq (iseq start finish))
        (xseq (if p-graph (send parent :xseq)
                          (combine (repeat (first seq) 2) 
                                    (repeat (rest (butlast seq)) 
                                    (repeat 3 (1- (- finish start))))
                                    (repeat (elt (last seq) 0) 2))))
        (len (length xseq))
        (yseq (combine (mapcar #'(lambda (x) 
                                  (list 0 (repeat (binomial-pmf x n p) 2))) 
                           (butlast seq)) 0))
        (n-y-seq (combine (mapcar #'(lambda (x y) 
                           (list 0 
                             (repeat (- (normal-cdf (/ (- x mean .5) sd)) 
                                        (normal-cdf (/ (- y mean .5) sd))) 2)))
                              (rest seq) (butlast seq)) 0))
        )
    (send graph :clear-lines)
    (send graph :add-lines (list xseq n-y-seq) :type 'dashed)
    (send graph :add-lines (list xseq yseq))
    (when (send graph :normal) 
                     (send graph :add-lines
                                 (list normal-x-seq normal-y-seq)))
    (when (send parent :color) (send graph :use-color t)

                               (send graph :linestart-color 
                                 (iseq len) (repeat 'yellow len))
                               (if (send graph :normal)
                                   (send graph :linestart-color 
                                   (iseq (* 2 len) (+ (* 2 len) 99)) 
                                   (repeat 'cyan 100))))
    (send graph :adjust-to-data)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Poisson Approx Control
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun draw-poisson-approx (graph)
 (let* (
        (parent (send graph :parent))
        (n (send parent :n))
        (p (send parent :p))
        (mean (* n p))
        (sd (sqrt (* n p (- 1 p))))
        (b-start (max 0 (floor (- mean (* 4 sd)))))
        (b-finish (min n (floor (+ mean (* 4 sd)))))
        (p-start (poisson-quant .01 mean))
        (p-finish (poisson-quant .99 mean))
        (start (min b-start p-start))
        (finish (max b-finish p-finish))
        (seq (iseq start finish))
        (xseq (combine (repeat (first seq) 2) 
                       (repeat (rest (butlast seq)) 
                       (repeat 3 (1- (- finish start))))
                       (repeat (elt (last seq) 0) 2)))
        (len (length xseq))
        (b-yseq (combine (mapcar #'(lambda (x) 
                                  (list 0 (repeat (binomial-pmf x n p) 2))) 
                                 (butlast seq)) 0))
        (p-yseq (combine (mapcar #'(lambda (x)
                                  (list 0 (repeat (poisson-pmf x mean) 2))) 
                                        (butlast seq)) 0))
      )
    (send parent :xseq xseq)
    (send parent :start start)
    (send parent :finish finish)
    (send graph :clear)
    (send graph :add-lines (list xseq p-yseq) :type 'dashed)
    (send graph :add-lines (list xseq b-yseq))
    (when (send parent :color)
          (send graph :use-color t)
          (send graph :linestart-color (iseq len) (repeat 'yellow len)))
    (send graph :adjust-to-data)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro normal-assessor (key slot prototype)
`(defmeth ,prototype ,key (&optional (content nil set))
   (when set (setf (slot-value ',slot) content))
   (slot-value ',slot)))

(normal-assessor :n n approx-proto)
(normal-assessor :p p approx-proto)
(normal-assessor :poisson-graph poisson-graph approx-proto)
(normal-assessor :normal-graph normal-graph approx-proto)
(normal-assessor :start start approx-proto)
(normal-assessor :finish finish approx-proto)
(normal-assessor :color color approx-proto)
(normal-assessor :xseq xseq approx-proto)

(normal-assessor :normal normal scatterplot-proto)
(normal-assessor :parent parent scatterplot-proto)

;(make-color 'neween 22 8b 22)


(binomial-approx)


