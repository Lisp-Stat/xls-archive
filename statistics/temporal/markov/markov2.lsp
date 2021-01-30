;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Code to show the stationary distribution of a Markov Chain.  Function
;;  calls should be of the form:
;;  (markov-chain-demo transition-matrix
;;                                 &optional 
;;                                 (pie (/ (iseq 1 (array-dimension 
;;                                                   transition-matrix 0))
;;                                         (sum (iseq 1 (array-dimension 
;;                                                   transition-matrix 0)))))
;;                                 (num 10))
;;  where TRANSITION-MATRIX is a transition matrix for a markov chain,
;;  PIE is an initial distribution, and NUM is the number of iterations.
;;
;;  Original idea by Amy Braverman (abraverm@stat.ucla.edu)
;;
;;  Questions, Comments to Jason Bond (jbond@stat.ucla.edu)
;;                         Amy Braverman (abraverm@stat.ucla.edu)
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Prototype Definitions
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defproto markov-proto '(transition-plot dim p-n-mat p-mat
                         time num pie start-pie pie-plot))

(defproto markov-transition-graph-proto '(parent) () spin-proto)
(defproto markov-pie-graph-proto '(parent) () scatterplot-proto)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  :isnew method initializes slots, makes Transition-Mxtrix  plot 
;;  and Stationary Distribution plot, initializes values. etc.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmeth markov-proto :isnew (p-mat pie num)
  (send self :p-n-mat p-mat)
  (send self :p-mat p-mat)
  (send self :dim (array-dimension p-mat 0))
  (send self :time 0)
  (send self :pie pie)
  (send self :start-pie pie)
  (send self :num num)
  (let ((transition-plot (send markov-transition-graph-proto :new 3 
                                 :showing-axes nil))
        (pie-plot (send markov-pie-graph-proto :new 2 :location 
#+macintosh (list 300 40)
#+msdos (list 400 0)
#+unix (list 600 50)
)))
    (send transition-plot :transformation (matrix (list 3 3) (list
               -0.51449 0.85581 0.05375 -0.71894 -0.46467 0.51691 
                0.46735 0.22731 0.85435)))
    (send transition-plot :margin 0 80 0 25)
    (send pie-plot :margin 0 25 0 0)
    (send pie-plot :reverse-colors)
    (when (and (screen-has-color) (< (send self :dim) 8))
          (send pie-plot :use-color t)
          (send transition-plot :use-color t))
    (send transition-plot :parent self)
    (send pie-plot :parent self)
    (send transition-plot :add-overlay (send replay-overlay-proto :new))
    (send transition-plot :add-overlay (send show-time-overlay-proto :new))
    (send transition-plot :add-overlay (send restart-overlay-proto :new))
    (send transition-plot :add-overlay (send increment-time-overlay-proto :new))
    (send pie-plot :add-overlay (send show-time-overlay-proto :new))
    (send pie-plot :add-overlay (send description-overlay-proto :new))
    (send self :transition-plot transition-plot)
    (send self :pie-plot pie-plot)
    (send self :draw-state-0)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Description Overlay
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defproto description-overlay-proto nil nil graph-overlay-proto)

(defmeth description-overlay-proto :redraw ()
 (let ((graph (send self :graph)))
   (send graph :draw-string (format nil "Stationary Distribution") 5 20)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Re-Start Overlay
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defproto restart-overlay-proto nil nil graph-overlay-proto)

(defmeth restart-overlay-proto :redraw ()
 (let ((graph (send self :graph)))
   (send graph :frame-rect 10 30 10 10)
   (send graph :draw-string "Re-Start" 25 40)))

(defmeth restart-overlay-proto :do-click (x y m1 m2)
 (let* (
        (graph (send self :graph))
        (parent (send graph :parent))
       )
   (cond ((and (< 10 x 20) (< 30 y 40))
         (send parent :p-n-mat (send parent :p-mat))
         (send parent :pie (send parent :start-pie))
         (send parent :time 0)
         (send parent :draw-current-states)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Play Overlay
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defproto replay-overlay-proto nil nil graph-overlay-proto)

(defmeth replay-overlay-proto :redraw ()
 (let ((graph (send self :graph)))
   (send graph :frame-rect 10 10 10 10)
   (send graph :draw-string "Play" 25 20)))

(defmeth replay-overlay-proto :do-click (x y m1 m2)
 (let* (
        (graph (send self :graph))
        (parent (send graph :parent))
        (num (send parent :num))
        (dim (send parent :dim))
       )
   (cond ((and (< 10 x 20) (< 10 y 20))
         (send parent :p-n-mat (send parent :p-mat))
         (send parent :pie (send parent :start-pie))
         (send parent :time 0)
         (send parent :draw-current-states)
         (dotimes (i num)
           (send parent :iterate))))))
    

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Show Time Overlay
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defproto show-time-overlay-proto nil nil graph-overlay-proto)


(defmeth show-time-overlay-proto :redraw ()
 (let* (
        (graph (send self :graph))
        (time (send (send graph :parent) :time))
       )
   (apply #'send graph :draw-string (format nil "Time: ~a" time) 

#-unix (list 190 20)
#+unix (list 320 20)
)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Increment Time Overlay
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defproto increment-time-overlay-proto nil nil graph-overlay-proto)

(defmeth increment-time-overlay-proto :redraw ()
 (let ((graph (send self :graph)))
   (send graph :frame-rect 10 50 10 10)
;   (send graph :frame-rect 90 50 10 10)
   (send graph :draw-string "+ Time" 25 60)))

(defmeth increment-time-overlay-proto :do-click (x y m1 m2)
 (let* (
        (graph (send self :graph))
        (parent (send graph :parent))
        (num (send parent :num))
        (dim (send parent :dim))
       )
   (cond ((and (< 10 x 20) (< 50 y 60))
          (send parent :iterate)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;  Draws initial states
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmeth markov-proto :draw-state-0 ()
  (send (send self :transition-plot) :draw-initial-state)
#+macintosh (send (send self :transition-plot) :redraw)
  (send (send self :pie-plot) :draw-current-state))


(defmeth markov-transition-graph-proto :draw-initial-state ()
 (send self :clear-lines)
 (let* (
        (parent (send self :parent))
        (p-mat (send parent :p-mat))
        (dim (send parent :dim))
        (colors (if (and (< dim 8) (screen-has-color))
                    (select *colors* (list 2 3 4 5 6 7 0)) (repeat nil dim)))
       )
  (send self :start-buffering)
  (dotimes (i dim)
    (dotimes (j dim)
      (let ((p-i-j (aref p-mat i j)))
        (send self :draw-box p-i-j i j dim (elt colors j)))))
; (send self :thicken-lines dim)
  (send self :adjust-to-data)
  (send self :buffer-to-screen)))

(defmeth markov-transition-graph-proto :draw-box (p-i-j i j dim color)
 (let (
       (ibot (- i .25))
       (itop (+ i .25))
       (jbot (- j .25))
       (jtop (+ j .25))
      )
 (send self :add-lines (list (list ibot itop itop ibot ibot) 
                             (list jbot jbot jtop jtop jbot) 
                             (list p-i-j p-i-j p-i-j p-i-j p-i-j)))
 (send self :add-lines (list (list ibot ibot) (list jbot jbot) (list 0 p-i-j)))
 (send self :add-lines (list (list itop itop) (list jbot jbot) (list 0 p-i-j)))
 (send self :add-lines (list (list itop itop) (list jtop jtop) (list 0 p-i-j)))
 (send self :add-lines (list (list ibot ibot) (list jtop jtop) (list 0 p-i-j)))
 (if color (send self :linestart-color (iseq (* 13 (+ j (* i dim)))
                                             (+ 4 (* 13 (+ j (* i dim)))))
                                       (repeat color 5)))))


(defmeth markov-transition-graph-proto :draw-box2  (p-i-j i j)
 (send self :add-lines (list (list i i) (list j j) (list 0 p-i-j))))

(defmeth markov-transition-graph-proto :thicken-lines (dim)
   (send self :linestart-width (iseq (* 2 dim)) (repeat 8 (* 2 dim))))

(defmeth markov-pie-graph-proto :draw-current-state ()
 (send self :start-buffering)
 (send self :clear-lines)
 (let* (
        (parent (send self :parent))
        (pie (send parent :pie))
        (dim (send parent :dim))
        (seq (if (= dim 2) (list 1.5) (+ .5 (iseq 1 (1- dim)))))
        (xseq (combine (repeat .5 2) (repeat seq (repeat 3 (- dim 1)))
                       (repeat (+ dim .5) 2)))
        (yseq (combine (mapcar #'(lambda (x) (list 0 x x)) pie) (list 0)))
        (colors (if (and (< dim 8) (screen-has-color))
                    (select *colors* (list 2 3 4 5 6 7 0)) (repeat nil dim)))
       )
   (send self :add-lines (list xseq yseq))
   (send self :linestart-color (+ 1 (* 3 (iseq dim)))
                               (select colors (iseq dim)))
   (send self :adjust-to-data)
   (send self :buffer-to-screen)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Updates Transition Plot for next state
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmeth markov-transition-graph-proto :draw-current-state ()
 (let* (
        (parent (send self :parent))
        (dim (send parent :dim))
        (p-n-mat (send parent :p-n-mat))
       )
     (send self :start-buffering)
     (dotimes (i dim)
       (dotimes (j dim)
         (send self :new-state (aref p-n-mat i j) i j dim)))
     (send self :adjust-to-data)
     (send self :buffer-to-screen)))

(defmeth markov-proto :draw-current-states ()
  (let (
        (pie-plot (send self :pie-plot))
        (transition-plot (send self :transition-plot))
       )
     (when pie-plot (send pie-plot :draw-current-state))
     (send transition-plot :draw-current-state)))


(defmeth markov-transition-graph-proto :new-state (p-i-j i j dim)
 (let ((start-line (* 13 (+ j (* i dim)))))
   (send self :linestart-coordinate (repeat 2 5) 
                                    (iseq start-line (+ 4 start-line))
                                    (repeat p-i-j 5))
   (send self :linestart-coordinate (repeat 2 4)
                                    (+ start-line 6 (* 2 (iseq 4)))
                                    (repeat p-i-j 4))))

(defmeth markov-transition-graph-proto :new-state2 (p-i-j i j dim)
 (let ((start (+ j (* i dim))))
   (send self :linestart-coordinate 2 (1+ (* 2 start)) p-i-j)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Iteration process.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defmeth markov-proto :iterate ()
 (let (
       (dim (send self :dim))
       (transition-plot (send self :transition-plot))
       (pie-plot (send self :pie-plot))
       (pie (send self :pie))
       (p-mat (send self :p-mat))
       (p-n-mat (matmult (send self :p-mat) (send self :p-n-mat)))
      )
     (send self :pie (inner-product pie p-n-mat))
     (send self :p-n-mat p-n-mat)
     (send self :time (1+ (send self :time)))
     (send self :draw-current-states)))



(defmeth markov-pie-graph-proto :close ()
  (let ((parent (send self :parent)))
    (call-next-method)
    (send parent :kill-pie)))

(defmeth markov-proto :kill-pie ()
  (setf (slot-value 'pie-plot) nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;   Accessor Methods 
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro normal-accessor (key slot prototype)
`(defmeth ,prototype ,key (&optional (content nil set))
   (when set (setf (slot-value ',slot) content))
   (slot-value ',slot)))

(normal-accessor :transition-plot transition-plot markov-proto)
(normal-accessor :dim dim markov-proto)
(normal-accessor :p-mat p-mat markov-proto)
(normal-accessor :p-n-mat p-n-mat markov-proto)
(normal-accessor :time time markov-proto)
(normal-accessor :num num markov-proto)
(normal-accessor :pie-plot pie-plot markov-proto)
(normal-accessor :pie pie markov-proto)
(normal-accessor :start-pie start-pie markov-proto)

(normal-accessor :parent parent markov-transition-graph-proto)
(normal-accessor :parent parent markov-pie-graph-proto)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Test Matrix 1
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
#|
(def *start-mat* (matrix (list 3 3) (list 
.3 .5 .2 
.7 .2 .1 
.4 .4 .2)))
|#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Test Matrix 2
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def *start-mat* (matrix (list 5 5) (list 
.4 .3 .2 0 .1
.2 .1 .1 .4 .2
.1 .1 .5 .1 .2 
0 .3 .1 .2 .4
.1 .2 .3 .2 .2)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Test Matrix 3
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
#|
(def *start-mat* (matrix (list 3 3) (list 
.3 .3 .4 
.4 .3 .3 
.3 .4 .3)))
|#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Test Matrix 4
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;(def *start-mat* (matrix (list 2 2) (list 0 1 1 0)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Test Matrix 5
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
#|
(def *start-mat* (matrix (list 8 8) (list 
.1 .1 .2 .3 .2 0 .05 .05
.05 .1 .05 .1 .1 .4 .1 .1
.1 .2 .1 .1 .2 .1 .1 .1
.1 0 .1 .3 .1 .2 .1 .1
.1 .1 .1 .2 .3 .1 .1 0
.2 .1 .3  .2 .05 .1 .05 0
0 .1 .2 .1 0 .3 .2 .1
.1 .1 .1 .1 .1 .1 .1 .3)))
|#
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Test Matrix 6
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|
(def *start-mat* (matrix (list 5 5) (list
1 0 0 0 0
0 .2 .8 0 0
0 .6 .4 0 0
0 0 0 .5 .5
0 0 0 .2 .8)))
|#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Function to start demo.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun markov-chain-demo (transition-matrix 
                                   &optional 
                                   (pie (/ (iseq 1 (array-dimension 
                                                   transition-matrix 0))
                                           (sum (iseq 1 (array-dimension 
                                                   transition-matrix 0)))))
                                   (num 10))
  (send markov-proto :new transition-matrix pie num))


(def markov (markov-chain-demo *start-mat*))






