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
;;                                 (num 20))
;;  where TRANSITION-MATRIX is a transition matrix for a markov chain,
;;  PIE is an initial distribution, and NUM is the number of iterations.
;;
;;  Original idea by Amy Braverman (abraverm@stat.ucla.edu)
;;
;;  Questions, Comments to Jason Bond (jbond@stat.ucla.edu)
;;                         Amy Braverman (abraverm@stat.ucla.edu)


;;
;;  Prototype Definitions
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defproto markov-proto '(transition-plot dim p-n-mat p-mat time num
                         pie start-pie pie-plot temp-trans-mat temp-pie 
                         parameter-dialog))

(defproto markov-transition-graph-proto '(parent) () spin-proto)
(defproto markov-pie-graph-proto '(parent) () scatterplot-proto)
(defproto markov-dialog-proto nil nil dialog-proto)

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
                                 :title "N-Step Transition Matrix" 
                                 :showing-axes nil :go-away nil))
        (pie-plot (send markov-pie-graph-proto :new 2 :go-away nil
                      :title "Stationary Distribution"
                     :location 
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
    (send transition-plot :add-overlay (send back-overlay-proto :new))
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
;;  Back to Best Viewing Position Overlay
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defproto back-overlay-proto nil nil graph-overlay-proto)

(defmeth back-overlay-proto :redraw ()
 (let ((graph (send self :graph)))
   (send graph :frame-rect 10 70 10 10)
   (send graph :draw-string "Back" 25 80)))

(defmeth back-overlay-proto :do-click (x y m1 m2)
 (let ((graph (send self :graph)))
   (cond ((and (< 10 x 20) (< 70 y 80))
         (send graph :transformation (matrix (list 3 3) (list
               -0.51449 0.85581 0.05375 -0.71894 -0.46467 0.51691 
                0.46735 0.22731 0.85435)))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;  Draws initial states
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmeth markov-proto :draw-state-0 ()
  (send (send self :transition-plot) :draw-initial-state)
#+macintosh (send (send self :transition-plot) :redraw)
  (when (send self :parameter-dialog) (send self :update-parameter-dialog))
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
        (dialog (send self :parameter-dialog))
       )
     (send transition-plot :draw-current-state)
     (when dialog (send self :update-parameter-dialog))
     (when pie-plot (send pie-plot :draw-current-state))))

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
     (send self :pie (combine (matmult (matrix (list 1 dim) pie) p-mat)))
     (send self :p-n-mat p-n-mat)
     (send self :time (1+ (send self :time)))
     (send self :draw-current-states)))

(defmeth markov-proto :start-end-parameter-dialog ()
  (if (send self :parameter-dialog) 
      (progn (send (slot-value 'parameter-dialog) :remove)
             (setf (slot-value 'parameter-dialog) nil))
      (send self :start-parameter-dialog)))



(defmeth markov-proto :start-parameter-dialog ()
  (let* (
         (dim (send self :dim))
         (time (send self :time))
         (trans-step (send text-item-proto :new (format nil "~a-Step" (1+ time))))
         (pie-step (send text-item-proto :new (format nil "~a  " time)))
         (trans-tell (send text-item-proto :new " Transition Matrix:"))
         (transition-matrix (send self :p-n-mat))
         (pie-n (coerce (send self :pie) 'list))
         (trans-show (mapcar #'(lambda (x) (mapcar #'(lambda (y)
                       (send text-item-proto :new (format nil "~4,3f" y)))
                        (coerce x 'list))) (column-list transition-matrix)))
         (pie-tell (send text-item-proto :new "Pie at time:"))
         (pie-show (mapcar #'(lambda (x) (send text-item-proto :new 
                         (format nil "~4,3f" x))) pie-n))
         (dialog (send dialog-proto :new (list (list trans-step trans-tell) 
                                            trans-show 
                                           (list pie-tell pie-step) pie-show) 
                                    :go-away nil
                                    :location
#-unix (list 415 280)
#+unix (list 450 550)
))
        )
    (send self :parameter-dialog dialog)))


 
(defmeth markov-proto :update-parameter-dialog ()
  (let* (
         (dialog (send self :parameter-dialog))
         (time (send self :time))
         (items (send dialog :items))
         (trans-tell (elt items 1))
         (trans-time (elt (elt items 0) 0))
         (pie-time (elt (elt items 2) 1))
         (pie-tell (elt items 3))
         (p-n-mat (send self :p-n-mat))
         (pie (send self :pie))
        )
    (send trans-time :text (format nil "~a-Step" (1+ time)))
    (send pie-time :text (format nil "~a" time))
    (mapcar #'(lambda (x y) (mapcar #'(lambda (z w) 
                                (send z :text (format nil "~4,3f" w)))
                 x (coerce y 'list))) trans-tell (column-list p-n-mat))
    (mapcar #'(lambda (x y) (send x :text (format nil "~4,3f" y))) 
        pie-tell (coerce pie 'list))))


(defmeth markov-pie-graph-proto :close ()
  (let ((parent (send self :parent)))
    (call-next-method)
    (send parent :kill-pie)))

(defmeth markov-proto :kill-pie ()
  (setf (slot-value 'pie-plot) nil))

(defmeth markov-proto :kill-everything ()
  (when (slot-value 'pie-plot) 
        (send (slot-value 'pie-plot) :close))
  (when (slot-value 'transition-plot)
        (send (slot-value 'transition-plot) :close))
  (when (slot-value 'parameter-dialog)
        (send (slot-value 'parameter-dialog) :close)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Function to start demo.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun markov-chain-demo (transition-matrix 
                                   &optional 
                                   (num 20))
 (let (
       (markov (send markov-proto :new transition-matrix 
                    (make-pie (array-dimension transition-matrix 0)) num))
      )
  (def mark markov)
   (start-markov-dialog markov)))



(defun make-pie (dim)
    (/ (iseq 1 dim) (sum (iseq 1 dim))))


(defun check-probs (probs)
  (if (member nil (mapcar #'(lambda (x) 
               (and (not (member nil (mapcar #'numberp x)))
                    (= 1 (read-from-string (format nil "~5,4f" (sum x)))))) 
                      probs)) 
                     nil probs))

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
(normal-accessor :temp-trans-mat temp-trans-mat markov-proto)
(normal-accessor :temp-pie temp-pie markov-proto)
(normal-accessor :parameter-dialog parameter-dialog markov-proto)

(normal-accessor :parent parent markov-transition-graph-proto)
(normal-accessor :parent parent markov-pie-graph-proto)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Test Matrix 1
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def *start-mat-4* (matrix (list 4 4) (list
.1 .2 .3 .4 
.3 .1 .5 .1 
0 .4 .2 .4
.1 0 .6 .3)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Test Matrix 2
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def *start-mat-5* (matrix (list 5 5) 
(list 0 0 0 1 0 .2 .1 .1 .4 .2 .1 .1 .5 .1 .2 1 0 0 0 0 .1 .2 .3 .2 .2)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Test Matrix 3
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(def *start-mat-3* (matrix (list 3 3) (list 
.2 .4 .4 
.1 .3 .6 
.0 .9 .1)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Test Matrix 4
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(def *start-mat-2* (matrix (list 2 2) (list 0 1 1 0)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Test Matrix 5
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def *start-mat-7* (matrix (list 7 7) (list 
.1 .1 .2 .3 .2 0 .1
.05 .1 .05 .1 .1 .4 .2
.1 .2 .1 .1 .2 .1 .2
.1 0 .1 .3 .1 .4 0
0 0 0 0 0 0 1
0 0 0 0 1 0 0
0 .1 .2 .1 0 .4 .2)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Test Matrix 6
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def *start-mat-6* (matrix (list 6 6) (list
1 0 0 0 0 0
0 .2 .8 0 0 0
0 .6 .4 0 0 0
0 0 0 .5 .3 .2
0 0 0 .2 .7 .1
0 0 0 .5 .4 .1)))


(defun start-markov-dialog (markov)
 (let* (
        (get-choice (send choice-item-proto :new
                          (list "Use a pre-defined Transition Matrix"
                                "Enter a Transition Matrix")))
        (print-num (send text-item-proto :new "2"))
        (ask-num (send text-item-proto :new "Number of States: "))
        (get-num (send sequence-scroll-item-proto :new (iseq 2 7)
                     :action #'(lambda (x) 
                         (send print-num :text (format nil "~a" x)))))
        (tell-valid (send text-item-proto :new "" :text-length 30))
        (close-plots (send button-item-proto :new "Enough!"
                            :action #'(lambda () (send markov :kill-everything)
                                                 (send big-dialog :remove))))
        (see-parameters (send button-item-proto :new "+/- Parameters"
             :action #'(lambda () (send markov :start-end-parameter-dialog))))
        (ok (send button-item-proto :new "Ok" :action #'(lambda ()
               (send tell-valid :text "")
               (let (
                     (type (send get-choice :value))
                     (dim (read-from-string (send print-num :text)))
                     (parm-dialog (send markov :parameter-dialog))
                    )
                 (if (= type 1)
                   (let* (
                          (ask-probs (send text-item-proto :new 
                                       "Enter Transition Matrix:"))
                          (ask-pie (send text-item-proto :new 
                                       "Enter Initial Distribution:"))
                          (temp-mat (send markov :temp-trans-mat))
                          (temp-pie (send markov :temp-pie))
                          (bool (if temp-mat (if (= (length (combine temp-mat))
                                                     (^ dim 2)) t) nil))
                          (seq (if temp-mat (iseq dim)))
                          (get-probs (mapcar #'(lambda (x) (mapcar #'(lambda (y)
                                      (send edit-text-item-proto :new 
                                        (if (and bool y)
                                            (format nil "~a" y) "")
                                             :text-length 4))
                                        (if bool x (iseq dim))))
                                             (if bool temp-mat (iseq dim))))
                          (get-pie (mapcar #'(lambda (x)
                                       (send edit-text-item-proto
                                     :new (if (and bool x) 
                                            (format nil "~a" x) "") 
                                               :text-length 4))
                                      (if bool temp-pie (iseq dim))))
                          (cancel (send modal-button-proto :new "Cancel"
                                     :action #'(lambda ())))
                          (alright (send modal-button-proto :new "Do it" 
                               :action #'(lambda ()
                                (let* (
                                       (trans (mapcar #'(lambda (x) 
                                                (mapcar #'(lambda (y)
                                              (if (> (length (send y :text)) 0)
                                               (read-from-string 
                                                  (send y :text)))) x))
                                                     get-probs))
                                       (pie-0 (mapcar #'(lambda (x) 
                                              (if (> (length (send x :text)) 0)
                                                  (read-from-string
                                                     (send x :text))))
                                                 get-pie))
                                       (good-trans (check-probs trans))
                                       (good-pie (check-probs (list pie-0)))
                                      )
                              (send markov :temp-trans-mat trans)
                              (send markov :temp-pie pie-0)
                              (if (and good-trans good-pie)
                                  (let (
                                        (mat (matrix (list dim dim)
                                        (combine good-trans)))
                                        (pie (combine good-pie))
                                       )
                                    (send markov :p-mat mat)
                                    (send markov :p-n-mat mat)
                                    (send markov :pie pie)
                                    (send markov :start-pie pie)
                                    (send markov :dim dim)
                                    (send markov :time 0)
                                    (send markov :draw-state-0))
                                    (if good-trans
                                    (send tell-valid :text
                                         "Not a Valid Initial Distribution!")
                                    (send tell-valid :text
                                        "Not a Valid Transition Matrix!")))))))
                           (dialog (send modal-dialog-proto :new 
                                      (list ask-probs (transpose get-probs) 
                                            ask-pie get-pie
                                            (list alright cancel))))
                         )
                       (send dialog :modal-dialog))
                    (let* (
                           (*start-mat* (case dim (2 *start-mat-2*)
                                                  (3 *start-mat-3*)
                                                  (4 *start-mat-4*)
                                                  (5 *start-mat-5*)
                                                  (6 *start-mat-6*)
                                                  (7 *start-mat-7*)))
                           (pie (make-pie (array-dimension *start-mat* 0)))
                          )
                       (send markov :p-mat *start-mat*)
                       (send markov :p-n-mat *start-mat*)
                       (send markov :pie pie)
                       (send markov :start-pie pie)
                       (send markov :dim (array-dimension *start-mat* 0))
                       (send markov :time 0)
                       (send markov :draw-state-0)))
                       (if parm-dialog (progn
                             (send markov :start-end-parameter-dialog)
                             (send markov :start-parameter-dialog)))))))

                    (big-dialog (send markov-dialog-proto :new (list get-choice 
                                                     (list ask-num print-num)
                                                         get-num
                                                      tell-valid 
                                         (list ok see-parameters close-plots))
                                        :go-away nil
                                        :title "Markov Chain Dialog" :location 
#-unix (list 5 300)
#+unix (list 50 600)
)))))


(defmeth markov-dialog-proto :remove () 
(exit))

(markov-chain-demo *start-mat-2*)
