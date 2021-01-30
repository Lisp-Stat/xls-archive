; Xlispstat program to demonstrate Central Limit Theorem
; November 1992, Jan Graffelman (graff@upf.es)
; Just load this file and you are on your way.
;;========================================================================
; Construct Title & CopyRight.
;
; The next lines are commented out; they do work interactively, but
; do not work when loaded from file. Bug ?
;
;(setf c (send graph-proto :new 1 :size 200 200 ))
;(send c :location 50 700)
;(send c :title "Central Limit Theorem")
;(send c :draw-string "Jan Graffelman" 100 120)
;
(message-dialog "Demonstration of Central Limit Theorem")
(message-dialog "Written by Jan Graffelman. UPF. November 1992")
;========================================================================
; Construct 2 Histograms for output
;
; Window sizes and locations.
;
(setf hsize (- (round (/ (select (screen-size) 0) 2)) 40))
(setf vsize (- (round (/ (select (screen-size) 1) 2)) 40))
(setf sizes (list hsize vsize))

(setf hloc1 (+ (round (/ (select (screen-size) 0) 2)) 10 ))
(setf vloc1 20)

(setf hloc2 (+ (round (/ (select (screen-size) 0) 2)) 10))
(setf vloc2 (+ (round (/ (select (screen-size) 1) 2)) 20))

(setf h1 (send histogram-proto :new 1 :size sizes))
(send h1 :title "Distribution of One variable") 
(send h1 :location hloc1 vloc1)
(send h1 :num-bins 10)
(send h1 :y-axis t t 0)
(send h1 :add-points (repeat 0 10)) ; dummy data 

(setf h2 (send histogram-proto :new 1 :size sizes))
(send h2 :title "Distribution of the Mean of all variables")
(send h2 :location hloc2 vloc2)
(send h2 :num-bins 10)
(send h2 :y-axis t t 0)
(send h2 :add-points (repeat 0 10)) ; dummy data


;======================================================================
;
; define an object for the data.
;

(defproto my-data '(values sum vars distr) () compound-data-proto)
(def d (send my-data :new))


;======================================================================
; 
; Setup Menu for choosing the type of distribution.
;
(def samsize 1000)


(setf model-menu (send menu-proto :new "Choose Distribution"))



(setf u-item 
	(send menu-item-proto :new "Uniform"
	      :action 
	       #'(lambda () 
;		   (setf choice 0)
		   (send d :slot-value 'distr 0)
		   (display-one)
		   (update))))
;
;
(setf n-item (send menu-item-proto :new "Normal"
                        :action
                        #'(lambda () 
;                            (setf choice 1)
		   (send d :slot-value 'distr 1)
			    (display-one)
			    (update))))
;
;
(setf p-item (send menu-item-proto :new "Poisson"
                        :action
                        #'(lambda () 
;				(setf choice 2)
		   (send d :slot-value 'distr 2)
                                (setf lab (get-value-dialog "Labda: "))
				(display-one)
				(update))))

(setf e-item (send menu-item-proto :new "Exponential"
                        :action
                        #'(lambda () 
;				(setf choice 3)
		   (send d :slot-value 'distr 3)
				(display-one)
				(update))))

(setf c-item (send menu-item-proto :new "Cauchy"
                        :action
                        #'(lambda () 
;				(setf choice 4)
		   (send d :slot-value 'distr 4)
				(display-one)
				(update))))

;
;
(send model-menu :append-items u-item n-item p-item e-item c-item)
;
(send model-menu :install)

;=================================================================
; 
; Methods for the data object

(defmeth my-data :setdistr (x)
  (send self :slot-value 'distr x)
  (slot-value 'distr))

(send d :setdistr 0)

(defmeth my-data :monster ()
    (case (send self :slot-value 'distr)
	  (0 (setf samp (uniform-rand samsize)))
	  (1 (setf samp (normal-rand samsize)))
	  (2 (setf samp (poisson-rand samsize (select lab 0))))
	  (3 (setf samp (exp-rand)))
	  (4 (setf samp (/ (cauchy-rand samsize) 100)))
    )
    (setf (slot-value 'values) samp)
    (slot-value 'values))


(defmeth my-data :clear ()
    (send self :slot-value 'vars 0)
    (send self :slot-value 'values (repeat 0 samsize))
    (send self :slot-value 'sum (repeat 0 samsize))
)

(send d :clear)


;

(defmeth my-data :meansum ()
; calculate the mean of the variables to make a distribution
; of the mean.
  (setf som (send self :slot-value 'sum))
  (setf som (/ som (send dia :value)))
  (send self :slot-value 'sum som)
)

(defmeth my-data :scale ()
; calculate the mean of the variables to make a distribution
; of the mean.
  (setf som (send self :slot-value 'sum))
  (setf som (* som 10))
  (send self :slot-value 'sum som)
)

(defmeth my-data :sumup ()
    (setf som (send self :slot-value 'sum))
    (setf som (+ som (send self :slot-value 'values)))
    (send self :slot-value 'sum som)
    (slot-value 'sum)
)
;
;
(defmeth my-data :show ()
    (slot-value 'sum))
;
(defmeth my-data :plot ()
  (let* ((sums (send self :slot-value 'sum))
	 (mu (mean sums))
	 (s (standard-deviation sums))
 	 (x (rseq (min sums) (max sums) 30))
	 (y (/ (normal-dens (/ (- x mu) s)) s)))
    (send h2 :start-buffering)
    (send h2 :clear)
    (send h2 :erase-window)
    (send h2 :add-points sums t)
  (send h2 :add-lines (list x y) t)
  (send h2 :adjust-to-data)
  (send h2 :num-bins (send dia2 :value))
  (send h2 :y-axis t t 5) 
 (send h2 :buffer-to-screen))
)
;
(defmeth my-data :setvar (x)
   (setf (slot-value 'vars) x)
   (slot-value 'vars))

 
;=================================================================
;
; Functions
;
(defun exp-rand ()
; draw sample from exponential distribution
    (setf ex (* -1 (log (uniform-rand samsize))))
    ex
)

(defun exp-dens (x)
  (^ 2.7182818 (* -1  x)))

(defun unif-dens (x)
  (repeat 1.0 (length x)))

(defun fact (n)
  (let ((n-fac 1))
    (dotimes (i n n-fac)
	     (setf n-fac (* n-fac (+ i 1))))))

;(defun po-dens (x mu)
;  (* (/ (^ mu x) (map 'list #'fact x)) (^ 2.7182818 (- 0 mu))))
  
(defun display-one ()
; shows probability distribution of one variable.
(case (send d :slot-value 'distr)
      (0 (setf x (rseq 0 1 30))
	 (setf y (unif-dens x)))
      (1 (setf x (rseq -4 4 30))
	 (setf y (normal-dens x)))
      (2 (setf x (iseq 0 10))
	 (setf y (poisson-pmf x (select lab 0))))
      (3 (setf x (rseq 0 6 30))
	 (setf y (exp-dens x)))
      (4 (setf x (rseq -4 4 30))
	 (setf y (cauchy-dens x))))
  (send h1 :start-buffering)
  (send h1 :clear)
  (send h1 :erase-window)
;  (send h1 :add-points samp)
  (send h1 :add-lines (list x y) t)
  (send h1 :adjust-to-data)
  (send h1 :num-bins (send dia2 :value))
  (send h1 :y-axis t t 5)
  (send h1 :buffer-to-screen))

(defun get-labda ()
  (setf lab (get-value-dialog "Labda: " ))
  (def x (poisson-rand samsize (select lab 0))))
;
(defun setsample (x)
  (def samsize x)
  (display-one)
  (update))
;
(defun update (&optional x)
  (send d :clear)
  (send d :setvar (send dia :value))
  (dotimes (i (send dia :value))
	   (send d :monster)
	   (send d :sumup))
  (send d :meansum)
  (if (= (send d :slot-value 'distr) 4)
      (send d :scale))
  (send d :plot)
)
;
;
(defun sendbins (x)
  (send h1 :num-bins x t)
  (send h2 :num-bins x t)
)

;=============================================================
; Create Sliders
;
(setf dia (sequence-slider-dialog
          (iseq 100)
:action #'update 
:title "Number of Variables"
:text "#Variables"
))


(send dia :location 100 vloc2)
(send dia :value 2)

(setf dia2 (sequence-slider-dialog
           (iseq 50)
:action #'sendbins
:text "#Bins"
))

(send dia2 :value 28) 
(send dia2 :location (- (select (screen-size) 0) 10) vloc1)


(setf dia3 (sequence-slider-dialog
           (iseq 10000)
:action #'setsample
:text "Sam. Size"
))


(send dia3 :location (- (select (screen-size) 0) 10) vloc2)
(send dia3 :value 1000) 

;  histograms initially 

(send d :clear)

(send h1 :clear)
(send h1 :erase-window)

(send h2 :clear)
(send h2 :erase-window)



















