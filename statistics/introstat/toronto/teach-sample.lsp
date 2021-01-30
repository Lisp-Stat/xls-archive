;;;;
;;;;  This file contains code to illustrate some ideas central to
;;;;  random sampling:
;;;;  
;;;;    that a histogram of the sample will have the same shape
;;;;    as the parent distribution, for a large enough sample size,
;;;;    but that small samples will often appear "unusual"
;;;;    
;;;;    that the distribution of the standardized sample-mean from any
;;;;    distribution tends toward the normal as the sample size
;;;;    increases
;;;;
;;;;    that the sample mean converges to the true mean as the sample
;;;;    size increases
;;;;
;;;;    that the variance of the sample mean decreases
;;;;    as the sample size increases.
;;;;    
;;;;    that a confidence interval is a random interval, with a
;;;;    specified probability of including the true value.
;;;;
;;;;  With a litle imagination, the routines can be used to show other
;;;;  things, such as the validity of certain approximations using the
;;;;  normal.

;;;;
;;;;  These are some functions used throughout the code
;;;;
;;;;

;;   defines a function to load in this program and start up the menus

(defun ls() (load "sample.lsp"))

;;; This section defines a normal-rand and normal-density that will accept
;;; a dummy argument and not give an error message.  These functions
;;; are passed as arguments, so must have the same usage as functions that
;;; accept an argument.

(defun normal-rand-dummy (n dummy) (normal-rand n))
(defun normal-dens-dummy (x dummy) (normal-dens x))

;;;  These functions define density and random number generators
;;;  for the exponential distributions.
;;;

(defun exponential-dens (x mean)
  (* (/ 1 mean) (exp (/ x (* -1 mean)))))
(defun exponential-rand (n mean)
  (* -1 mean (log (uniform-rand n))))
;;;
;;;  This function accepts a list representing a sample and returns a list
;;;  of the low and high ends of a 95% CI and the sample mean.
;;;  The CI is: XBAR + (-) t(0.025)*s/(SQRT(n)
;;;

(defun ci-generate (x)
  (let* (  ( n (length x))
    ( x-bar (mean x))
    ( s (standard-deviation x))
    ( t-alpha (t-quant 0.975 (- n 1)))
    ( x-low (- x-bar (* t-alpha s (/ 1 (sqrt n)))))
    ( x-high (+ x-bar (* t-alpha s (/ 1 (sqrt n))))))
   (list x-low x-high x-bar)))
;;;
;;;  This function accepts the arguments :
;;;  sampling distribution
;;;  sample-size
;;;  true mean
;;;  true sd
;;;  number of samples to generate  
;;;
;;;  It returns a list of standardized sample means:
;;;    (XBAR-TRUE.MEAN)/(S/SQRT(N))
;;;
(defun normalized-mean (function sample-size mean sd number-of-samples)
  (let* ((samples (funcall function (repeat sample-size number-of-samples)
             (repeat mean number-of-samples)))
         (sample-means (mapcar #'mean samples)))
  (/ (- sample-means mean) (/ sd (sqrt sample-size)))))


;;;;  POISSON AND BINOMIAL DISTRIBUTION FUNCTIONS;;;;

;;
;;   These functions (yvals-top-***) calculate the y-values for the tops of the bars
;;   of the probablity function (f) and return 2 values for each x value
;;   One takes 2 arguments and the other 3.

(defun yvals-top-bin (p n max-x)
  (map 'list #'(lambda (x) (repeat (binomial-pmf x n p) 2))
       (iseq (+ max-x 1))))

(defun yvals-top-pois (mean max-x)
  (map 'list #'(lambda (x)
      (repeat (poisson-pmf x mean) 2))
    (iseq (+ max-x 1))))

;;   Functions in this section attempt to duplicate XLISP-STAT's handling of discrete
;;   data in histograms by setting the pmf in each unit increment to the average
;;   of the true pmf in adjacent intervals (e.g. P-hist(1.5)=0.5*(P-true(1)+P-true(2)
;;   However, it does not seem to work terribly well, and depends on the bins size.
;;   An easier thing to do might be write a modification for "histogram" that
;;   recognizes discrete data.

(defun yvals-top-pois-hist (mean max-x)
  (let* (  (y (poisson-pmf (iseq (+ 1 max-x)) mean ))
     (y-hist (combine (list   
        (+ (select y 0) (* 0.5 (select y 1)))
        (* 0.5 (+ (select y (iseq 1 (- max-x 1)))
            (select y (iseq 2 max-x))))
        (* 0.5 (select y max-x))))))
  (map 'list #'(lambda (x)
      (repeat x 2))
    y-hist)))
;;
;;   yvals-all-hist creates the correct pattern of y-values and zeroes
;;   for the tops and bottoms of the bars that matches 
;;  that expected on a histogram.
;;

(defun yvals-all-hist (function p1 p2 max-x)
  (combine 0 (map 'list #'(lambda (x) (combine x 0))
     (yvals-top-pois-hist  p1 max-x))))
(defun discrete-pmf-hist (function p1 p2 max-x)
           (list (xvals-all max-x)  (yvals-all-hist function p1 p2 max-x)))

;; yvals-all creates the correct pattern of y-values and zeroes
;; for the tops and bottoms of the bars.
;; The pattern is: (0,p(0),p(0),0,p(1),p(1),0,p(2),p(2),0,...p((max_x)).p((max_x)),0)
;;
(defun yvals-all (function p1 p2 max-x)
  (combine 0 (map 'list #'(lambda (x) (combine x 0))
    (if (eql function 'pois-pmf)
             (yvals-top-pois  p1 max-x)
             (yvals-top-bin   p1 p2 max-x)  ))))

;;
;; The function xvals-all creates the correct pattern of x-values for the
;; lines needed to draw the probability function.
;; The pattern is: (0,0,1,1,1,2,2,2,3,3,...,(max_x),(max_x)+1,(max_x)+1)
;;
(defun xvals-all (max-x)
   (combine   (list 0 0
           (repeat (iseq 1 max-x) (repeat 3 max-x) )
                (+ max-x 1) (+ max-x 1))))
;;
;;   This function returns the vectors required for drawing the lines 
;;  of the barplot of a discrete probability function.
;;
(defun discrete-pmf (function p1 p2 max-x)
           (list (xvals-all max-x)  (yvals-all function p1 p2 max-x)))



;;;
;;;  This defines an object that acts as an external counter
;;;  It has a slot for the current "count" and an acessor method
;;;  for it.
;;;
(setf counter (send *object* :new))
(send counter :add-slot 'n 0)
(defmeth counter :n ()
  (setf (slot-value 'n) (+ (slot-value 'n) 1))
  (slot-value 'n))

;;;
;;;  This section defines menu items and their actions.
;;;  Most of the meat of the code is in the :action section.
;;;  By keeping the graph local to the function, separate
;;;  calls to the same menu item are kept distinct.
;;;  This means that multiple instances of the same program can be 
;;;  run without having the dialog box of one referring to the 
;;;  plot of another.
;;;

;;
;;  This defines the CI menu item
;;

(setf ci-item
(send menu-item-proto :new "Confidence Interval Generation"
  :action
  #'(lambda()
;  sets up a graph with a bold line running top to bottom
;  and an x-axis range of -6 to 6
    (let* (  (xrange-low -6)
      (xrange-high 6)
      (number-of-samples 100))
        (setf w (send graph-proto :new 2))
        (send w :title "Bold Line is Population Mean")
        (send w :add-lines (list (list 0 0) (list 1 number-of-samples )))
        (send w :linestart-width '(0 1) '(2 2))
        (send w :range 0 xrange-low xrange-high)
        (send w :range 1 -2 (+ number-of-samples 5))
        (send w :x-axis t nil 13)
        (send w :redraw)
;  this let* list creates items that are installed in a dialog
;  box.  Each item has instructions within it.

;  defines the s.s. options that are presented to the user
      (let* ( (sample-sizes (list 5 10 20 80))
;  creates some text-items to be used in the dialog box
        (n-label (send text-item-proto :new "Size of Sample"))
        (n-value (send text-item-proto :new "5" :text-length 10))
        (n-scroll  
;  creates a scroll-bar to select the sample size
        (send sequence-scroll-item-proto :new
                            sample-sizes
                            :text-item n-value
                            :action #'(lambda(x))))
;  a help button and dialog
        (help (send button-item-proto :new
          "Help"
                            :action
                       #'(lambda ()
            (message-dialog "Collect Sample: calculates a 95% confidence~%~
                 confidence interval for a sample of size n~%~
                 from N(0,4) and then plots it relative to ~%~
                 the true population mean.  The sample size ~%~
                 is selected with the scroll-bar.~%~
                 ~%~
                 The Graph: The horizontal line represents the ~%~
                 C.I. and the dot represents the sample mean."))))
;  a close button
        (close-all (send button-item-proto :new
          "Close Plot"
                            :action
                       #'(lambda ()
            (send w :close)
            (send d :remove)
            (Send counter :slot-value 'n 0)
            (gc))))
;   the "hot" button: this sends a message to the plot to add a new
;  confidence interval.
        (send-points (send button-item-proto :new
          "Collect Sample"
                            :action
                       #'(lambda ()
;  this is the y-value on the plot
            (let* ((sample-number (send counter :n))
;  this is the sample size
                   (sample-size (select sample-sizes (send n-scroll :value)))
;  this calls the ci-generate function to get the endpoints and sample mean
                   (ci-list (ci-generate
                (* 2 (normal-rand sample-size)))))
;  this is just so that the user does not mistakenly erase the graph, or over-fill it
                  (if (= 99 sample-number)
                (flet ((warning-of-fill ()
                  (message-dialog "Warning: Plot will fill up~%~
                                   if sent any more samples")))
                  (warning-of-fill)))
                  (if (= 100 sample-number)
                (flet ((graph-filled ()
                  (message-dialog "I meant it: Plot is filled up and it~%~
                                   will close if sent any more samples")))
                  (graph-filled)))
                  (if (< 100 sample-number)
;  this removes the plot after all warnings have been ignored
                (flet ((graph-filled ()
                  (message-dialog "I told you: Plot is filled up and will now close")
                  (send w :close)
                  (send d :remove)
                  (Send counter :slot-value 'n 0)))
                   (graph-filled ))

;  this draws the CI on the plot and marks the mean with a hilited dot
                 (flet ((add-ci ()
                      (send w :add-lines (list (select ci-list (iseq 2))
                         (list sample-number sample-number)))
                      (send w :linestart-next (* 2 sample-number) (+ 1 (* 2 sample-number)))
                      (send w :add-points (list (list (select ci-list 2)) (list sample-number)))
                      (send w :point-state (iseq sample-number ) 'hilited)))
                   (add-ci) ))))))
;  this puts all the dialog-box elements together in one object
        (d (send dialog-proto :new
             (list
             (list n-label n-value)
             n-scroll send-points
             (list help close-all))
             :title "Defining the Sample")) ))))))
;;;
;;;	This creates a menu-item that calls a function to demonstrate
;;;	the central limit theorem.
;;;
;;;
(setf clt-item
(send menu-item-proto :new "Central Limit Theorem"
  :action
  #'(lambda()
;	set up a histogram with the appropriate range and title
    (let* ((h (histogram (iseq -4 4) :title "Distribution of Normalized Sample Mean" :show nil))
;  define a few constants and the options that will be presented to the user
         (number-of-samples 500)
         (function-list-names (list "Normal(0,1)" "Chi-squared(10)" "Poisson(3)" "Exponential(5)"))
         (function-list (list 'normal-rand-dummy 'chisq-rand 'poisson-rand 'exponential-rand))
         (mean-list (list 0 10 3 5))
         (variance-list (list 1 20 3 25))
         (sample-sizes (combine (list (iseq 1 20) 30 40 50 75 100)))
; create text-items to go in the dialog box
         (n-label (send text-item-proto :new "Size of Sample"))
         (n-value (send text-item-proto :new "1" :text-length 10))
         (distribution-label (send text-item-proto :new "Sample from Distribution:"))
;  create a scroll bar to select sample size
         (n-scroll  (send sequence-scroll-item-proto :new
                             sample-sizes
                            :text-item n-value
                            :action #'(lambda(x))))
;	create a set of selection buttons to allow the user to choose a distb'n
        (distribution-choice (send choice-item-proto :new
              function-list-names :value 0))
;	help button and text
        (help (send button-item-proto :new
          "Help"
                            :action
                       #'(lambda ()
            (message-dialog "Collect Sample: generates 500 samples with the ~%~
                 specified size from the specified distribution.~%~
                 A histogram of the 500 standardized sample means~%~
                 is then drawn, with a superimposed reference of N(0,1)"))))
        (close-all (send button-item-proto :new
          "Close Plot"
                            :action
                       #'(lambda ()
            (send h :close)
            (send d :remove)
            )))
;	again, the meat of the program is the send points button
        (send-points (send button-item-proto :new
                            "Collect Sample"
                            :action
;  the action is to generate "number-of-samples" samples of size "sample-size"
;  and then calculate the standardized sample mean for each.  The  histogram is then
;  sent these values; a N(0,1) is superimposed as a reference  
          #'(lambda ()
            (let* ( (sample-size (select sample-sizes (send n-scroll :value)))
              (function-index (send distribution-choice :value))
              (function  (select function-list function-index))
              (mean (select mean-list function-index))
              (sd (sqrt (select variance-list function-index)))
              (normalized-sample-means (normalized-mean function sample-size mean sd number-of-samples)))
;  clears previous points
              (send h :clear-points :draw nil)
;  adds standardized sample means to histogram
              (send h :add-points normalized-sample-means )
;  changes the number of bins so that the plot is smoother
              (send h :num-bins 17)
;  plots the reference
              (send h :add-function #'normal-dens -4 4 )
;  shows the window
              (send h :show-window)))))

;  creates a dialog box holding all of the above items
        (d (send dialog-proto :new
          (list
          distribution-label
          distribution-choice
          (list n-label n-value )
          n-scroll send-points
          (list help close-all))
          :title "Defining the Sample")) )))))



;;;;	Sampling from some common distributions is demonstrated here.
;;;;	This section creates menu-items that build up a histogram
;;;;	of a distribution and superimpose the density as a reference.
;;;;	Since they are so similar, only the first will be fully described.
;;;;
;;;;
;;
;;	Sets up an item for showing a sample from N(0,1)
;;
(setf normal-item
(send menu-item-proto :new "...from Normal"
  :action
  #'(lambda()
;	this precalculates the population, since repeated calls to
;	normal-rand seem slower than calls to sample, and allow for faster
;	redrawing of the screen
    (let* ((pop (normal-rand 10000))
           (h (histogram (select pop (iseq 300))
                          :title "Normal" :show nil)))
      (send h :clear-points)
      (send h :add-function #'normal-dens -4 4 200)
      (send h :adjust-to-data)
      (send h :num-bins 17)
      (send h :show-window)
; defines a local function to add n new points from the population
; to the histogram
      (flet ((new-data (n)
        (send h :add-points
                (select pop (sample (iseq 10000) n nil)))))
; defines the increments by which points can be added to the graph
; they are based on the principle that 4 presses are okay, but 5 are too many
      (let* ( (n-to-send (list 1 5 25 100 500))
; again, defines labels for the items in the dialog box
        (n-label (send text-item-proto :new "Number to Add"))
        (n-value (send text-item-proto :new "1" :text-length 10))
        (n-tot-label (send text-item-proto :new "Current Sample Size"))
        (n-tot-value (send text-item-proto :new "0" :text-length 4))
;defines a scroll bar allowing selection of the number of points to add
        (n-scroll  
        (send sequence-scroll-item-proto :new
                            n-to-send
                            :text-item n-value
                            :action #'(lambda(x))))
        (close-all (send button-item-proto :new
          "Close Plot"
                            :action
                       #'(lambda ()
                                (send d :remove)
                                (send h :close))))
        (send-points (send button-item-proto :new
                      "Add Points"
                       :action
                       #'(lambda ()
; gets the current size of the sample
                           (let ((sample-size (sum (send h :bin-counts))))

; if a histogram gets too big (2000 or more), MS-WINDOWS XLISP-STAT has
; the habit of crashing;  hence this check.
                              (if (> 1500 sample-size )
                                    (new-data 
                                       (select n-to-send (send n-scroll :value)))
                                       (message-dialog "The histogram is dangerously large.~%~
                                                        Close it and start again."))

;  update the sample size shown in the dialog box
                              (send n-tot-value :text (with-output-to-string (s)
                                                 (prin1 (sum (send h :bin-counts)) s )))))))
        (d (send dialog-proto :new
          (list
          (list n-tot-label n-tot-value)
          (list n-label n-value)
          n-scroll send-points
          close-all)
          :title "Sampling from N(0,1)")))))))))

;;;
;;;	Sets up a menu item for sampling from exponential(5)
;;;
;;;
(setf exp-item
(send menu-item-proto :new "...from Exponential"
  :action
  #'(lambda()
    (let* 
     ((pop (exponential-rand 10000 5))
      (h (histogram (iseq 35)
                :title "Exponential" :show nil))
      (xseq (rseq 0 35 200)))
      (send h :clear-points)
      (send h :add-lines (list xseq (exponential-dens xseq 5)))
      (send h :adjust-to-data)
      (send h :num-bins 35)
      (send h :show-window)
      (flet ((new-data (n)
        (send h :add-points
          (select pop (sample (iseq 10000) n t )))))
      (let* ( (n-to-send (list 1 5 25 100 500))
        (n-label (send text-item-proto :new "Number to Add"))
        (n-value (send text-item-proto :new "1" :text-length 10))
        (n-tot-label (send text-item-proto :new "Current Sample Size"))
        (n-tot-value (send text-item-proto :new "0" :text-length 4))
        (n-scroll  
        (send sequence-scroll-item-proto :new
                            n-to-send
                            :text-item n-value
                            :action #'(lambda(x))))
        (close-all (send button-item-proto :new
                            "Close Plot"
                            :action
                               #'(lambda ()
                                    (send d :remove)
                                    (send h :close))))
        (send-points (send button-item-proto :new
                            "Add Points"
                            :action
                               #'(lambda ()
                                   (let ((sample-size (sum (send h :bin-counts))))
                                    (if (> 1500 sample-size )
                                      (new-data (select n-to-send (send n-scroll :value)))
                                      (message-dialog "The histogram is dangerously large.  ~%~
                                                       Close it and start again"))
                                    (send n-tot-value :text (with-output-to-string (s)
                                                       (prin1 (sum (send h :bin-counts)) s )))))))
        (d (send dialog-proto :new
              (list
              (list n-tot-label n-tot-value)
              (list n-label n-value)
              n-scroll send-points
              close-all)
              :title "Sampling from Exponential(5)")))))))))

;;;
;;;	Sets up a menu item for sampling from chi-squared(5)
;;;
;;;
(setf chisq-item
(send menu-item-proto :new "...from Chi-squared"
  :action
  #'(lambda()
       (let* ((pop (chisq-rand 10000 10))
             (h (histogram (iseq 35)
                   :title "Chi-squared" :show nil))
             (xseq (rseq 0 35 200)))
          (send h :clear-points)
          (send h :add-lines (list xseq (chisq-dens xseq 10)))
          (send h :adjust-to-data)
          (send h :num-bins 35)
          (send h :show-window)
          (flet ((new-data (n)
                    (send h :add-points
                          (select pop (sample (iseq 10000) n t )))))
            (let* ((n-to-send (list 1 5 25 100 500))
                   (n-label (send text-item-proto :new "Number to Add"))
                   (n-value (send text-item-proto :new "1" :text-length 10))
                   (n-tot-label (send text-item-proto :new "Current Sample Size"))
                   (n-tot-value (send text-item-proto :new "0" :text-length 4))
                   (n-scroll  
                   (send sequence-scroll-item-proto :new
                                       n-to-send
                                       :text-item n-value
                                       :action #'(lambda(x))))
                   (close-all (send button-item-proto :new
                                "Close Plot"
                                 :action
                                 #'(lambda ()
                                     (send d :remove)
                                     (send h :close))))
                   (send-points (send button-item-proto :new
                                "Add Points"
                                :action
                                 #'(lambda ()
                                      (let ((sample-size (sum (send h :bin-counts))))
                                        (if (> 1500 sample-size )
                                            (new-data (select n-to-send (send n-scroll :value)))
                                            (message-dialog "The histogram is dangerously large.  ~%~
                                                             Close it and start again"))
                                        (send n-tot-value :text (with-output-to-string (s)
                                                       (prin1 (sum (send h :bin-counts)) s )))))))
                   (d (send dialog-proto :new
                          (list
                          (list n-tot-label n-tot-value)
                          (list n-label n-value)
                          n-scroll send-points
                          close-all)
                          :title "Sampling from Chi-squared(10)")))))))))

;;;
;;;	Sets up a menu item for sampling from Poisson(3)
;;;
;;;
(setf poisson-item
(send menu-item-proto :new "...from Poisson"
  :action
  #'(lambda()
    (let* ((pop (poisson-rand 10000 3))
           (h (histogram (iseq 20)
                 :title "Poisson" :show nil)))
      (send h :clear-points)
      (message-dialog "XLISP-STAT histogram does not have special  ~%~
                       methods for count-type data so the bin-counts ~%~
                       for the histogram do not correspond to ~%~
                       response counts and therefore do not match ~%~
                       the probabilities for each count.")
; the only difference here is that the pmf is drawn in bold lines
      (send h :add-lines (discrete-pmf-hist 'pois-pmf 3 nil 12))
      (send h :linestart-width (iseq (+ 4 (* 3 12))) 2)
      (send h :num-bins 20)
      (send h :show-window)
      (flet ((new-data (n)
          (send h :add-points
          (select pop (sample (iseq 10000) n t )))))
      (let* ((n-to-send (list 1 5 25 100 500))
             (n-label (send text-item-proto :new "Number to Add"))
             (n-value (send text-item-proto :new "1" :text-length 10))
             (n-tot-label (send text-item-proto :new "Current Sample Size"))
             (n-tot-value (send text-item-proto :new "0" :text-length 4))
             (n-scroll  (send sequence-scroll-item-proto :new
                            n-to-send
                            :text-item n-value
                            :action 
                             #'(lambda(x))))
             (close-all (send button-item-proto :new
                           "Close Plot"
                            :action
                             #'(lambda ()
                                (send d :remove)
                                (send h :close))))
             (send-points (send button-item-proto :new
                            "Add Points"
                            :action
                             #'(lambda ()
                                 (let ((sample-size (sum (send h :bin-counts))))
                                   (if (> 1500 sample-size )
                                       (new-data (select n-to-send (send n-scroll :value)))
                                       (message-dialog "The histogram is dangerously large.~%~
                                                        Close it and start again"))
            (send n-tot-value :text (with-output-to-string (s)
                    (prin1 (sum (send h :bin-counts)) s )))))))
            (d (send dialog-proto :new
               (list
               (list n-tot-label n-tot-value)
               (list n-label n-value)
               n-scroll send-points
               close-all)
               :title "Sampling from Poisson(3)")))))))))

;;;
;;;  This section creates menus and installs them in the menu bar 
;;;

(setf samp-menu (send menu-proto :new "&Sampling"))
(setf close-item
  (send menu-item-proto :new "Close Menu"
        :action
              #'(lambda() (send samp-menu :dispose) (gc))))
(send samp-menu :append-items
    normal-item
    chisq-item
    exp-item
    (send dash-item-proto :new)
    poisson-item
    (send dash-item-proto :new)
    ci-item
    clt-item
    (send dash-item-proto :new)
    close-item)
(send samp-menu :install)

