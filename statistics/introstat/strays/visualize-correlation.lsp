;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                      ;;
;;  This program visualizes correlation. It allows to set a value for a ;;
;;  given correlation coeeficient with a slider. It also allows to play ;;
;;  a movie, starting from a given correlation coefficient and changing ;; 
;;  the y data so that the correlation changes in small steps to a set  ;; 
;;  target value. The target value also can be set with a slider.       ;; 
;;  Most of these functions are controlled from the added Display menu. ;; 
;;  The movie feature is new in the sense that is not offered by an     ;; 
;;  earlier code contribution by jbond@stat.ucla.edu                    ;;
;;  There also is a new object called                                   ;;
;;  formatted-interval-slider-dialog which allows to                    ;; 
;;  control the formatting of the display of the "slider value" which   ;; 
;;  is sometimes untidy with the original dialog.                       ;; 
;;  The mathematics for the correlation stuff has been described in     ;; 
;;  Erich Neuwirth: Visualizing Correlation with a Spreadsheet          ;; 
;;  Teaching Statistics, 12, 1990.                                      ;; 
;;                                                                      ;; 
;;  Comments and criticism to neuwirth@smc.univie.ac.at                 ;; 
;;                                                                      ;;   
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                                                                         
(defproto formatted-interval-scroll-item-proto '(format-string) ()
interval-scroll-item-proto)

(defmeth formatted-interval-scroll-item-proto :format-string (&optional (str nil set))
  (if set (setf (slot-value 'format-string) str))
  (slot-value 'format-string))
      
 
(defmeth formatted-interval-scroll-item-proto :display-value ()
  (if (slot-value 'xlisp::value-text-item)
      (send (slot-value 'xlisp::value-text-item)
            :text (if (send self :format-string) 
                      (format nil (send self :format-string) (send self :value))
                      (num-to-string (send self :value))))))


(defproto formatted-interval-slider-dialog-proto () () dialog-proto)

(defmeth formatted-interval-slider-dialog-proto :isnew 
  (data &key (text "Value") (title "Slider") (use-format "~,2f") action (points 30) (nice t))
  (if nice
      (let ((range (get-nice-range (nth 0 data) (nth 1 data) points)))
        (setq data (list (nth 0 range) (nth 1 range)))
        (setq points (nth 2 range))))
  (let* ((value-item (send text-item-proto :new "              "
                           :location '(100 5)))
         (name-item (send text-item-proto :new text))
         (scroll-item (send formatted-interval-scroll-item-proto :new data 
                            :text-item value-item
                            :action action :points points)))
    (call-next-method (list name-item value-item scroll-item) :title title)
    (send scroll-item :format-string use-format)
    (send scroll-item :display-value)))

(defmeth formatted-interval-slider-dialog-proto :value (&rest args)
  (apply #'send (nth 2 (slot-value 'items)) :value args))

(defun formatted-interval-slider-dialog (&rest args)
"Args: (data &key (text \"Value\") (title \"Slider\") (use-format \"~,2f\") action (points 30) (nice t))
Opens modeless dialog with title TITLE, prompt TEXT, a text display and a
scrollbar. The scrollbar scrolls through the interval DATA, a list of the form
(LOW HIGH), sequence and displays the value. When a scroll event occurs
ACTION is called with the current value in the interval as argument. If NICE
is not NIL DATA and POINTS are revised to produce a nice set of values."
  (apply #'send formatted-interval-slider-dialog-proto :new args))

      

(defun corr-graph-create (n)
  (let* ((x0 (normal-rand n))
         (y0 (normal-rand n))
         (x1 (- x0 (mean x0)))
         (y1 (- y0 (mean y0)))
         (y2 (- y1  (* (inner-product x1 y1) (/ x1 (inner-product x1 x1)))))
         (x (/ x1 (standard-deviation x1)))
         (y (/ y2 (standard-deviation y2)))
         (graph 
          (plot-points  
           x y
           :title "Correlation Graph" 
           :symbol 'dot4
           :range 0 -4 4
           :range 1 -4 4    
           )))
    (progn (send graph :add-slot 'xvals x)
           (send graph :add-slot 'yvals y)
           (send graph :add-slot 'firstregp nil)
           (send graph :add-slot 'secondregp nil)
           (send graph :add-slot 'current.corr 0) 
           (send graph :add-method :firstreg (lambda (dummy &optional (pval nil set))
                                               (if set (progn 
                                                        (send self :slot-value 
                                                             'firstregp pval)
                                                        (send self :set-corr 
                                                              (send self :slot-value 
                                                                    'current.corr)))
                                                   (send self :slot-value 'firstregp))))
           (send graph :add-method :secondreg (lambda (dummy &optional (pval nil set))
                                               (if set (progn 
                                                        (send self :slot-value 
                                                              'secondregp pval)
                                                        (send self :set-corr 
                                                              (send self :slot-value 
                                                                    'current.corr)))
                                                   (send self :slot-value 'secondregp))))
           (send graph :add-method :xvals (lambda (dummy) 
                                      (send self :slot-value 'xvals)))
           (send graph :add-method :yvals (lambda (dummy) 
                                      (send self :slot-value 'yvals)))
           (send graph :add-method :set-corr 
                 (lambda (dummy &optional (r 0)) 
                   (let* ((xorig (send self :xvals)) 
                          (yorig (send self :yvals))
                          (znew (+ (* r xorig) (* (sqrt (- 1 (* r r))) yorig)))
                          (empcorr (/ (inner-product (- xorig (mean xorig)) 
                                               (- znew (mean znew)))
                                (send self :num-points)
                                (standard-deviation xorig)
                                (standard-deviation znew))))
               
                     (progn
                      (send self :start-buffering)
                      (send self :slot-value 'current.corr r)
                      (send self :clear :draw nil)
                      (send self :x-axis t nil 5)
                      (send self :y-axis t nil 5)
                      (send self :add-points xorig znew :symbol 'dot4) 
                      (send self :title (format nil "r = ~,3f" 
                                          (/ (round (* (^ 10 5) empcorr))
                                             (^ 10 5))))
                      (if (send self :slot-value 'firstregp) 
                          (progn
                           (send self :add-lines (list -4 4) (* r (list -4 4)))
                           (let ((drawpos (send self :real-to-canvas 2.3 
                                                      (- (* r 2.3) 0.45))))
                             (send self :draw-string "Y on X" 
                                   (first drawpos) (second drawpos)))))
                      (if (send self :slot-value 'secondregp)
                          (progn
                            (send self :add-lines (* r (list -4 4)) (list -4 4))
                           (let ((drawpos (send self :real-to-canvas  
                                                      (- (* r 2.3) 0.85) 2.3)))
                             (send self :draw-string "X on Y" 
                                   (first drawpos) (second drawpos)))))
                      (send self :buffer-to-screen)
                     ))))
           graph)))

(defun slider (whom fstring) (let ((sli (formatted-interval-slider-dialog 
                                 (list -1 1)
                                 :points 201
                                 :nice t
                                 :title "Current corr. value"
                                 :use-format fstring
                                 :action #'(lambda (x) 
                                             (send whom :set-corr x))
                                 )))
                      (progn
                       (send sli :value 0)
                        sli)))

(defun slider.empty (fstring) (let ((sli (formatted-interval-slider-dialog 
                                 (list -1 1)
                                 :points 201
                                 :nice t
                                 :title "Target corr. value"
                                 :use-format fstring
                                 :action #'(lambda (x) 
                                             ())
                                 )))
                      (progn
                       (send sli :value 0)
                       (send sli :remove)
                        sli)))


(setf gr (corr-graph-create 1000))
(setf sl (slider gr "~,2f"))
(setf sl.to (slider.empty "~,2f"))


(defun movie.prim (from to steps)
    (dolist (i (rseq from to (+ steps 1))) 
               (send sl :value i)
))

(defun movie.up ()
  (let ((curr.value (send sl :value)))
  (movie.prim curr.value 1 (ceiling (* 20 (- 1 curr.value))))))


(defun movie.down ()
  (let ((curr.value (send sl :value)))
  (movie.prim curr.value -1 (ceiling (* 20 (+ 1 curr.value))))))

(defun movie.to (to.corr)
  (let ((curr.value (send sl :value)))
    (if (not (< (abs (- curr.value to.corr)) 0.05))  
  (movie.prim curr.value to.corr (ceiling (* 20 (- to.corr curr.value)))))))


(defun movie.round ()
  (let ((curr.value (send sl :value)))
    (progn (movie.up)
           (movie.down)
           (movie.to curr.value))))

(setf mmenu (send menu-proto :new "Display"))
(setf movie-item-up (send menu-item-proto :new "Run movie up"
                       :action (lambda () (movie.up))))
(setf movie-item-down (send menu-item-proto :new "Run movie down"
                       :action (lambda () (movie.down))))
(setf movie-item-go (send menu-item-proto :new "Go to target"
                       :action (lambda () (movie.to (send sl.to :value)))))

(setf movie-item-round (send menu-item-proto :new "Run movie round"
                       :action (lambda () (movie.round))))
(setf movie-item-to (send menu-item-proto :new "Set movie target"
                       :action (lambda () 
                                 (progn (send sl.to :location 
                                              (first (send sl :location)) 
                                              (+ (second (send sl :location))
                                                 (second (send sl :frame-size))
                                                 10))
                                  (send sl.to :show-window)
                                        (send mmenu :delete-items movie-item-to)
                                        (send mmenu :append-items movie-item-to-remove)
                                        (send mmenu :append-items movie-item-go)
                                        ))))
                                        
(setf movie-item-to-remove (send menu-item-proto :new "Remove movie target"
                          :action (lambda () (send mmenu :delete-items movie-item-to-remove)
                                    (send mmenu :delete-items movie-item-go)
                                    (send sl.to :remove)
                                    (send mmenu :append-items movie-item-to))))


(setf first-reg-item (send menu-item-proto :new "Reg. Y on X"
                           :mark t
                           :action (lambda () (send gr :firstreg (not (send gr
                                                                          :firstreg))))))
(defmeth first-reg-item :update ()
  (send self :mark (if (send gr :firstreg) t nil)))

(setf second-reg-item (send menu-item-proto :new "Reg. X on Y"
                           :mark t
                           :action (lambda () (send gr :secondreg (not (send gr                                                                          :secondreg))))))
(defmeth second-reg-item :update ()
  (send self :mark (if (send gr :secondreg) t nil)))

(send mmenu :append-items first-reg-item second-reg-item
      movie-item-round movie-item-up movie-item-down movie-item-to)

(send mmenu :install)





-- 
Erich Neuwirth
Dept. of Statistics, OR, and Computer Science, Univ. of Vienna
Universitaetsstr. 5/9, A-1010 Vienna, Austria
email: neuwirth@smc.univie.ac.at  
phone: +43-1-4076355-130  fax: +43-1-4076355-88

a fool with a spreadsheet is still a fool             ---  Paul Abrahams

a fool without a spreadsheet is a fool without a tool ---  Richard Rasala

