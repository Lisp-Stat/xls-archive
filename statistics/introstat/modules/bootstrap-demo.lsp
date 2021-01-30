
(defun jackknife1 (data func)
(let* (
       (n1 (length data))
       (k1 (iseq n1))
       (y (repeat nil n1))
      )
  (dotimes (i n1 y)
          (setf (elt y i) (funcall func (select data (remove i k1)))))))

(defun bcanon1 (x nboot func alpha test)
(let* (
      (f (funcall func x))
      (g (repeat nil nboot))
      (nx (length x))
      (z (normal-quant alpha))
     )
  (dotimes (i nboot)
    (let (
          (samp (if test (sample x nx t) (* f (uniform-rand nx))))
         )
      (setf (elt g i) (funcall func samp))
    )
  )
  (let* (
         (tmp (length (which (< g f))))
         (nq (cond ((< tmp 0.0000000001) .0000000001)
                   ((> tmp .9999999999) .9999999999)
                   (t tmp)))
         (z0 (normal-quant (/ nq nboot)))
         (th (jackknife1 x func))
         (tm (mean th))
         (s1 (sum (^ (- tm th) 3)))
         (s2 (sum (^ (- tm th) 2)))
         (aa (/ s1 (* 6 (^ s2 1.5))))
         (a1 (normal-cdf (+ z0 (/ (+ z0 z) (1- (* aa (+ z0 z)))))))
         (a2 (normal-cdf (+ z0 (/ (- z0 z) (1- (* aa (- z0 z)))))))
        )
 (list f g (quantile g a2) (quantile g a1) 
           (quantile g (- 1 alpha)) (quantile g alpha))
)))

(defun jackknife2 (data func)
(let* (
       (n1 (length (first data)))
       (n2 (length (second data)))
       (k1 (iseq n1))
       (k2 (iseq n2))
       (xlist (repeat nil n1))
       (ylist (repeat nil n2))
       (nume nil)
       (denom nil)
      )
  (dotimes (i n1)
          (setf (elt xlist i) (funcall func 
                   (list (select (first data) (remove i k1)) (second data)))))
  (dotimes (i n2)
           (setf (elt ylist i) (funcall func 
                   (list (first data) (select (second data) (remove i k2))))))
  (setf xlist (- xlist (mean xlist)))
  (setf ylist (- ylist (mean ylist)))
  (setf nume (+ (* (sum (^ xlist 3)) (^ (/ (1- n1) n1) 3))
                (* (sum (^ ylist 3)) (^ (/ (1- n2) n2) 3))))
  (setf denom (* 6 (^ (+ (* (sum (^ xlist 2)) (^ (/ (1- n1) n1) 2))
                         (* (sum (^ ylist 2)) (^ (/ (1- n2) n2) 2))) 1.5)))
  (/ nume denom)
))


(defun bcanon2 (x y nboot func alpha)
(let* (
      (meanxy (mean (combine (list x y))))
      (meanx (mean x))
      (meany (mean y))
      (newx (+ (- x meanx) meanxy))
      (newy (+ (- y meany) meanxy))
      (f (funcall func (list x y)))
      (g (repeat nil nboot))
      (nx (length x))
      (ny (length y))
      (z (normal-quant alpha))
     )
  (dotimes (i nboot)
    (let (
          (xsamp (sample newx nx t))
          (ysamp (sample newy ny t))
         )
      (setf (elt g i) (funcall func (list xsamp ysamp)))
    )
  )
  (let* (
         (tmp (length (which (< g f))))
         (nq (cond ((< tmp 0.0000000001) .0000000001)
                   ((> tmp .9999999999) .9999999999)
                   (t tmp)))
         (z0 (normal-quant (/ nq nboot)))
         (aa (jackknife2 (list x y) func))
         (a1 (normal-cdf (+ z0 (/ (+ z0 z) (1- (* aa (+ z0 z)))))))
         (a2 (normal-cdf (+ z0 (/ (- z0 z) (1- (* aa (- z0 z)))))))
        )
 (list f g (quantile g a2) (quantile g a1) 
           (quantile g (- 1 alpha)) (quantile g alpha))
)))


(defun uneq-sig-t (data)
  (let* (
         (m1 (mean (first data)))
         (m2 (mean (second data)))
         (sd1 (standard-deviation (first data)))
         (sd2 (standard-deviation (second data)))
         (n1 (length (first data)))
         (n2 (length (second data)))
         (numerator (/ (- m1 m2) (sqrt (+ (/ n1) (/ n2)))))
         (denominator (sqrt (+ (/ (^ sd1 2) n1) (/ (^ sd2 2) n2))))
        )
    (/ numerator denominator)
  )
)


(defun t-diff (data)
  (let* (
         (m1 (mean (first data)))
         (m2 (mean (second data)))
         (sd1 (standard-deviation (first data)))
         (sd2 (standard-deviation (second data)))
         (n1 (length (first data)))
         (n2 (length (second data)))
        )
    (- (/ m1 (/ sd1 (sqrt n1))) (/ m2 (/ sd2 (sqrt n2))))
  )
)


(defun start ()
 (let* (
        (typeask (send text-item-proto :new "Which Type of Example?"))
        (typeget (send choice-item-proto :new 
                       (list "Well Behaved" "Not So Well Behaved")))
        (ok (send button-item-proto :new "Start the Example"
             :action #'(lambda ()
                 (let (
                       (example (send typeget :value))
                      )
                   (if (= example 0) (good-dialog) (bad-dialog))
                 ))))
       )
    (send dialog-proto :new (list (list typeask) (list typeget) ok))
 )
)
     

(defun bad-dialog ()
 (setf done nil)
 (setf data-hist1 (send bootproto :new 1 :size (list 400 400) 
                               :title "Non-parametric Bootstrap Distribution"))
 (setf data-hist2 (send bootproto :new 1 :size (list 400 400) 
                                  :location (list 700 50)
                               :title "Parametric Bootstrap Distribution"))
 (let* (
        (do-it (send button-item-proto :new "Bootstrap it!"
               :action #'(lambda ()
                           (let* (
                                 (data-1 (+ 10 (uniform-rand 100)))
                                 (bootdata (bcanon1 data-1 1000 'max .025 t))
                                 (bootdata2 (bcanon1 data-1 1000 'max .025 nil))
                                 )
                              (send data-hist2 :draw-new-sample bootdata2)
                              (send data-hist1 :draw-new-sample bootdata)
                              (setf done t)
                              (send data-hist1 :n1 (length data-1))))))
       )
     (send dialog-proto :new (list do-it))
 )
)

(defproto bootproto '(mu1 mu2 sd1 sd2 n1 n2 nboot alpha a1 a2 ralpha1 ralpha2 
                      old obs-t obsmu1 obsmu2 fun-value funct)
                      () histogram-proto)

(defun good-dialog ()
 (setf data-hist (send bootproto :new 1 :size (list 400 400)
                                 :title "Bootstrap Distribution"))
 (send data-hist :mu1 0)
 (send data-hist :mu2 0)
 (send data-hist :sd1 1)
 (send data-hist :sd2 1)
 (send data-hist :n1 20)
 (send data-hist :n2 20)
 (send data-hist :nboot 1000)
 (send data-hist :alpha .05)
 (send data-hist :fun-value 0)
 (send data-hist :funct (list "uneq-sig-t" "t-diff"))
 (def done nil)
  (let* (
         (mu1 (send text-item-proto :new "Population 1 Mean: "))
         (mu2 (send text-item-proto :new "Population 2 Mean: "))
         (sd1 (send text-item-proto :new "Population 1 Sd: "))
         (sd2 (send text-item-proto :new "Population 2 Sd: "))
         (num1 (send text-item-proto :new "Sample Size 1: "))
         (num2 (send text-item-proto :new "Sample Size 2: "))
         (nboottell (send text-item-proto :new "Number of Bootstraps: "))
         (alphatell (send text-item-proto :new "Alpha: "))
         (funtell (send text-item-proto :new "Function: "))
         (mu1val (send text-item-proto :new (format nil "~a" 
                                (send data-hist :mu1))))
         (mu2val (send text-item-proto :new (format nil "~a"
                                (send data-hist :mu2))))
         (sd1val (send text-item-proto :new (format nil "~a"
                                (send data-hist :sd1))))
         (sd2val (send text-item-proto :new (format nil "~a"
                                (send data-hist :sd2))))
         (num1samp (send text-item-proto :new (format nil "~a" 
                                (send data-hist :n1))))
         (num2samp (send text-item-proto :new (format nil "~a"
                                (send data-hist :n2))))
         (nboot (send text-item-proto :new (format nil "~a"
                                (send data-hist :nboot))))
         (alp (send text-item-proto :new (format nil "~a"
                                (send data-hist :alpha))))
         (fun (send text-item-proto :new (select (send data-hist :funct)
                             (send data-hist :fun-value))))
         (advbutton (send button-item-proto :new "Advanced Options"
                     :action #'(lambda () 
                     (let* (
                            (val (send data-hist :adv-dialog))
                           )
                      (cond ((elt val 7)
                        (send mu1val :text (format nil "~a" (elt val 0)))
                        (send mu2val :text (format nil "~a" (elt val 1)))
                        (send sd1val :text (format nil "~a" (elt val 2)))
                        (send sd2val :text (format nil "~a" (elt val 3)))
                        (send num1samp :text (format nil "~a" (elt val 4)))
                        (send num2samp :text (format nil "~a" (elt val 5)))
                        (send nboot :text (format nil "~a" (elt val 6)))
                        (send alp :text (format nil "~a" (elt val 7)))
                        (send fun :text (select (send data-hist :funct)
                                        (send data-hist :fun-value)))))))))
         (doitbutton (send button-item-proto :new "Start Bootstrap"
             :action #'(lambda ()
              (let (
                    (mu1 (read (make-string-input-stream 
                                   (send mu1val :text)) nil))
                    (mu2 (read (make-string-input-stream 
                                   (send mu2val :text)) nil))
                    (sd1 (read (make-string-input-stream
                                   (send sd1val :text)) nil))
                    (sd2 (read (make-string-input-stream
                                   (send sd2val :text)) nil))
                    (num1 (read (make-string-input-stream
                                   (send num1samp :text)) nil))
                    (num2 (read (make-string-input-stream
                                   (send num2samp :text)) nil))
                    (nb (read (make-string-input-stream
                                  (send nboot :text)) nil))
                    (alpha (read (make-string-input-stream
                                  (send alp :text)) nil))
                    (funct (intern (string-upcase (select 
                             (send data-hist :funct)
                              (send data-hist :fun-value)))))
                   )
                (let* (
                       (data-1 (+ (* sd1 (normal-rand num1)) mu1))
                       (data-2 (+ (* sd2 (normal-rand num2)) mu2))
                       (obsm1 (send data-hist :obsmu1 (mean data-1)))
                       (obsm2 (send data-hist :obsmu2 (mean data-2)))
                       (bootdata (bcanon2 data-1 data-2 nb funct (/ alpha 2)))
                      )
                   (send data-hist :old t)
                   (send data-hist :draw-new-sample bootdata)
                   (def done t)
                 )
              ))))
         )
    (send dialog-proto :new (list (list mu1 mu1val) (list mu2 mu2val)
                                  (list sd1 sd1val) (list sd2 sd2val)
                                  (list num1 num1samp) (list num2 num2samp)
                                  (list nboottell nboot) (list alphatell alp)
                                  (list funtell fun)
                                  (list advbutton doitbutton)))
  )
)


(defmeth bootproto :adv-dialog ()
  (let* (
         (mu1 (send text-item-proto :new "Pop 1 Mean: "))
         (mu2 (send text-item-proto :new "Pop 2 Mean: "))
         (sd1 (send text-item-proto :new "Pop 1 Sd: "))
         (sd2 (send text-item-proto :new "Pop 2 Sd: "))
         (n1 (send text-item-proto :new "Sample Size 1: "))
         (n2 (send text-item-proto :new "Sample Size 2: "))
         (nb (send text-item-proto :new "Number of Bootstraps: "))
         (alpha (send text-item-proto :new "Alpha Value: "))
         (funct (send text-item-proto :new "Function: "))

         (m1 (send edit-text-item-proto :new 
               (format nil "~a" (send self :mu1)) :text-length 3))
         (m2 (send edit-text-item-proto :new 
               (format nil "~a" (send self :mu2)) :text-length 3))
         (s1 (send edit-text-item-proto :new
               (format nil "~a" (send self :sd1)) :text-length 3))
         (s2 (send edit-text-item-proto :new
               (format nil "~a" (send self :sd2)) :text-length 3))
 
         (n-one (send edit-text-item-proto :new 
                (format nil "~a" (send self :n1)) :text-length 3))
         (n-two (send edit-text-item-proto :new 
                (format nil "~a" (send self :n2)) :text-length 3))
         (nboot (send edit-text-item-proto :new 
                (format nil "~a" (send self :nboot)) :text-length 3))
         (alp (send edit-text-item-proto :new 
                (format nil "~a" (send self :alpha)) :text-length 3))
         (fun (send choice-item-proto :new (send self :funct)))

         (ok (send modal-button-proto :new "Done" 
              :action #'(lambda ()
                (let* (
                       (mu1 (read (make-string-input-stream 
                                   (send m1 :text)) nil))
                       (mu2 (read (make-string-input-stream 
                                   (send m2 :text)) nil))
                       (sd1 (read (make-string-input-stream
                                   (send s1 :text)) nil))
                       (sd2 (read (make-string-input-stream
                                   (send s2 :text)) nil))
                       (n1 (read (make-string-input-stream 
                                   (send n-one :text)) nil))
                       (n2 (read (make-string-input-stream 
                                   (send n-two :text)) nil))
                       (nb (read (make-string-input-stream 
                                   (send nboot :text)) nil))
                       (alpha (read (make-string-input-stream 
                                   (send alp :text)) nil))
                       (funct (send fun :value))
                       (good-data (every 'numberp 
                                    (list mu1 mu2 sd1 sd2 n1 n2 nb alpha)))
                       
                      )
              (cond (good-data
                (send data-hist :mu1 mu1)
                (send data-hist :mu2 mu2)
                (send data-hist :sd1 sd1)
                (send data-hist :sd2 sd2)
                (send data-hist :n1 n1)
                (send data-hist :n2 n1)
                (send data-hist :nboot nb)
                (send data-hist :alpha alpha)
                (send data-hist :fun-value funct)))
                (list mu1 mu2 sd1 sd2 n1 n2 nb alpha good-data)))))

         (dialog (send modal-dialog-proto :new (list 
                   (list mu1 m1) (list mu2 m2) (list sd1 s1) (list sd2 s2)
                   (list n1 n-one) (list n2 n-two)
                   (list nb nboot) (list alpha alp) (list funct fun)
                   (list ok))))
        )
    (send fun :value (send data-hist :fun-value))
    (send dialog :modal-dialog)
 )
)

(defmeth bootproto :redraw ()
 (call-next-method)
(cond ((send self :old)  
 (let (
       (a1 (send self :a1))
       (a2 (send self :a2))
       (ralpha1 (send self :ralpha1))
       (ralpha2 (send self :ralpha2))
       (obs-t (send self :obs-t))
       (top (second (send self :range 1)))
      )
   (apply #'send self :draw-string (format nil "~3,3f" a1)
                      (send self :real-to-canvas a1 (* .8 top)))
   (apply #'send self :draw-string (format nil "~3,3f" a2)
                      (send self :real-to-canvas a2 (* .8 top)))

   (apply #'send self :draw-string (format nil "~3,3f" ralpha1)
                      (send self :real-to-canvas ralpha1 (* .6 top)))
   (apply #'send self :draw-string (format nil "~3,3f" ralpha2)
                      (send self :real-to-canvas ralpha2 (* .6 top)))

   (apply #'send self :draw-string "Obs"
                      (send self :real-to-canvas obs-t (* .9 top)))
   (send self :add-lines (list (list obs-t obs-t) (list 0 (* .9 top)))
                         :type 'dashed)

   (send self :add-lines (list (list a1 a1) (list 0 (* .8 top))))
   (send self :add-lines (list (list a2 a2) (list 0 (* .8 top))))
   (send self :add-lines (list (list ralpha1 ralpha1) (list 0 (* .6 top)))
                         :type 'dashed)
   (send self :add-lines (list (list ralpha2 ralpha2) (list 0 (* .6 top)))
                         :type 'dashed))))
)


(defmeth bootproto :draw-new-sample (bootdata)
 (let* (
        (obs-t (first bootdata))
        (boot-dist (second bootdata))
        (a2 (third bootdata))
        (a1 (fourth bootdata))
        (ralpha1 (elt bootdata 4))
        (ralpha2 (elt bootdata 5))
       )
   (cond (done 
         (send self :clear-points)
         (send self :clear-lines)))
   (send self :a1 a1)
   (send self :a2 a2)
   (send self :ralpha1 ralpha1)
   (send self :ralpha2 ralpha2)
   (send self :obs-t obs-t)
   (send self :add-points boot-dist)
   (send self :adjust-to-data)
   (send self :num-bins 29)
 )
)


(defmeth bootproto :mu1 (&optional (val nil set))
  (if set (setf (slot-value 'mu1) val)
   (slot-value 'mu1)))

(defmeth bootproto :mu2 (&optional (val nil set))
  (if set (setf (slot-value 'mu2) val)
   (slot-value 'mu2)))

(defmeth bootproto :sd1 (&optional (val nil set))
  (if set (setf (slot-value 'sd1) val)
   (slot-value 'sd1)))

(defmeth bootproto :sd2 (&optional (val nil set))
  (if set (setf (slot-value 'sd2) val)
   (slot-value 'sd2)))

(defmeth bootproto :n1 (&optional (val nil set))
  (if set (setf (slot-value 'n1) val)
   (slot-value 'n1)))

(defmeth bootproto :n2 (&optional (val nil set))
  (if set (setf (slot-value 'n2) val)
   (slot-value 'n2)))

(defmeth bootproto :nboot (&optional (val nil set))
  (if set (setf (slot-value 'nboot) val)
   (slot-value 'nboot)))

(defmeth bootproto :alpha (&optional (val nil set))
  (if set (setf (slot-value 'alpha) val)
   (slot-value 'alpha)))

(defmeth bootproto :a1 (&optional (val nil set))
  (if set (setf (slot-value 'a1) val)
   (slot-value 'a1)))

(defmeth bootproto :a2 (&optional (val nil set))
  (if set (setf (slot-value 'a2) val)
   (slot-value 'a2)))

(defmeth bootproto :ralpha1 (&optional (val nil set))
  (if set (setf (slot-value 'ralpha1) val)
   (slot-value 'ralpha1)))

(defmeth bootproto :ralpha2 (&optional (val nil set))
  (if set (setf (slot-value 'ralpha2) val)
   (slot-value 'ralpha2)))

(defmeth bootproto :old (&optional (val nil set))
  (if set (setf (slot-value 'old) val)
   (slot-value 'old)))

(defmeth bootproto :obs-t (&optional (val nil set))
  (if set (setf (slot-value 'obs-t) val)
   (slot-value 'obs-t)))

(defmeth bootproto :obsmu1 (&optional (val nil set))
  (if set (setf (slot-value 'obsmu1) val)
   (slot-value 'obsmu1)))

(defmeth bootproto :obsmu2 (&optional (val nil set))
  (if set (setf (slot-value 'obsmu2) val)
   (slot-value 'obsmu2)))

(defmeth bootproto :funct (&optional (val nil set))
  (if set (setf (slot-value 'funct) val)
   (slot-value 'funct)))

(defmeth bootproto :fun-value (&optional (val nil set))
  (if set (setf (slot-value 'fun-value) val)
   (slot-value 'fun-value)))


(start)


