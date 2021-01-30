;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;                                                                       ;;
;;;;  A subset of the pull-and-move.lsp program.  Shows the projection     ;;
;;;;  onto a line that the user may move around.  The projection is        ;;
;;;;  based on a specific loss function where the independant variable     ;;
;;;;  is normally distributed.  The regression line may also be inserted.  ;;
;;;;                                                                       ;;
;;;;  Question, comments to Jason Bond (jbond@laplace.stat.ucla.edu)       ;;
;;;;                                                                       ;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;







(defproto move-line-proto () () scatterplot-proto)


(defun move-line (x y &key (same nil))

 (setf zz (send graph-window-proto :new))
 (setf pp (send move-line-proto :new 2 :title "Pull My Line"))
 (send pp :add-points (list x y))
 (send pp :adjust-to-data)
   (let* (
          (nn (send pp :num-points))
          (xx (send pp :scaled-range 0))
          (yy (send pp :scaled-range 1))
          (k1 .00000000001)
         )
     (setf regtest nil)
     (setf pulltest nil)

     (setf reg-install (send menu-item-proto :new 
                      "Insert\\Remove Regression Line"
                     :action #'(lambda () (setf params (send pp :get-reg-type))
                             (cond (params  (send pp :reg-coefs (first params))
                                            (send pp :reg-ss (second params))
                                            (send zz :display-info-pull 
                                              pulltest regtest)
                                            (setf regtest t)
                                            (send pp :project-points))
                                   (t  (setf regtest nil)
                                       (send pp :project-points))))))


     (setf ppsave-image (send menu-item-proto :new "Save to File"
                       :action #'(lambda () 
                      (send pp :project-points)
                      (send pp :ask-save-image))))

     (setf zzsave-image (send menu-item-proto :new "Save to File"
                       :action #'(lambda () (send zz :ask-save-image))))

     (setf which-type (send menu-item-proto :new "Display Which Type?"
                       :action #'(lambda () (get-type)
                       (send pp :project-points))))

     (setf ppmenu (send menu-proto :new "Point Pull"))
     (send ppmenu :append-items reg-install which-type ppsave-image)
     
     (setf zzmenu (send menu-proto :new "Info Display"))
     (send zzmenu :append-items zzsave-image)

     (send pp :menu ppmenu)
     (send zz :menu zzmenu)
     (send pp :draw-mode 'xor)

     (send pp :add-slot 'reg-line)
     (send pp :add-slot 'reg-rss 0)
     (send pp :add-slot 'your-coefs)
     (send pp :add-slot 'your-rss 0)
     (send pp :add-slot 'regk)
     (send pp :add-slot 'k-val)
     (send pp :add-slot 'xx xx)
     (send pp :add-slot 'yy yy)
     (send pp :add-mouse-mode 'pulling 
              :title "Pulling Mode" 
              :click :where-am-i 
              :cursor 'hand)

     (send zz :size 400 150)
     (send zz :location 500 90)

     (send pp :begin-characteristics)
     (send pp :mouse-mode 'pulling)
   )
)


(defmeth move-line-proto :where-am-i (x y m1 m2)
(send self :while-button-down

      #'(lambda (x y)
          (setf pulltest t)
          (send reg-install :enabled t)
          (send which-type :enabled t)
          (let* (
                 (xx (send self :xx))
                 (yy (send self :yy))
                 (cc (send self :canvas-to-scaled x y))
                 (bg (list (first xx) (first yy)))
                 (ed (list (second xx) (second yy)))
                 (d1 (dist bg cc))
                 (d2 (dist ed cc))
                 )
            (cond ((< d1 d2) (setf (first xx) (first cc))
                   (setf (first yy) (second cc)))
              (t (setf (second xx) (first cc))
                 (setf (second yy) (second cc))))

          (send self :project-points))
 ))
)


(defmeth move-line-proto :project-points ()
(let* (
       (xx (send self :xx))
       (yy (send self :yy))
       (n (send self :num-points))
       (x (send self :point-coordinate 0 (iseq n)))
       (y (send self :point-coordinate 1 (iseq n)))
       (coefs (send self :line-calculate xx yy))
       (k (send self :k-value))
       (params (send self :calc-params k nil))
       (ss (elt params 1))
       (xproj (elt params 2))
       (yproj (elt params 3))
      )

  (send self :clear-lines)
  (send self :adjust-to-data)

  (if regtest (send self :draw-regline))

  (send self :add-lines xx yy)
  (send self :your-rss ss)
  (dotimes (i n) 
    (send self :add-lines (list (elt x i) (elt xproj i)) 
                          (list (elt y i) (elt yproj i))
    )
  )

 (send zz :display-info-pull t regtest)
))



(defmeth move-line-proto :set-regression-characteristics ()
  (let* (
         (params (send self :calc-params .00000000001 t))
         (coefs (first params))
         (rss (second params))
        )
    (setf pulltest nil)
    (setf regtest t)
    (send self :clear-lines :draw nil)
    (send self :abline (select coefs 0) (select coefs 1))
    (send self :reg-coefs coefs)
    (send self :reg-ss rss)
    (send zz :display-info-pull nil t)
  )
)

(defmeth move-line-proto :begin-characteristics ()
  (let* (
         (params (send self :calc-params .00000000001 t))
         (coefs (first params))
         (rss (second params))
        )
    (setf regtest nil)
    (send self :clear-lines :draw nil)

    (send self :abline (select coefs 0) (select coefs 1))
    (send self :your-coefs coefs)
    (send self :your-rss rss)
    (send self :reg-coefs coefs)
    (send self :reg-ss rss)
    (send self :k-value .00000000001)
    (send self :reg-k .00000000001)
    (send zz :display-info-pull nil nil)
  )
)


(defmeth move-line-proto :line-calculate (xx yy)
  (let* (
         (m (/ (- (second yy) (first yy)) (- (second xx) (first xx))))
         (a (- (first yy) (* m (first xx))))
        )
   (send self :your-coefs (list a m))
   (list a m)
  )
)


(defmeth graph-window-proto :display-info-pull (test regtest)
     (send self :erase-window)
     (send pp :frame-rect 230 320 167 35)

    (cond (regtest

    (setf type
              (cond ((< (send pp :reg-k) .1) "Vertical")
                    ((< (send pp :reg-k) .9) "Orthogonal")
                    (t "Horizontal")))
           
     (send self :draw-string (format nil "Regression Projection: ~a" type) 10 80)
     (send self :draw-string (format nil
                                      "The Least Squares Line: ~1,4f + ~1,4fx"
                             (first (send pp :reg-coefs))
                             (second (send pp :reg-coefs))) 10 100)
     (send self :draw-string (format nil "The Least Squares RSS: ~1,4f"
                             (send pp :reg-ss)) 10 125)
     (send pp :draw-string (format nil "Reg Line:")
                          240 350)
     (send pp :draw-string (format nil " _____") 338 346)
     (send self :draw-string (format nil 
                           "_________________________________________")
                             10 65)))

     (cond (test
      (setf type
               (cond ((< (send pp :k-value) .1) "Vertical")
                     ((< (send pp :k-value) .9) "Orthogonal")
                     (t "Horizontal")))
            
      (send self :draw-string (format nil "Projection Displayed: ~a" type) 10 15)
      (send pp :draw-string (format nil "Pull The Line Where you Wish") 70 15)
      (send pp :draw-string (format nil "Your line:  ") 240 335)
      (send pp :draw-line 347 330 390 330)
      (send self :draw-string 
                  (format nil "Your Line Equation:  ~1,4f + ~1,4fx"
                          (first (send pp :your-coefs))
                          (second (send pp :your-coefs))) 10 35)

      (send self :draw-string (format nil "Your RSS:  ~1,4f"
                          (send pp :your-rss)) 10 57)))
)





(defmeth move-line-proto :calc-params (k test)
  (let* (
         (n (send self :num-points))
         (x (send self :point-coordinate 0 (iseq n)))
         (y (send self :point-coordinate 1 (iseq n)))
         (mux (mean x))
         (muy (mean y))
         (t-val (/ (- 1 k) k))
         (covmat (covariance-matrix x y))

         (beta (if test (/ (+ (aref covmat 1 1) 
                              (- 0 (* (aref covmat 0 0) t-val))
                              (^ (+ (^ (- (* t-val (aref covmat 0 0))
                                          (aref covmat 1 1)) 2)
                                    (* 4 t-val (^ (aref covmat 0 1) 2)))
                               .5))
                           (* 2 (aref covmat 0 1)))
                      
                        (second (send self :your-coefs))))

                  
           (alpha (if test (- muy (* beta mux)) 
                           (first (send self :your-coefs))))

         (ss (find-ss alpha beta x y t-val covmat n test))          
         (params (find-proj alpha beta x y n k))
              
       )
  (list (list alpha beta) ss (first params) (second params))
 )
)



(defun find-ss (a b x y t-val covmat n test)
   (let* (
          (fa (/ (- n 1) n))
          (ss 
              (if test (/ (+ (* fa (aref covmat 1 1)) 
                             (- 0 (* 2 b (aref covmat 0 1) fa))
                             (* (^ b 2) (aref covmat 0 0) fa))
                       (+ t-val (^ b 2)))
                       
                       (/ (mean (^ (- y a (* b x)) 2)) 
                          (+ t-val (^ b 2)))))
         )
     ss
   )
)

(defun find-proj (alpha beta x y n k)
 (let* (
        (xproj (list ))
        (yproj (list ))
        (tval (/ (- 1 k) k))
       ) 
   (dotimes (i n)
     (let* (
            (nume (+ (* x tval) (* beta (- y alpha))))
            (denom (+ tval (^ beta 2)))
            (xki (/ nume denom))
            (yki (+ alpha (* beta xki)))
           )
        (setf xproj (append xproj xki))
        (setf yproj (append yproj yki))
     )
     
   )
 (list xproj yproj)
 )
)


(defmeth move-line-proto :draw-regline ()
    (let* (
          (i (iseq 0 (- (send self :num-points) 1)))
          (x (send self :point-coordinate 0 i))
          (coefs (send self :reg-coefs))
          )
      (send self :abline (first coefs) (second coefs))
    )
)




(defmeth move-line-proto :get-reg-type ()
  (let* (
         (label (send text-item-proto :new "Minimize Which Distances?"))
         (pick (send choice-item-proto :new (list "Vertical Distances"
                      "Horizontal Distances" "Orthogonal Distances"
                      "Clear Regression Lines") :value 0))
         (ok   (send modal-button-proto :new "OK" 
                         :action #'(lambda () (setf p (send pick :value)))))
         (pick-dialog (send modal-dialog-proto :new 
                      (list label pick ok)))
        )
    
         (send pick-dialog :modal-dialog)

         (setf params (case p 
                             (0 (send self :reg-k .00000000001)
                                (send self :calc-params .00000000001 t))
                             (1 (send self :reg-k .9999999999)
                                (send self :calc-params .99999999999 t))
                             (2 (send self :reg-k .5)
                                (send self :calc-params .5 t))))

  params
  )
)


(defun dist (x y)
(sqrt (sum (^ (- x y) 2))))




(defun get-type ()
  (let* (
         (label (send text-item-proto :new "Which Type of distance?"))
         (pick (send choice-item-proto :new (list "Vertical Distances"
                      "Horizontal Distances" "Orthogonal Distances") :value 0))
         (ok   (send modal-button-proto :new "OK" 
                         :action #'(lambda () (setf val (send pick :value)))))
         (pick-dialog (send modal-dialog-proto :new (list label pick ok))) 
        )

         (send pick-dialog :modal-dialog)

         (send pp :k-value (case val 
                                 (0 .00000000001) (1 .99999999) (2 .5)))
  )
)




(defmeth move-line-proto :reg-coefs (&optional (val nil set))

(if set (setf (slot-value 'reg-line) val))

(slot-value 'reg-line))



(defmeth move-line-proto :reg-ss (&optional (val nil set))

(if set (setf (slot-value 'reg-rss) val))

(slot-value 'reg-rss))



(defmeth move-line-proto :your-coefs (&optional (val nil set))

(if set (setf (slot-value 'your-coefs) val))

(slot-value 'your-coefs))

 

(defmeth move-line-proto :your-rss (&optional (val nil set))

(if set (setf (slot-value 'your-rss) val))

(slot-value 'your-rss))



(defmeth move-line-proto :xx (&optional (val nil set))

(if set (setf (slot-value 'xx) val))

(slot-value 'xx))



(defmeth move-line-proto :yy (&optional (val nil set))

(if set (setf (slot-value 'yy) val))

(slot-value 'yy))



(defmeth move-line-proto :type (&optional (val nil set))

(if set (setf (slot-value 'type) val))

(slot-value 'type))


(defmeth move-line-proto :k-value (&optional (val nil set))

(if set (setf (slot-value 'k-val) val))

(slot-value 'k-val))


(defmeth move-line-proto :reg-k (&optional (val nil set))

(if set (setf (slot-value 'regk) val))

(slot-value 'regk))

#|
(defmeth graph-proto :close ()
 (exit)
)

(defmeth dialog-proto :close ()
  (exit)
)


(defun read-files ()
  (let* (
         (f (open "/u/quetelet/m2/www/httpd/htdocs/textbook/lisp/dirfile.lsp"))
         (file-names (list ))
        )
    (setf file-names
     (loop
       (let* (
              (filei (read f nil))
             )
         (if filei (setf file-names (append file-names (list (string filei))))
                   (return file-names))
       )
     )
    )
  (close f)
  (mapcar #'string-downcase file-names)
  )
)


(defun read-dialog ()
  (let* (
         (file-names (read-files))
         (title (send text-item-proto :new "Pick a Dataset"))
         (list-item (send list-item-proto :new file-names
                        :action #'(lambda (x)
               (setf data (read-data-columns (concatenate 'string
                           "/u/quetelet/m2/www/httpd/htdocs/textbook/lisp/"
                              (select file-names
                                 (send list-item :selection)))))
               (cond ((= 2 (length data)) 
                     (move-line (first data) (second data))
                     (send reader :hide-window))))))
        )
    (setf reader (send dialog-proto :new (list (list title) (list list-item))))
  )
)
(read-dialog)
|#









