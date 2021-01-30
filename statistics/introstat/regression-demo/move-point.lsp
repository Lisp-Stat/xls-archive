;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;                                                                       ;;
;;;;   A subset of the pull-and-move.lsp program.  Lifted from Tierney     ;;
;;;;   (1989).  The user can move around any point on the scatterplot.     ;;
;;;;   The regression-line of y on x may also be inserted.                 ;;
;;;;    Questions, comments to Jason Bond (jbond@laplace.stat.ucla.edu)    ;;
;;;;                                                                       ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;





(defproto move-point-proto () () scatterplot-proto)


(defun move-point (x y &key (same nil))

 (setf zz (send graph-window-proto :new))
 (setf pp (send move-point-proto :new 2 :title "Pull My Line"))
 (send pp :add-points (list x y))
 (send pp :adjust-to-data)
   (let* (
          (nn (send pp :num-points))
          (xx (send pp :scaled-range 0))
          (yy (send pp :scaled-range 1))
          (k1 .00000000001)
         )

     (setf reg-install (send menu-item-proto :new 
                      "Change Regression Line"
                     :action #'(lambda () (setf params (send pp :get-reg-type))
                             (cond (params  (send pp :reg-coefs (first params))
                                            (send pp :reg-ss (second params))
                                            (send pp :draw-regline)
                                            (send zz :display-info-pull)))))) 


     (setf ppsave-image (send menu-item-proto :new "Save to File"
                       :action #'(lambda () 
                      (send pp :ask-save-image))))

     (setf zzsave-image (send menu-item-proto :new "Save to File"
                       :action #'(lambda () (send zz :ask-save-image))))

     (setf ppmenu (send menu-proto :new "Point Pull"))

     (send ppmenu :append-items reg-install ppsave-image)
     
     (setf zzmenu (send menu-proto :new "Info Display"))
     (send zzmenu :append-items zzsave-image)

     (send pp :menu ppmenu)
     (send zz :menu zzmenu)
     (send pp :draw-mode 'xor)

     (send pp :add-slot 'reg-line)
     (send pp :add-slot 'reg-rss 0)
     (send pp :add-slot 'regk)
     (send pp :add-slot 'xx xx)
     (send pp :add-slot 'yy yy)

     (send pp :add-mouse-mode 'point-moving
              :title "Point Moving"
              :cursor 'finger
              :click :do-point-moving)

     (send zz :size 400 75)
     (send zz :location 500 90)

     (send pp :begin-characteristics)
     (send pp :mouse-mode 'point-moving)
   )
)


(defmeth move-point-proto :do-point-moving (x y a b)
  (setf pointmove t)
  (let ((p (send self :drag-point x y :draw nil)))
    (if p (send self :set-regression-characteristics)))
)


(defmeth move-point-proto :set-regression-characteristics ()
  (let* (
         (params (send self :calc-params (send pp :reg-k)))
         (coefs (first params))
         (rss (second params))
        )
    (send self :clear-lines :draw nil)
    (send self :abline (select coefs 0) (select coefs 1))
    (send self :reg-coefs coefs)
    (send self :reg-ss rss)
    (send zz :display-info-pull)
  )
)

(defmeth move-point-proto :begin-characteristics ()
  (let* (
         (params (send self :calc-params .00000000001))
         (coefs (first params))
         (rss (second params))
        )
    (send self :clear-lines :draw nil)

    (send self :abline (select coefs 0) (select coefs 1))
    (send self :reg-coefs coefs)
    (send self :reg-ss rss)
    (send self :reg-k .00000000001)
    (send zz :display-info-pull)
  )
)



(defmeth graph-window-proto :display-info-pull ()
     (send self :erase-window)
 (let (
       (type (cond ((< (send pp :reg-k) .1) "Vertical")
                   ((< (send pp :reg-k) .9) "Orthogonal")
                   (t "Horizontal")))
      )
     (send self :draw-string (format nil "Distances Minimized: ~a" type)
                             10 15)
     (send self :draw-string (format nil
                                  "The Least Squares Line: ~1,4f + ~1,4fx"
                             (first (send pp :reg-coefs))
                             (second (send pp :reg-coefs))) 10 35)
     (send self :draw-string (format nil "The Least Squares RSS: ~5,4f"
                             (send pp :reg-ss)) 10 55)
 )
)


(defmeth move-point-proto :calc-params (k)
  (let* (
         (n (send self :num-points))
         (x (send self :point-coordinate 0 (iseq n)))
         (y (send self :point-coordinate 1 (iseq n)))
         (mux (mean x))
         (muy (mean y))
         (t-val (/ (- 1 k) k))
         (covmat (covariance-matrix x y))

         (beta (/ (+ (aref covmat 1 1) 
                              (- 0 (* (aref covmat 0 0) t-val))
                              (^ (+ (^ (- (* t-val (aref covmat 0 0))
                                          (aref covmat 1 1)) 2)
                                    (* 4 t-val (^ (aref covmat 0 1) 2)))
                               .5))
                           (* 2 (aref covmat 0 1))))
                      

                  
           (alpha (- muy (* beta mux))) 

         (ss (find-ss alpha beta x y t-val covmat n))          
         (params (find-proj alpha beta x y n k))
              
       )
  (list (list alpha beta) ss (first params) (second params))
 )
)



(defun find-ss (a b x y t-val covmat n)
   (let* (
          (fa (/ (- n 1) n))
          (ss (/ (+ (* fa (aref covmat 1 1)) 
                             (- 0 (* 2 b (aref covmat 0 1) fa))
                             (* (^ b 2) (aref covmat 0 0) fa))
                       (+ t-val (^ b 2))))
                       
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


(defmeth move-point-proto :draw-regline ()
    (let* (
          (i (iseq 0 (- (send self :num-points) 1)))
          (x (send self :point-coordinate 0 i))
          (coefs (send self :reg-coefs))
          )
      (send self :clear-lines)
      (send self :abline (first coefs) (second coefs))
    )
)


(defmeth move-point-proto :get-reg-type ()
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

        (setf params
         (case p 
               (0 (send self :reg-k .00000000001)
                  (send self :calc-params .00000000001))
               (1 (send self :reg-k .9999999999)
                  (send self :calc-params .99999999999))
               (2 (send self :reg-k .5)
                  (send self :calc-params .5))))
params
        )
)


(defmeth move-point-proto :reg-coefs (&optional (val nil set))

(if set (setf (slot-value 'reg-line) val))

(slot-value 'reg-line))



(defmeth move-point-proto :reg-ss (&optional (val nil set))

(if set (setf (slot-value 'reg-rss) val))

(slot-value 'reg-rss))


(defmeth move-point-proto :xx (&optional (val nil set))

(if set (setf (slot-value 'xx) val))

(slot-value 'xx))



(defmeth move-point-proto :yy (&optional (val nil set))

(if set (setf (slot-value 'yy) val))

(slot-value 'yy))



(defmeth move-point-proto :type (&optional (val nil set))

(if set (setf (slot-value 'type) val))

(slot-value 'type))


(defmeth move-point-proto :reg-k (&optional (val nil set))

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
                     (move-point (first data) (second data))
                     (send reader :hide-window))))))
        )
    (setf reader (send dialog-proto :new (list (list title) (list list-item))))
  )
)
(read-dialog)















