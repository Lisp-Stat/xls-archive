;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; This program showsthe effect of changing the correaltion between  ;;
;; two sequences.  The user can pick any correaltion in [-1,1] and   ;;
;; points are generated with the corresponding correlation.  The reg ;;
;; lines of x on y and y on x are also plotted.                      ;;
;;                                                                   ;;
;; Comments, questions to jbond@stat.ucla.edu                        ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;




(defproto show-correlation-proto () () scatterplot-proto)
(defun show-correlation (x-y &key (same nil))
 (setf pp (send show-correlation-proto :new 2 :title "Move a Point"))
 (send pp :add-points x-y)
 (send pp :adjust-to-data)
 (send pp :draw-mode 'xor)
 (send pp :add-slot 'coefs-y-on-x)
 (send pp :add-slot 'coefs-x-on-y)
 (send pp :add-slot 'corr)
 
 (let* (
       (slider-dialog (interval-slider-dialog '(-1 1) :title "Correlation"
                           :points 200 :action #'(lambda (k)
                         (let (
                               (x (send pp :point-coordinate 0
                                   (iseq (send pp :num-points))))
                              )
                           (send pp :correlation k)
                           (send pp :clear)
                           (send pp :add-points (make-points (length x) k))
                           (send pp :redraw-background)
                           (send pp :draw-params)))))
       )
 )
)


(defmeth show-correlation-proto :do-point-moving (x y a b)
  (let (
        (p (send self :drag-point x y :draw nil))
       )
    (if p (send self :draw-params))
  )
)

(defmeth show-correlation-proto :calc-params ()
 (let* (
        (ind (send self :points-showing))
        (n (length ind))
        (x (send self :point-coordinate 0 ind))
        (y (send self :point-coordinate 1 ind))
        (mux (mean x))
        (muy (mean y))
        (sx (standard-deviation x))
        (sy (standard-deviation y))
        (covar (/ (sum (* (- x mux) (- y muy))) (1- n)))
        (beta-y-on-x (/ covar (^ sx 2)))
        (alpha-y-on-x (- muy (* beta-y-on-x mux)))
        (beta-x-on-y (/ (^ sy 2) covar))
        (alpha-x-on-y (* (- (* (/ 1 beta-x-on-y) muy) mux) beta-x-on-y))
        (corr (/ covar (* sx sy)))
       )
   (send self :correlation corr)
   (send self :coefs-y-on-x (list alpha-y-on-x beta-y-on-x))
   (send self :coefs-x-on-y (list alpha-x-on-y beta-x-on-y))
   )
)


(defmeth show-correlation-proto :draw-params ()
  (let* (
         (ind (send self :points-showing))
         (x (send self :point-coordinate 0 ind))
        )
    (send self :calc-params)
    (send self :clear-lines :draw nil)
    (send self :redraw-background)
    (send self :abline (first (send self :coefs-y-on-x))
                       (second (send self :coefs-y-on-x)))
    (send self :abline (first (send self :coefs-x-on-y))
                       (second (send self :coefs-x-on-y)))
    (send self :draw-string (format nil "Correlation: ~1,4g" 
                       (send self :correlation)) 50 350)
    (apply #'send self :draw-string "Y on X" (send self :real-to-canvas 
                      (max x) (+ (first (send self :coefs-y-on-x))
                              (* (max x) (second (send self :coefs-y-on-x))))))
    (apply #'send self :draw-string "X on Y" (send self :real-to-canvas 
                    (max x) (+ (first (send self :coefs-x-on-y))
                            (* (max x) (second (send self :coefs-x-on-y))))))
  )
)


(defmeth show-correlation-proto :coefs-y-on-x (&optional (val nil set))
 (if set (setf (slot-value 'coefs-y-on-x) val))
 (slot-value 'coefs-y-on-x)
)

(defmeth show-correlation-proto :coefs-x-on-y (&optional (val nil set))
 (if set (setf (slot-value 'coefs-x-on-y) val))
 (slot-value 'coefs-x-on-y)
)


(defmeth show-correlation-proto :correlation (&optional (val nil set))
 (if set (setf (slot-value 'corr) val))
 (slot-value 'corr)
)


#|
(defmeth graph-proto :close ()
 (exit)
)

(defmeth dialog-proto :close ()
 (exit)
)

|#


(defun start-dialog ()
  (let* (
         (point-text (send text-item-proto :new "Number of Points:"))
         (get-point (send edit-text-item-proto :new "" :text-length 3))
         (ok (send modal-button-proto :new "Ok"
                     :action #'(lambda ()
            (let (
                  (p (read (make-string-input-stream (send get-point :text))
                                                      nil))
                  )
            (show-correlation (make-points p .5))))))
          (info-dialog (send modal-dialog-proto :new
                              (list (list point-text get-point) ok)))
                                                           
         )
    (send info-dialog :modal-dialog)
  )
)


(defun make-points (num corr)
   (let* (
          (z1 (normal-rand num))
          (z2 (normal-rand num))
          (x (/ (- z1 (mean z1)) (standard-deviation z1)))
          (y (/ (- z2 (mean z2)) (standard-deviation z2)))
          (covxy (/ (sum (* (- x (mean x)) (- y (mean y))))
                      (1- num)))
          (c2 (^ corr 2))
          (a (+ 1 (^ covxy 2) (- (* 2 covxy))
                  (- (* 2 c2)) (* 2 c2 covxy)))
          (b (* 2 (+ (- (^ covxy 2)) covxy c2 (- (* c2 covxy)))))
          (c (- (^ covxy 2) c2))
          (coef (/ (+ (- b) (sqrt (- (^ b 2) (* 4 a c)))) (* 2 a)))
         )
     
     (if (> corr 0)
            (list x (+ (* coef x) (* (- 1 coef) y)))
            (list (- x) (+ (* coef x) (* (- 1 coef) y))))
   )
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
         (main-window (send list-item-proto :new file-names
                        :action #'(lambda (x)
               (setf data (read-data-columns
                           (concatenate 'string 
                           "/u/quetelet/m2/www/httpd/htdocs/textbook/lisp/"
                           (select file-names (send main-window :selection)))))
               (show-correlation (first data) (second data))
               (send reader :hide-window))))
        )
    (setf reader (send dialog-proto :new (list main-window)))
  )
)

(start-dialog)



