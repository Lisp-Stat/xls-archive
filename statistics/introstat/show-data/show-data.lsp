(defun show-data (vars &key varnames case-labels)
  (let* (
         (plotask (send text-item-proto :new "Pick plot type"))
         (plotget (send choice-item-proto :new 
                    (list "Scatterplot" "Line Plot" "Boxplot" "Histogram"
                          "Spin Plot" "Scatterplot Matrix" "Quantile Plot"
                          "Dot-plot" "Stem-Leaf Plot")))
         (varnames (if varnames varnames 
                     (mapcar #'(lambda (x) (format nil "Var ~a" x))
                       (iseq (length vars)))))
         (case-labels (if case-labels case-labels
                        (mapcar #'(lambda (x) (format nil "~a" x))
                                      (iseq (length (first vars))))))

         (ok (send button-item-proto :new "Make Plot"
               :action #'(lambda ()
               (let* (
                      (val (send plotget :value))
                     )
                (case val (0 (let* (
                                    (varlist (get-vars vars varnames 2))
                                    (varvals (first varlist))
                                    (varname (second varlist))
                                    (con1 (= 1 (length (remove-duplicates
                                            (mapcar #'length varvals)))))
                                   )
                               (cond (con1 (setf point-plot (plot-points 
                                  (first varvals) (second varvals)
                                 :variable-labels varname 
                                :point-labels case-labels
                              :title (concatenate 'string (elt varname 0)
                               " vs "  (elt varname 1))))
                                 (terpri)
                                 (format t "Plot Name: point-plot")
                                 (terpri))
                                 (t (error "Sequences not of same length")))))

                          (1 (let* (
                                   (indepask (send text-item-proto :new
                                     "Independant Variable:"))
                                   (indepget (send choice-item-proto :new 
                                      varnames))
                                   (checks (mapcar #'(lambda (x)
                                     (send toggle-item-proto :new
                                      (format nil "~a" x))) varnames))
                                   (askvars (send text-item-proto :new
                                     "Pick Dependant Variables to Use:"))
                                   (ok (send modal-button-proto :new "Ok"
                                    :action #'(lambda ()
                                     (let* (
                                           (indepvar (select vars 
                                             (send indepget :value))) 
                                           (goodvars
                                              (which (mapcar #'(lambda (x)
                                                (send x :value)) checks)))
                                           (depvars (select vars goodvars))
                                           (varname (select varnames 
                                             (send indepget :value)))
                                           (con1 (= 1 (length (remove-duplicates
                                            (mapcar #'length (append depvars
                                             (list indepvar)))))))
                                           )
                                         (cond (con1
                                 (setf line-plot (plot-points (list
                                    (repeat indepvar (length goodvars))
                                    (combine depvars))
                                  :variable-labels (list varname nil)))
                                 (mapcar #'(lambda (x) (send line-plot 
                                     :add-lines indepvar x)) depvars)
                                 (terpri)
                                 (format t "Plot Name: line-plot")
                                 (terpri))
                                 (t (error "Sequences not of same length")))))))

                                   (dialog (send modal-dialog-proto
                                              :new (cons askvars
                                                  (append checks 
                                     (list indepask indepget) (list ok)))))
                                   )
                              (send dialog :modal-dialog)
                              ))

                          (2 (let* (
                                    (varlist (get-vars vars varnames 1))
                                    (varvals (first varlist))
                                    (varname (second varlist))
                                   )
                               (setf box-plot (boxplot varvals 
                                 :point-labels case-labels
                                 :title (first varname)))
                               (send box-plot :variable-label 0 (first varname))
                               (send box-plot :x-axis t t 0)
                               (terpri)
                               (format t "Plot Name: box-plot")
                               (terpri)))

                          (3 (let* (
                                    (varlist (get-vars vars varnames 1))
                                    (varvals (first varlist))
                                    (varname (second varlist))
                                   )
                               (setf hist-plot (histogram varvals
                              :title (first varname)
                              :point-labels case-labels
                              :variable-labels varname))
                               (terpri)
                               (format t "Plot Name: hist-plot")
                               (terpri)))

                          (4  (let* (
                                     (varlist (get-vars vars varnames 3))
                                     (varvals (first varlist))
                                     (varname (second varlist))
                                     (con1 (= 1 (length (remove-duplicates
                                            (mapcar #'length varvals)))))
                                    )
                                 (cond (con1 (setf spin-plot
                                      (spinit varvals
                                 :variable-labels varname
                                 :point-label case-labels
                              :title (concatenate 'string (elt varname 0)
                                  " vs " (elt varname 1) " vs "
                                         (elt varname 2))))
                                    (terpri)
                                    (format t "Plot Name: spin-plot")
                                    (terpri))
                                  (t (error "Sequences not of same length")))))

                          (5 (let* (
                                   (checks (mapcar #'(lambda (x)
                                     (send toggle-item-proto :new 
                                      (format nil "~a" x))) varnames))
                                   (askvars (send text-item-proto :new
                                     "Pick Variables to Use"))
                                   (ok (send modal-button-proto :new "Ok"
                                    :action #'(lambda () 
                                     (setf goodvars 
                                        (which (mapcar #'(lambda (x)
                                                (send x :value)) checks))))))
                                   (dialog (send modal-dialog-proto 
                                              :new (cons askvars
                                                  (append checks (list ok)))))
                                   )
                              (send dialog :modal-dialog)
                              (setf scatter-matrix (scatterplot-matrix 
                                           (select vars goodvars)
                             :point-labels case-labels
                             :variable-labels (select varnames goodvars)))
                              (terpri)
                              (format t "Plot Name: scatter-matrix")
                              (terpri)))

                          (6 (let* (
                                    (varlist (get-vars vars varnames 1))
                                    (varvals (first varlist))
                                    (varname (second varlist))
                                   )
                               (setf quant-plot (quantile-plot (first varvals)
                              :point-labels case-labels
                              :title (first varname)))
                               (terpri)
                               (format t "Plot Name: quant-plot")
                               (terpri)))

                          (7 (let* (
                                    (varlist (get-vars vars varnames 1))
                                    (varvals (first varlist))
                                    (varname (first (second varlist)))
                                    (con1 (= 1 (length (remove-duplicates
                                            (mapcar #'length varvals)))))
                                   )
                               (cond (con1 (setf dot-plot 
                                   (dotplot (first varvals)
                                   (if case-labels case-labels
                                       (iseq (length (first varvals))))
                                         :title varname))
                                  (terpri)
                                  (format t "Plot Name: dot-plot")
                                  (terpri)))))

                          (8 (let* (
                                    (varlist (get-vars vars varnames 1))
                                    (varvals (first varlist))
                                    (varname (second varlist))
                                   )
                                (let* (
                                       (stemask (send text-item-proto :new
                                                  "Stem Size: "))
                                       (leafask (send text-item-proto :new
                                                  "Leaf Size: "))
                                       (stemget (send choice-item-proto :new
                                                  (list "1" "2" "3" "4" "5")))
                                       (leafget (send choice-item-proto :new
                                                  (list "1" "2" "3")))
                                       (ok (send modal-button-proto :new "Ok"
                                            :action #'(lambda ()
                                        (setf params (list 
                                         (1+ (send stemget :value))
                                         (1+ (send leafget :value)))))))
                                       (get-data (send modal-dialog-proto :new
                                             (list (list stemask stemget
                                                 leafask leafget) (list ok))))
                                      )
                                  (send get-data :modal-dialog)
                                  (setf stem-plot
                                     (stem-and-leaf-plot (first varvals)
                                        :stem-size (first params)
                                        :leaf-size (second params)))
                                  (terpri)
                                  (format t "Plot Name: stem-plot")
                                  (terpri)))))))))
        )
     (send dialog-proto :new (list plotask plotget ok))
  )
)
   


(defun get-vars (vars varnames n)
  (let* (
         (var1ask (send text-item-proto :new "Var 1"))
         (var2ask (send text-item-proto :new "Var 2"))
         (var3ask (send text-item-proto :new "Var 3"))
         (var1get (send choice-item-proto :new varnames))
         (var2get (send choice-item-proto :new varnames))
         (var3get (send choice-item-proto :new varnames))
         (ok (send modal-button-proto :new "Ok"
              :action #'(lambda ()
                         (let* (
                                 (var1 (send var1get :value))
                                 (var2 (send var2get :value))
                                 (var3 (send var3get :value))
                               )
                           (mapcar #'(lambda (x) 
                            (select x (select (list var1 var2 var3) (iseq n))))
                                  (list vars varnames))))))
         (dialog (send modal-dialog-proto :new (list 
                    (select (list var1ask var2ask var3ask) (iseq n))
                    (select (list var1get var2get var3get) (iseq n)) ok)))
        )
    (send dialog :modal-dialog)
  )
)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                     ;;
;;   Code to produce a dotplot.  A linked name list function to link   ;;
;;   the labels to the points is also included.                        ;;
;;                                                                     ;;
;;   Comments, questions to Jason Bond (jbond@stat.ucla.edu)           ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



(defproto dotplot-proto '(data labels sort old) nil scatterplot-proto)


(defmeth dotplot-proto :data (&optional (val nil set))
 (if set (setf (slot-value 'data) val)
 (slot-value 'data))
)  

(defmeth dotplot-proto :labels (&optional (val nil set))
 (if set (setf (slot-value 'labels) val)
 (slot-value 'labels))
)  

(defmeth dotplot-proto :old (&optional (val nil set))
 (if set (setf (slot-value 'old) val)
 (slot-value 'old))
)  


(defmeth dotplot-proto :sort (&optional (val nil set))
 (if set (progn (setf (slot-value 'sort) val)
            (send self :redraw))
        (slot-value 'sort))
)


(defun dotplot (data labels &key (title "Dotplot") (sort nil)
                   (variable-labels (list "X" "Category")) (has-h-scroll nil))
                   
 "vars: DATA LABELS
  DATA is a list of numbers and LABELS is a list of strings, both of
  which are the same length.  Makes a dotplot of DATA using LABELS."
 (let* (
        (dp (send dotplot-proto :new 2 :variable-labels variable-labels
                                :title title))
        (num (if (listp (first data)) (length data) 1))
        (n (if (> num 1) (length (first data)) (length data)))
        (len nil)
        (maxdata (max (select (combine data) 
                       (which (mapcar #'numberp (combine data))))))
        (mindata (min (select (combine data)
                       (which (mapcar #'numberp (combine data))))))
        (nice-range (third (get-nice-range mindata maxdata 5)))
        (space (send dp :text-ascent))
        (yorigin (second (send dp :content-origin)))
        (ytop (- yorigin (* n space)))
       )
    (if (/= n (length (which (mapcar #'stringp labels))))
         (setf labels (mapcar #'(lambda (x)
               (if (stringp x) x (format nil "~a" x))) labels)))
    (send dp :old t)
    (send dp :data data)
    (send dp :labels labels)
    (setf len (floor (* 4 (max (mapcar #'length labels)))))

    (send dp :add-points (list (combine data) (repeat 0 (* num n))))
    (send dp :point-label (iseq (* num n)) (combine (repeat labels num)))
    (dotimes (i num)
      (send dp :point-symbol (iseq (* n i) (1- (* (1+ i) n)))
                             (elt (select *plot-symbols* (list 5 7 11 8)) i)))

    (send dp :x-axis t t nice-range)
    (send dp :y-axis t nil 0)

    (if (> n 20) (send dp :has-v-scroll (floor (* space n 1.7)))
                 (send dp :size 300 200))

    (send dp :range 0 (- mindata (* .1 (- maxdata mindata))) 
                      (+ maxdata (* .1 (- maxdata mindata))))
    (send dp :margin (floor (* (max (mapcar #'(lambda (x) 
                         (send dp :text-width x)) 
                       labels)) .8)) 0 0 0)
    (if has-h-scroll (send dp :has-h-scroll has-h-scroll))
    (send dp :linked t)
    (send dp :showing-labels t)
    dp
 )
)


(defun labels (labels)
 (let (
       (name-list (send name-list-proto :new 0
                        :title "Labels"))
       (n (length labels))
      )    
   (if (/= n (length (which (mapcar #'stringp labels))))
         (setf labels (mapcar #'(lambda (x)
               (if (stringp x) x (format nil "~a" x))) labels)))
   (send name-list :add-points n)
   (send name-list :point-label (iseq n) labels) 
   (send name-list :showing-labels t)
   (send name-list :linked t)
 )
)


(defmeth dotplot-proto :redraw ()
(call-next-method)
(send self :clear-lines)
(when (send self :old)
  (let* (
        (data (send self :data))
        (num (if (listp (first data)) (length data) 1))
        (n (if (> num 1) (length (first data)) (length data)))
        (labels (send self :labels))
        (space (floor (* 1.5 (send self :text-ascent))))
        (maxdata (max (select (combine data) 
                       (which (mapcar #'numberp (combine data))))))
        (mindata (min (select (combine data)
                       (which (mapcar #'numberp (combine data))))))
        (xorigin (first (send self :content-origin)))

        (yorigin (second (send self :content-origin)))
        (ycanvas (mapcar #'(lambda (i) (- yorigin (* i space))) 
                              (iseq n)))
        (yreal (mapcar #'second (mapcar #'(lambda (y)
                   (send self :canvas-to-real xorigin y))
                     (floor (- ycanvas (* .4 space))))))
        (yrealpoint (mapcar #'second (mapcar #'(lambda (y)
                    (send self :canvas-to-real xorigin y))
                     (floor (- (+ ycanvas (* .05 space)) (* .4 space))))))
       )

    (dotimes (i n)
       (send self :draw-text (elt labels i) (- xorigin 3)
                             (elt ycanvas i) 2 0)
       (send self :add-lines (list (list (- mindata (* .1 (- maxdata mindata)))
                                        (+ maxdata (* .1 (- maxdata mindata))))
                          (list (elt yreal i) (elt yreal i))) :type 'dashed))
    (dotimes (i num)
      (send self :point-coordinate 1 (iseq (* i n) (1- (* (1+ i) n)))
                                     yrealpoint))

  )
 ) 
)


(defmeth dotplot-proto :resize ()
  (call-next-method)
  (if (send self :old) (send self :redraw))
)


;;  By Jason Bond
;;  Address: jbond@laplace.stat.ucla.edu

#|
(def x1 (* 10 (normal-rand 52)))
(def x2 (combine (repeat (list "asd" "dd" "qweasdasdas" "12") 13)))


(def x3 (combine (list (repeat nil 5) (sample (iseq 50 140) 40 t)
                       (repeat nil 7))))
(def x4 (combine (+ 22 (normal-rand 30)) (repeat nil 4) (normal-rand 10)
                 (repeat nil 8)))

(def x5 (normal-rand 52))
(def x6 (* 12 (normal-rand 52)))
;labels x2)
|#

(defproto stem-proto () () graph-proto)


(defun stem-and-leaf-plot
  (data &key (stem-size 1) (leaf-size 1))
  "vars: list stem-size leaf-size 
       list - a list of numbers or symbols which to plot
       stem-size - the optional number of characters to be printed to the left
                   of the stem
                   (default is 1)
       leaf-size - the optional number of characters in each leaf
                   (default is 1)"
        (def sp (send stem-proto :new 2))
	(let* (
               (stream-ptr nil)
               (dat (parse-list data stem-size leaf-size))
               (vert (* 30 (length dat)))
               (horiz (* 30 (max (mapcar #'length dat))))
              )
           (send sp :add-slot 'data dat)
           (send sp :has-v-scroll vert)
           (send sp :has-h-scroll horiz)
         )
)


(defun string-list-cat (x)
  (do ((tmp "" (concatenate 'string tmp (car remainder)))
       (remainder x (cdr remainder)))
       ((equal remainder nil) tmp)))

(defun check (x)
            (every #'(lambda (x) (or (numberp x) (symbolp x))) x))
(defun to-string (x)
            (cond ((numberp x) (format nil "~f" (float x)))
                  ((symbolp x) (symbol-name x))
                  (T (error "unimplement type for to-string"))))

(defun string-left (x n)
            (do ((i 0 (+ i 1))
                 (result "" (concatenate 'string result (string (select x i)))))
                ((>= i n) result)))

(defun string-right-remainder (x n)
            (do ((i n (+ i 1))
                 (result "" (concatenate 'string result (string (select x i))))
                 (string-length (length x))) ((>= i string-length) result)))

(defun pad-window-to-string (x n)
            (cond ((symbolp x) (do ((i (length (symbol-name x)) (+ i 1))
                                    (result (symbol-name x)
                                            (concatenate 'string " " result)))
                                   ((>= i n) result)))
                  ((numberp x) (do ((i (length (format nil "~f" (float x)))
                                       (+ i 1))
                                    (result (format nil "~f" (float x))
                                            (concatenate 'string "0" result)))

                                   ((>= i n) result)))
                  (T (error
                      "pad-to-string: unimplemented type for pad-to-string"))))

(defun list-max (x)
            (do* ((remainder x (cdr remainder))
                  (maximum 0 maximum))
                 ((equal remainder nil) maximum)
                 (setf maximum (max maximum (car remainder)))))

(defun left-string-equal (x y n)
            (do ((i 0 (+ i 1)))
                ((or (>= i n) (char-not-equal (select x i) (select y i)))
                 (cond ((>= i n) T) (T nil)))))

(defun string-list-cat (x)
            (do ((tmp "" (concatenate 'string tmp (car remainder)))
                 (remainder x (cdr remainder)))
                ((equal remainder nil) tmp)))

(defun print-plot (x plot)
            (do (
                 (remainder x (cdr remainder))
                 (xposn 50 50)
                 (yposn 20 (+ 20 yposn))
                )
                ((equal remainder nil) T)
              (send plot :draw-string (car (car remainder)) xposn yposn)
              (send plot :draw-string  " | " (+ xposn 10) yposn)
              (send plot :draw-string (string-list-cat (cdr (car remainder)))
                                             (+ xposn 40) yposn)
            )
          )

(defun parse-list (x stem-size leaf-size)
            (let ((tmp nil) (stem nil) (remainder nil) (result nil))
              (cond ((not (check x)) (error "unimplemented type")))
              (setf tmp (mapcar #'to-string x))
              (setf tmp (map-elements #'pad-window-to-string x
                                      (max (mapcar #'length tmp))))
              (setf tmp (sort tmp #'string<))
              (do () ((equal tmp nil) (reverse result))
                  (setf stem
                        (select tmp (which (map-elements #'left-string-equal
                                                         tmp
                                                         (car tmp)
                                                         stem-size))))
                  (setf remainder
                        (select tmp
                                (which (mapcar #'not
                                               (map-elements
                                                #'left-string-equal
                                                tmp (car tmp) stem-size)))))
                  (setf result
                        (cons (cons
                               (string-left (car stem) stem-size)
                               (map-elements #'string-left
                                             (map-elements
                                              #'string-right-remainder
                                              stem stem-size) leaf-size))
                              result))
                  (setf tmp remainder))))



(defmeth stem-proto :data (&optional (val nil set))
 (if set (setf (slot-value 'data) val)
 (slot-value 'data))
)


(defmeth stem-proto :redraw ()
 (call-next-method)
 (send self :erase-window)
(let* (
       (x (send self :data))
      )
           (do* (
                 (remainder x (cdr remainder))
                 (xposn 50 50)
                 (yposn 20 (+ 20 yposn))
                 (point (send self :real-to-canvas xposn yposn))
                )
              ((equal remainder nil) t)
              (send self :draw-string (car (car remainder)) 
                                        xposn yposn)
              (send self :draw-string  " | " (+ xposn 10) yposn)
              (send self :draw-string (string-list-cat (cdr (car remainder)))
                                       (+ xposn 40) yposn)
             )
 )
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;                                                                        ;;
;;;;   Code that creates a spin plot with overlays that allows the user     ;;
;;;;   to insert, remove the regression plane; insert, remove projections;  ;;
;;;;   insert, remove hilighted points which also recomputes the regression ;;
;;;;   plane and projections.  Plots of residuals vs predicted and          ;;
;;;;   studentized-residuals vs. predicted are also produced.  Points can   ;;
;;;;   be colored by an index variable.  Residuals and Studentized          ;;
;;;;   residuals are also shown.  Plots are linked so case deletions can    ;;
;;;;   be made from any graph.                                              ;;
;;;;                                                                        ;;
;;;;   Comments, questions to Jason Bond (jbond@laplace.stat.ucla.edu)      ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defproto better-spin-proto '(project frame plane plane-there adjust plots) 
                             () spin-proto)
(defproto subordinate-graph-proto '(spin-object) () scatterplot-proto)

(defproto add-frame-overlay nil nil graph-overlay-proto)
(defproto project-overlay nil nil graph-overlay-proto)
(defproto plane-overlay nil nil graph-overlay-proto)
(defproto return-overlay nil nil graph-overlay-proto)
(defproto delete-overlay nil nil graph-overlay-proto)


(defmeth better-spin-proto :start-plot ()
(let* (
       (n (send self :num-points))
       (x (send self :point-coordinate 0 (iseq n)))
       (y (send self :point-coordinate 1 (iseq n)))
       (z (send self :point-coordinate 2 (iseq n)))
       (plane (send self :plane))
      )
 (send self :linked t)
 (send self :margin 70 0 0 30)
 (send self :frame nil)
 (send self :plane-there nil)
 (cond ((not plane) 
         (def reg1 (regression-model (list x y) z))
         (send self :plane (send reg1 :coef-estimates))))
 (send self :transformation (matrix (list 3 3) (list 0 1 0 0 0 1 1 0 0)))
 (send self :add-overlay (send add-frame-overlay :new))
 (send self :add-overlay (send project-overlay :new))
 (send self :add-overlay (send plane-overlay :new))
 (send self :add-overlay (send return-overlay :new))
 (send self :add-overlay (send delete-overlay :new))
 )
)

(defmeth add-frame-overlay :redraw ()
 (let (
       (graph (send self :graph))
      )
   (send graph :frame-rect 10 10 10 10)
   (send graph :draw-string "Frame" 25 20)
 )
)

(defmeth add-frame-overlay :do-click (x y m1 m2)
 (let (
       (graph (send self :graph))
      )
  (cond ((and (< 10 x 20) (< 10 y 20))
          (send graph :frame (not (send graph :frame)))
          (send graph :cleanup)))
 )
)



(defmeth project-overlay :redraw ()
 (let ( 
       (graph (send self :graph))
      )
   (send graph :frame-rect 10 30 10 10)
   (send graph :draw-string "Proj" 25 40)
 )
)


(defmeth project-overlay :do-click (x y m1 m2)
 (let (
       (graph (send self :graph))
      )
    (cond ((and (< 10 x 20) (< 30 y 40))
            (send graph :project (not (send graph :project)))
            (send graph :cleanup)))
 )
)

(defmeth plane-overlay :redraw ()
 (let (
       (graph (send self :graph))
      )
   (send graph :frame-rect 10 50 10 10)
   (send graph :draw-string "Plane" 25 60)
 )
)


(defmeth plane-overlay :do-click (x y m1 m2)
  (let (
        (graph (send self :graph))
       )
    (cond ((and (< 10 x 20) (< 50 y 60))
              (send graph :plane-there (not (send graph :plane-there)))
              (send graph :cleanup)))
  )
)

(defmeth delete-overlay :redraw ()
 (let ( 
       (graph (send self :graph))
      )
   (send graph :frame-rect 10 70 10 10)
   (send graph :draw-string "+/- Pts" 25 80)
 )
)


(defmeth delete-overlay :do-click (x y m1 m2)
 (let* (
        (graph (send self :graph))
        (n (send graph :num-points))
       )
    
    (cond ((and (< 10 x 20) (< 70 y 80))
           (cond ((= (length (send graph :points-selected)) 0)

                  (send graph :point-state (iseq n) 'normal)
                  (send graph :adjust (send graph :points-selected))
                  (send graph :cleanup))

                  (t (send graph :adjust (send graph :points-selected))
                     (send graph :point-state (send graph :points-selected)
                                                'invisible)
                     (send graph :cleanup)))))
 )
)


(defmeth return-overlay :redraw ()
 (let (
       (graph (send self :graph))
      )
   (send graph :frame-rect 10 90 10 10)
   (send graph :draw-string "Back" 25 100)
 )
)

(defmeth return-overlay :do-click (x y m1 m2)
  (let (
        (graph (send self :graph))
       )
    (if (and (< 10 x 20) (< 90 y 100))
            (send graph :transformation
               (matrix (list 3 3) (list 0 1 0 0 0 1 1 0 0))))
     (send self :redraw)
  )
)


(defmeth better-spin-proto :adjust-screen-point (i)
 (let* (
        (last-adjust (first (last (send self :adjust))))
        (vars (variables))
       )
(if last-adjust
    (cond ((= i last-adjust) 
            (cond ((not (every #'(lambda (x y) (equalp x y))
                      (send self :point-state (iseq (send self :num-points)))
                      (send self :last-point-state
                                     (iseq (send self :num-points)))))
                   (send reg1 :included (send self :point-showing
                           (iseq (send self :num-points))))
                   (cond ((send self :plots)  
                     (cond ((find 'residplot vars)
                      (send residplot :clear-points)
                      (send residplot :add-points
                        (list (send reg1 :fit-values) (send reg1 :residuals)))
                      (send residplot :adjust-to-data)
                      (send residplot :points-showing
                         (send self :points-showing))))

                     (cond ((find 'studplot vars)
                      (send studplot :clear-points)
                      (send studplot :add-points
                         (list (send reg1 :fit-values)
                               (send reg1 :studentized-residuals)))
                      (send studplot :adjust-to-data)
                      (send studplot :points-showing 
                         (send self :points-showing))))))

                     (send self :plane (send reg1 :coef-estimates))
                     (send reg1 :display)
                     (send self :cleanup))))))
  (call-next-method i)
  (if last-adjust 
  (if (and (= (send self :num-points) (length (send self :points-showing)))
           (= i last-adjust)) (send self :adjust nil)))
 )
)

(defmeth better-spin-proto :cleanup ()
   (send self :clear-lines)
   (if (send self :project) (send self :project-points))
   (if (send self :frame) (send self :draw-frame))
   (if (send self :plane-there) (send self :draw-plane))
   (send self :redraw)
)

(defmeth better-spin-proto :draw-plane ()
  (let (
       (plane (send self :plane))
      )
  (setf plane (send self :plane))

  (send self :abcplane (first plane) (second plane) (third plane))
  (send self :redraw)
;  (send self :redraw-overlays)
;  (send self :redraw-content)
 )
)

(defmeth better-spin-proto :project-points ()
 (let* (
        (n (send self :num-points))
        (vars (send self :content-variables))
        (plane (send self :plane))
        (x (select (send self :point-coordinate (first vars) (iseq n))
              (send self :points-showing)))
        (y (select (send self :point-coordinate (second vars) (iseq n))
              (send self :points-showing)))
        (z (select (send self :point-coordinate (third vars) (iseq n))
              (send self :points-showing)))
        (newz (+ (first plane) (* (second plane) x)
                 (* (third plane) y)))
       )
     (mapcar #'(lambda (x y z1 z2) (send self :add-lines
                    (list (list x x) (list y y) (list z1 z2)))) x y z newz)
  )
)


(defmeth better-spin-proto :color-variable (color-variable)
  (let* (
         (vars (send self :content-variables))
         (n (send self :num-points))
         (color-var (send self :point-coordinate color-variable (iseq n)))
         (num-colors (length (remove-duplicates color-var)))
         (categories (remove-duplicates color-var))
         (num-colors (length categories))
         (colors (select (color-symbols) (list 6 5 3 7 1 2 0 4)))
         (colors (mapcar #'(lambda (x) (elt colors (position x categories))) 
                    color-var))
        )
    (send self :use-color t)
    (send self :point-color (iseq n) colors)
  )
)


(defmeth better-spin-proto :frame (&optional (val nil set))
 (if set (setf (slot-value 'frame) val)
  (slot-value 'frame))
)

(defmeth better-spin-proto :project (&optional (val nil set))
 (if set (setf (slot-value 'project) val)
  (slot-value 'project))
)

(defmeth better-spin-proto :plane (&optional (val nil set))
 (if set (setf (slot-value 'plane) val)
  (slot-value 'plane))
)

(defmeth better-spin-proto :plane-there (&optional (val nil set))
 (if set (setf (slot-value 'plane-there) val)
  (slot-value 'plane-there))
)

(defmeth better-spin-proto :adjust (&optional (val nil set))
 (if set (setf (slot-value 'adjust) val)
  (slot-value 'adjust))
)


(defmeth better-spin-proto :plots (&optional (val nil set))
 (if set (setf (slot-value 'plots) val)
  (slot-value 'plots))
)


(defmeth subordinate-graph-proto :spin-object (&optional (val nil set))
 (if set (setf (slot-value 'spin-object) val)
   (slot-value 'spin-object))
)







(defmeth subordinate-graph-proto :close ()
    (call-next-method)
    (let (
          (vars (variables))
          (varplot (first (which (mapcar #'(lambda (x) (equalp x self))
                       (mapcar #'symbol-value (variables))))))
         )
  
     (undef (elt vars varplot))
    )
)


(defmeth better-spin-proto :draw-frame ()
 (let* (
        (n (send self :num-points))
        (vars (send self :content-variables))
        (x (send self :point-coordinate (first vars) (iseq n)))
        (y (send self :point-coordinate (second vars) (iseq n)))
        (z (send self :point-coordinate (third vars) (iseq n)))
        (maxx (+ (max x) (* .1 (- (max x) (min x)))))
        (minx (- (min x) (* .1 (- (max x) (min x)))))
        (maxy (+ (max y) (* .1 (- (max y) (min y)))))
        (miny (- (min y) (* .1 (- (max y) (min y)))))
        (maxz (+ (max z) (* .1 (- (max z) (min z)))))
        (minz (- (min z) (* .1 (- (max z) (min z)))))
       )

  (mapcar #'(lambda (x) (send self :add-lines (list
              (list (elt x 0) (elt x 1)) (list (elt x 2) (elt x 3))
              (list (elt x 4) (elt x 5)))))
    (list (list minx minx miny maxy maxz maxz) 
          (list maxx maxx miny maxy maxz maxz)
          (list minx maxx miny miny maxz maxz)
          (list minx maxx maxy maxy maxz maxz)

          (list minx minx miny maxy minz minz) 
          (list maxx maxx miny maxy minz minz)
          (list minx maxx miny miny minz minz)
          (list minx maxx maxy maxy minz minz)
 
          (list minx minx miny miny minz maxz)
          (list maxx maxx miny miny minz maxz)
          (list minx minx maxy maxy minz maxz)
          (list maxx maxx maxy maxy minz maxz)))
  )
 (send self :redraw-background)
 (send self :redraw-overlays)
 (send self :redraw-content)
)


(defun spinit (vars &key (color-variable nil) (plots nil) (title "Graph") 
                          location (go-away t) menu (black-on-white nil)
                          has-h-scroll has-v-scroll menu-title point-label
                          (variable-labels (list "X" "Y" "Z"))
                          (plane nil))

  (let (
        (sp (send better-spin-proto :new (length vars) 
              :variable-labels variable-labels :location location 
              :go-away go-away :menu menu :black-on-white black-on-white
              :has-h-scroll has-h-scroll :has-v-scroll has-v-scroll 
              :point-label point-label :plane plane))
       )
    (send sp :add-points vars)
    (send sp :adjust-to-data)
#|    (send sp :point-label (iseq (length (first vars)))
            (if point-label point-label
           (mapcar #'(lambda (x) (format nil "~a" x)) 
               (iseq (length (first vars))))))
|#
    (if color-variable (send sp :color-variable color-variable))
    (send sp :start-plot)
    (if plots (send sp :subordinate-graphs vars sp))
   sp
  )
)


(defmeth better-spin-proto :subordinate-graphs (vars sp)
    (def residplot (send subordinate-graph-proto :new 2 :variable-labels 
                    (list "Predicted" "Residuals")
              :title "Residuals vs. Predicted" :linked t))
    (def studplot (send subordinate-graph-proto :new 2 :variable-labels
                    (list "Predicted" "Studentized Residuals")
               :title "Studentized Residuals vs. Predicted" :linked t))
    
    (send residplot :add-points 
     (list (select (send reg1 :fit-values) 
                 (which (send reg1 :included (iseq (send reg1 :num-cases)))))
         (select (send reg1 :residuals)
                 (which (send reg1 :included (iseq (send reg1 :num-cases)))))))

    (send studplot :add-points 
     (list (select (send reg1 :fit-values)
                 (which (send reg1 :included (iseq (send reg1 :num-cases)))))
         (select (send reg1 :studentized-residuals)
                 (which (send reg1 :included (iseq (send reg1 :num-cases)))))))
    (send residplot :adjust-to-data)
    (send residplot :linked t)
    (send studplot :adjust-to-data)
    (send studplot :linked t)
    (send residplot :spin-object sp)
    (send studplot :spin-object sp)
    (send sp :plots (list studplot residplot))
)    

(defmeth subordinate-graph-proto :erase-selection ()
 (let (
       (spin-object (send self :spin-object))
      )
   (send spin-object :adjust (send self :points-selected))
   (call-next-method)
 )
)


