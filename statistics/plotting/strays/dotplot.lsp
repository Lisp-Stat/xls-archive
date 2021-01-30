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
                   
 "Args: DATA LABELS
  DATA is a list of numbers and LABELS is a list of strings, both of
  which are the same length.  Makes a dotplot of DATA using LABELS."
 (let* (
        (dp (send dotplot-proto :new 2 :variable-labels variable-labels
                                :title title))
        (n (length data))
        (len nil)
        (maxdata (max data))
        (mindata (min data))
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
    (send dp :add-points (list data (repeat 0 n)))
    (send dp :point-label (iseq n) labels)
    (send dp :x-axis t t nice-range)
    (send dp :y-axis t nil 0)
    (if (> n 20) (send dp :has-v-scroll (floor (* space n 1.7)))
                 (send dp :size 300 200))

    (send dp :range 0 (- mindata (* .1 (- maxdata mindata))) 
                      (+ maxdata (* 1.1 (- maxdata mindata))))
    (send dp :margin (floor (* (max (mapcar #'(lambda (x) 
                         (send dp :text-width x)) 
                       labels)) .8)) 0 0 0)
    (if has-h-scroll (send dp :has-h-scroll has-h-scroll))
    (send dp :point-symbol (iseq n) 'disk)
    (send dp :linked t)
    (send dp :showing-labels t)
    (send dp :sort sort)
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
 (let* (
        (data (send self :data))
        (sort (send self :sort))

        (rankdata (rank data))
        (n (length data))

        (labels (cond (sort (let (
                                  (templist (repeat nil n))
                                 )
                             (mapcar #'(lambda (x y) (setf (elt templist x) y))
                                  rankdata (send self :labels))
                                templist))
                     (t (send self :labels))))
        (space (floor (* 1.5 (send self :text-ascent))))
        (maxdata (max data))
        (mindata (min data))
        (xorigin (first (send self :content-origin)))

        (yorigin (second (send self :content-origin)))
        (ycanvas (mapcar #'(lambda (i) (- yorigin (* i space))) 
                              (iseq n)))
        (yreal (mapcar #'second (mapcar #'(lambda (y)
                   (send self :canvas-to-real xorigin y))
                     (floor (- ycanvas (* .4 space))))))
        (yrealpoint (if sort 
                 (select (mapcar #'second (mapcar #'(lambda (y)
                   (send self :canvas-to-real xorigin y))
                     (floor (- (+ ycanvas (* .05 space)) (* .4 space)))))
                     rankdata)
                  (mapcar #'second (mapcar #'(lambda (y)
                    (send self :canvas-to-real xorigin y))
                     (floor (- (+ ycanvas (* .05 space)) (* .4 space)))))))
       )
    (dotimes (i n)
       (send self :draw-text (elt labels i) (- xorigin 3)
                             (elt ycanvas i) 2 0)
       (send self :add-lines (list (list (- mindata (* .1 (- maxdata mindata)))
                                        (+ maxdata (* 1.1 (- maxdata mindata))))
                          (list (elt yreal i) (elt yreal i))) :type 'dashed))
    (send self :point-coordinate 1 (iseq n) yrealpoint)
 nil
 )
)


(defmeth dotplot-proto :resize ()
  (call-next-method)
  (if (send self :old) (send self :redraw))
)


;;  By Jason Bond
;;  Address: jbond@laplace.stat.ucla.edu

(def x1 (* 10 (normal-rand 52)))
(def x2 (combine (repeat (list "asd" "dd" "qweasdasdas" "12") 13)))


(def x1 (sample (iseq 50 70) 8))
(def x2 (iseq 10 17))

(def dot (dotplot x1 x2 :sort t :variable-labels (list "qwe" "asdf")))
(labels x2)


