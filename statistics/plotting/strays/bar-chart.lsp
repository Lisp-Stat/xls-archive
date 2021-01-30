
(defproto bar-chart-proto () () graph-proto)

(defmeth bar-chart-proto :data (&optional (val nil set))
 (if set (setf (slot-value 'data) val)
 (slot-value 'data)))

(defmeth bar-chart-proto :categories (&optional (val nil set))
 (if set (setf (slot-value 'categories) val)
 (slot-value 'categories)))


(defmeth bar-chart-proto :redraw () 
(call-next-method)
(let* (
       (x-size (first (send self :size)))
       (y-size (second (send self :size)))
       (data (send self :data))
       (maxdata (max data))
       (mindata (min data))
       (bottom (/ (- maxdata mindata) 4))
       (categories (send self :categories))
       (n (length categories))
       (xstart 0)
       (xi (/ (1+ n)))
       (xmidpoints (+ xstart (* (iseq 1 n) xi)))
       (barwidth (/ xi 3))
       (xtop (mapcar #'(lambda (x) (list (- x barwidth) (+ x barwidth)))
                 xmidpoints))
      )
  (mapcar #'(lambda (x y) 
             (let ((coords (send self :real-to-canvas y (- mindata bottom))))
              (send self :draw-text x (first coords) (second coords) 1 1)))
        categories xmidpoints)
  (mapcar #'(lambda (x y) 
     (send self :add-lines (transpose (list 
              (list (first x) (first (send self :range 1)))
              (list (first x) y)
              (list (second x) y)
              (list (second x) (first (send self :range 1))))))) xtop data)))

  
(defun bar-chart (data &key categories variable-labels)
"Args: data &key caregories variable-labels
Makes a bar chart of DATA using the x-axis labels CATEGORIES.  The x-axis
and y-axis labels may optionally be provided as a list of strings."
 (let* (
        (bc (send bar-chart-proto :new 2 :variable-labels variable-labels))
        (categories (if categories categories 
               (mapcar #'(lambda (x) (format nil "~a" x)) 
                   (iseq 1 (length data)))))
       (maxdata (max data))
       (mindata (min data))
       (bottom (/ (- maxdata mindata) 4))
       )
   (send bc :add-slot 'data data)
   (send bc :add-slot 'categories categories)
   (send bc :range 1 (- mindata bottom) (+ maxdata bottom))
   (send bc :x-axis t t 0)
   (send bc :y-axis t t 4)
   (send bc :redraw)
   bc
 )
)



