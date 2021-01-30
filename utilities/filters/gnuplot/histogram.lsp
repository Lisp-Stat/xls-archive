;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;                                                               ;;
;;;;  Code based on Chapter 1 of Haerdle, Smoothing Techniques     ;;
;;;;  Creates a histogram with many options.  A hollow histogram   ;;
;;;;  can be drawn.  A series of lines connecting the midpoints of ;;
;;;;  the tops of the bins can be drawn.  Frequencies or relative  ;;
;;;;  frequencies may be displayed.                                ;;
;;;;  By Jan Deleeuw.  deleeuw@galton.stat.ucla.edu                ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defproto my-hist-proto () () scatterplot-proto)

(defun my-histogram 
  (data h x0 &key (plot t) (hollow nil) (prob nil) (poly nil) 
                           (title "Histogram"))
 (setf pp (if plot (send my-hist-proto :new 2 :show nil :title title)))
 (send pp :add-slot 'data data)
 (send pp :add-slot 'h h)
 (send pp :add-slot 'x0 x0)
 (send pp :add-slot 'plot plot)
 (send pp :add-slot 'hollow hollow)
 (send pp :add-slot 'prob prob)
 (send pp :add-slot 'title title)
 (send pp :add-slot 'poly poly)
 (let* (
        (slider (send menu-item-proto :new "Change-Bins"
                   :action #'(lambda () (send pp :create-slider))))
        (poly (send menu-item-proto :new "Polygon Frequencyiogram"
                   :action #'(lambda () 
                             (send pp :poly (not (send pp :poly)))
                             (send pp :main))))
        (hollow (send menu-item-proto :new "Hollow/Full Histogram"
                   :action #'(lambda () 
                             (send pp :hollow (not (send pp :hollow)))
                             (send pp :main))))
        (ppmenu (send menu-proto :new "Histogram"))
       )
   (send ppmenu :append-items slider poly hollow)
   (send pp :menu ppmenu)
   (send pp :main) 
 )
)


(defmeth my-hist-proto :create-slider ()
 (let (
       (ldata (length (remove-duplicates (send self :data))))
      )
  (interval-slider-dialog (list 0 ldata) 
                          :points (* 10 ldata)
                          :action #'(lambda (x) 
                            (send self :clear)
                            (send self :h x)
                            (send self :main)))
 )
)
    

(defmeth my-hist-proto :main ()
(let* (
       (data (send self :data))
       (h (send self :h))
       (x0 (send self :x0))
       (plot (send self :plot))
       (hollow (send self :hollow))
       (prob (send self :prob))
       (poly (send self :poly))
       (title (send self :title))
       (nn (length data))
       (ft (floor (/ (- (min data) x0) h)))
       (lt (ceiling (/ (- (max data) x0) h)))
       (bm (+ x0 (* h (iseq ft lt))))
       (nb (length bm))
       (n0 (repeat 0 nn))
       (n1 (repeat 1 nn))
       (cn (count-in-bins data h x0))
       (nh (* nn h))
       )
(cond (plot (cond (hollow (send self :draw-mode 'xor))
                  (t (send self :draw-mode 'normal)))
            (send self :clear)
            (dotimes (i (1- nb))
                   (let* (
                         (a (elt bm i))
                         (b (elt bm (1+ i)))
                         (c (elt cn i))
                         (d (if prob (/ c nh) c))
                         )

             (cond (poly (send self :draw-mode 'normal)
                     (send self :add-points (list (/ (+ a b) 2)) (list d)))
                   (t (send self :add-lines (list a a b b) (list 0 d d 0))))))
          (if poly (send self :add-lines 
                              (send self :point-coordinate 0 
                                     (send self :points-showing))
                              (send self :point-coordinate 1 
                                     (send self :points-showing))))
          (send pp :adjust-to-data)
          (send pp :show-window t))
         (t (let (
                (midp (+ x0 (* h (rseq (- ft .5) (+ lt .5) (+ 2 (- lt ft))))))
                (est (/ cn (* nn h)))
                )
            (list midp (concatenate 'list (list 0) est (list 0))))))
))



(defun count-in-bins (data h x0)
(let* (
       (n (length data))
       (indx (floor (/ (- data x0) h)))
       (jndx (- indx (min indx)))
       (nbin (1+ (max jndx)))
       (bcnt (repeat 0 nbin))
      )
(mapcar #'(lambda (x) (setf (elt bcnt x) (1+ (elt bcnt x)))) jndx)
bcnt
))





(defmeth my-hist-proto :data (&optional (val nil set))

(if set (setf (slot-value 'data) val))

(slot-value 'data))


(defmeth my-hist-proto :h (&optional (val nil set))

(if set (setf (slot-value 'h) val))

(slot-value 'h))


(defmeth my-hist-proto :x0 (&optional (val nil set))

(if set (setf (slot-value 'x0) val))

(slot-value 'x0))



(defmeth my-hist-proto :plot (&optional (val nil set))

(if set (setf (slot-value 'plot) val))

(slot-value 'plot))



(defmeth my-hist-proto :poly (&optional (val nil set))

(if set (setf (slot-value 'poly) val))

(slot-value 'poly))



(defmeth my-hist-proto :prob (&optional (val nil set))

(if set (setf (slot-value 'prob) val))

(slot-value 'prob))



(defmeth my-hist-proto :hollow (&optional (val nil set))

(if set (setf (slot-value 'hollow) val))

(slot-value 'hollow))



(defmeth my-hist-proto :title (&optional (val nil set))

(if set (setf (slot-value 'title) val))

(slot-value 'title))

#|

(defmeth graph-window-proto :close ()
  (exit)
)
 

(defmeth dialog-proto :close ()
 (exit)
)

(defun read-files ()
  (let* (
         (f (open "/u/quetelet/m2/www/httpd/htdocs/textbook/lisp/data/dirfile.lsp"))
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
               (setf data (read-data-columns (concatenate 'string
                          "/u/quetelet/m2/www/httpd/htdocs/textbook/lisp/data/"
                              (select file-names
                                 (send main-window :selection)))))
               (my-histogram (first data) (min (first data)) 1)
               (send reader :hide-window))))
        )
    (setf reader (send dialog-proto :new (list main-window)))
  )
)


(defmeth dialog-proto :close ()
 (exit)
)
(read-dialog)
|#


