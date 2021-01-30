(defproto bitmap-edit-proto
  '(bitmap h v mutrue samples) nil graph-proto)

(defmeth bitmap-edit-proto :isnew (width height)
  (call-next-method 2)
  (setf (slot-value 'bitmap)
        (matrix (list height width) 
                   (sample (combine (repeat 1 (floor (* height width .25)))
                                    (repeat 0 (floor (* height width .75))))
                      (* height width))))
) 


(defmeth bitmap-edit-proto :bitmap (&optional (val nil set))
 (if set (setf (slot-value 'bitmap) val)
 (slot-value 'bitmap))
)

(defmeth bitmap-edit-proto :v () (slot-value 'v))

(defmeth bitmap-edit-proto :h () (slot-value 'h))

(defmeth bitmap-edit-proto :mutrue (&optional (val nil set))
 (if set (setf (slot-value 'mutrue) val)
    (slot-value 'mutrue))
)

(defmeth bitmap-edit-proto :calculate-mu ()
  (let (
        (b (send self :bitmap))
       )
    (send self :mutrue (/ (sum b) (length (combine b))))
  )
)


(defmeth bitmap-edit-proto :samples (&optional (val nil set))
 (if set (setf (slot-value 'samples) val)
    (slot-value 'samples))
)


(defmeth bitmap-edit-proto :resize ()
  (let ((m (array-dimension (send self :bitmap) 0))
        (n (array-dimension (send self :bitmap) 1))
        (height (send self :canvas-height))        
        (width (send self :canvas-width)))
    (setf (slot-value 'v)
          (coerce (floor (* (Iseq 0 m) (/ height m)))
                  'vector))
    (setf (slot-value 'h)
          (coerce (floor (* (Iseq 0 n) (/ width n)))
                  'vector))))

(defmeth bitmap-edit-proto :draw-pixel (i j)
  (let* ((b (send self :bitmap))
         (v (send self :v))
         (h (send self :h))
         (left (aref h j))
         (right (aref h (+ j 1)))
         (top (aref v i))
         (bottom (aref v (+ i 1))))
    (send self (if (= 1 (aref b i j)) :paint-rect :erase-rect)
          left top (- right left) (- bottom top))))

(defmeth bitmap-edit-proto :draw-box (left right top bottom)
  (let* (
         (v (send self :v))
         (h (send self :h))
         (left (aref h left))
         (right (aref h (1+ right)))
         (top (aref v (1+ top)))
         (bottom (aref v bottom))
        )
(send self :draw-mode 'xor)
(send self :draw-line left bottom left top)
(send self :draw-line left top right top)
(send self :draw-line right top right bottom)
(send self :draw-line right bottom left bottom)
(send self :draw-mode 'normal)
 )
)

(defmeth bitmap-edit-proto :redraw ()
  (let* ((b (send self :bitmap))
         (m (array-dimension b 0))
         (n (array-dimension b 1))
         (v (coerce (send self :v) 'list))
         (h (coerce (send self :h) 'list))
         (fv (elt v 0))
         (lv (+ (elt v 1) (elt v (1- m))))
         (fh (elt h 0))
         (lh (+ (elt h 1) (elt h (1- n))))
         (v (append v (list lv)))
         (h (append h (list lh)))
         (width (send self :canvas-width))
         (height (send self :canvas-height)))
    (send self :start-buffering)
    (send self :erase-rect 0 0 width height)
    (dotimes (i m)
          (dotimes (j n)
                   (send self :draw-pixel i j)))
    (dotimes (i (length h))
       (send self :draw-line (elt h i) fv (elt h i) lv)
       (send self :draw-line fh (elt v i) lh (elt v i)))
    (send self :buffer-to-screen)))

(defmeth bitmap-edit-proto :set-pixel (x y)
  (let* ((b (send self :bitmap))
         (m (array-dimension b 0))
         (n (array-dimension b 1))
         (width (send self :canvas-width))
         (height (send self :canvas-height))
         (i (min (floor (* y (/ m height))) (- m 1)))
         (j (min (floor (* x (/ n width))) (- n 1))))
    (setf (aref b i j) (if (= (aref b i j) 1) 0 1))
    (send self :draw-pixel i j)))


(defmeth bitmap-edit-proto :do-it (x y m1 m2)
  (send self :set-pixel x y))

(defmeth bitmap-edit-proto :name-bitmap ()
  (let ((str (get-string-dialog "Symbol for the bitmap:")))
    (if str
        (let ((name (with-input-from-string (s str) (read s))))
          (setf (symbol-value name) (send self :bitmap))))))

(defmeth bitmap-edit-proto :bitmap-as-cursor (yes)
  (if yes (make-cursor 'temp-cursor (send self :bitmap)))
  (send self :cursor (if yes 'temp-cursor 'arrow)))

(setf bitmenu (send menu-proto :new "Bitmap")) 

(setf name-item
      (send menu-item-proto :new "Name Bitmap..."
            :action #'(lambda () (send w :name-bitmap))))

(setf mouse (send menu-item-proto :new "Mouse Modes"
                    :action #'(lambda () (send w :choose-mouse-mode))))

(setf cursor-item
      (send menu-item-proto :new "Use as Cursor"
            :action
            #'(lambda ()
                (let ((mark (send cursor-item :mark)))
                  (send w :bitmap-as-cursor (not mark))
                  (send cursor-item :mark (not mark))))))
(setf redraw-item 
      (send menu-item-proto :new "Redraw window"
            :action #'(lambda () (send w :redraw))))



             
(send bitmenu :append-items name-item cursor-item mouse redraw-item)

(defmeth bitmap-edit-proto :show-it (x y m1 m2)
  (let* (
         (b (send self :bitmap))
         (m (array-dimension b 0))
         (n (array-dimension b 1))
         (width (send self :canvas-width))
         (height (send self :canvas-height))
         (i (min (floor (* y (/ m height))) (- m 1)))
         (j (min (floor (* x (/ n width))) (- n 1)))
         (left (max 0 (- j (floor (* .05 n)))))
         (right (min (1- m) (+ j (floor (* .05 n)))))
         (bottom (max 0 (- i (floor (* .05 m)))))
         (top (min (1- n) (+ i (floor (* .05 m)))))
         (wide (+ right (- left) 1))
         (height (+ top (- bottom) 1))
         (mag-b (matrix (list wide height) 
                        (combine (mapcar #'(lambda (x) 
                                   (mapcar #'(lambda (y) (aref b x y))
                                      (iseq left right))) (iseq bottom top)))))

       )
      (send expand-window :clear)
      (send expand-window :bitmap mag-b)
      (send expand-window :new-resize)
      (send expand-window :new-redraw)
      (send self :draw-box left right top bottom)
      (send info-box :tell)
  )
)

(defmeth histogram-proto :tell ()
  (let* (
         (b (send expand-window :bitmap))
         (sum (sum (combine b)))
         (total (length (combine b)))
         (sample-prop (/ sum total))
         (truemu (send w :mutrue))
         (samp (append (send w :samples) (list sample-prop)))
         (sampmu (mean samp))
        )
    (send w :samples samp)
    (send self :erase-window)
    (send self :add-points (list sample-prop))
    (send self :x-axis t)
    (send self :adjust-to-data)
    (send self :draw-string 
         (format nil "The Sample proportion is: ~4,3f" sample-prop) 10 15)
    (send self :draw-string 
         (format nil "The True proportion is: ~4,3f" truemu) 10 30)
  )
)




(defproto expand-window-proto '(bitmap v h) () graph-window-proto)

(setf expand-window (send expand-window-proto :new :title "Magnified Sample" 
                     :location (list 500 50)))
(setf info-box (send histogram-proto :new 1 
                   :location (list 320 550) :x-axis t 
                   :size (list 300 300)))

(defmeth expand-window-proto :bitmap (&optional (val nil set))
 (if set (setf (slot-value 'bitmap) val)
 (slot-value 'bitmap))
)


(defmeth expand-window-proto :new-redraw ()
  (let* ((b (send self :bitmap))
         (m (array-dimension b 0))
         (n (array-dimension b 1))
         (v (coerce (send self :v) 'list))
         (h (coerce (send self :h) 'list))
         (fv (elt v 0))
         (lv (+ (elt v 1) (elt v (1- m))))
         (fh (elt h 0))
         (lh (+ (elt h 1) (elt h (1- n))))
         (v (append v (list lv)))
         (h (append h (list lh)))
         (width (send self :canvas-width))
         (height (send self :canvas-height)))
    (send self :start-buffering)
    (send self :erase-rect 0 0 width height)
    (dotimes (i m)
          (dotimes (j n)
                   (send self :draw-pixel i j)))
    (dotimes (i (length h))
       (send self :draw-line (elt h i) fv (elt h i) lv)
       (send self :draw-line fh (elt v i) lh (elt v i)))

    (send self :buffer-to-screen)))


(defmeth expand-window-proto :new-resize ()
  (let ((m (array-dimension (send self :bitmap) 0))
        (n (array-dimension (send self :bitmap) 1))
        (height (send self :canvas-height))
        (width (send self :canvas-width)))
    (setf (slot-value 'v)
          (coerce (floor (* (Iseq 0 m) (/ height m)))
                  'vector))
    (setf (slot-value 'h)
          (coerce (floor (* (Iseq 0 n) (/ width n)))
                  'vector))))

(defmeth expand-window-proto :draw-pixel (i j)
  (let* ((b (send self :bitmap))
         (v (send self :v))
         (h (send self :h))
         (left (aref h j))
         (right (aref h (+ j 1)))
         (top (aref v i))
         (bottom (aref v (+ i 1))))
    (send self (if (= 1 (aref b i j)) :paint-rect :erase-rect)
          left top (- right left) (- bottom top))))


(defmeth expand-window-proto :v () (slot-value 'v))

(defmeth expand-window-proto :h () (slot-value 'h))

(defun sampler ()
(setf w (send bitmap-edit-proto :new 100 100))
(send w :calculate-mu)
(send w :menu bitmenu)
(send w :add-mouse-mode 'magnify-section
                  :title "Magnify"
                  :cursor 'finger
                  :click :show-it)
(send w :add-mouse-mode 'start
                  :title "start"
                  :cursor 'finger
                  :click :do-it)
(send w :mouse-mode 'magnify-section)
)

(sampler)

