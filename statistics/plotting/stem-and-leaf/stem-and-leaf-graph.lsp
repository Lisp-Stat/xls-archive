(defun stem-and-leaf-plot
  (data &key (stem-size 1) (leaf-size 1) (where *STANDARD-OUTPUT*))
  "Args: list stem-size leaf-size where
       list - a list of numbers or symbols which to plot
       stem-size - the optional number of characters to be printed to the left
                   of the stem
                   (default is 1)
       leaf-size - the optional number of characters in each leaf
                   (default is 1)
       where - an optional file name (defaults to stdout)"
         (def sp (send graph-proto :new 2))
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
                                      (list-max (mapcar #'length tmp))))
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



(defmeth graph-proto :data (&optional (val nil set))
 (if set (setf (slot-value 'data) val)
 (slot-value 'data))
)


(defmeth graph-proto :redraw ()
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


