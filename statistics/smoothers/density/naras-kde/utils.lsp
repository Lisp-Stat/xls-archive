(provide "utils")


(defun get-data-files ()
  "Args: ()
Prompts the user for one or a list of filenames. Returns the result as a list."
  (let ((n (select (get-value-dialog "How many Markov Chains?"
				     :initial 1) 0))
	(fl ()))
    (cond ((= n 1)
	   (list (get-string-dialog (format nil "Please enter~%~
				      input file name~%~
				      Ex: data.in"))))
	  ((> n 1) (dotimes (i n)
			    (setf fl (cons 
				      (get-string-dialog 
				       (format nil "Please enter~%~
				               input file name ~d~%~
					       Ex: data.in" i))
				      fl)))
	   (reverse fl))
	  (t (error "Your input was inappropriate. Please try again!")))))

(defun get-hyper-vals (m p)
  "Args (m p)
Returns the hyper-values supplied by the user. M is the no. of chains and
p is the number of parameters"
  (cond ((= m 1) (flet ((getlist (x) 
				 (select 
				  (get-value-dialog 
				   (format nil 
					   "Enter Hyper-parameters~%~
                                             for Parameter ~d~%~
                                             AS A LIST.~%~
                                             Ex: (list 1 2)" x))
				  0)))
		       (mapcar #'getlist (iseq p))))
	((> m 1) (let ((res ()))
		   (dotimes (i m)
			    (flet ((getlist (x) 
					    (select 
					     (get-value-dialog 
					      (format nil 
						      "For Chain ~d~%~
                                                       Enter Hyper-parameters~%~
                                                       for Parameter ~d~%~
                                                       AS A LIST.~%~
                                                       Ex: (list 1 2)" i x))
					     0)))
				  (setf res 
					(cons (mapcar #'getlist (iseq p))
					      res))))
		   (reverse res)))
	(t (error "Your input was inappropriate. Please try again!"))))

(defun make-same-struct (x y)
  (let ((w ())
	(c 0))
    (dotimes (i (length x))
	     (let* (( s (select x i))
		    (l (if (listp s)
			   (length s)
			 1)))
	       (cond 
		((listp s) (setf w (cons (select y (+ c (iseq l))) w)))
		(t (setf w (cons (select y c) w))))
	       (setf c (+ c l))))
    (reverse w)))

(defun even-seq (b)
  (1+ (* 2 (iseq (floor (/ b 2))))))

(defun definedp (symbol)
  "Method args: symbol
Returns t if symbol is defined and bound."
  (and (boundp symbol) symbol))

(defun ask-user (&rest args)
  "Method args: &rest args
Asks the user to enter data with a string which is all of args concatenated."
  (let ((tmp ""))
    (dolist (x args)
            (setf tmp (concatenate 'string tmp x)))
    (get-value-dialog tmp)))

(defun make-choose-mes (str)
  "Method args: str
Returns a message with the same name as the string with spaces
replaced by hyphens and the sequence of letters choose- prepended
to the string."
  (let ((tmp (coerce (mapcar #'(lambda(x) (if (equal x #\Space) #\- x))
			     (coerce str 'list)) 'string)))
    (read (make-string-input-stream 
	   (concatenate 'string ":choose-" tmp)))))

(defun str-to-mes (str)
  "Method args: str
Returns a message with the same name as the string with spaces
replaced by hyphens and : prepended to the string."
  (let ((tmp (coerce 
	      (mapcar #'(lambda(x) (if (equal x #\Space) #\- x))
		      (coerce str 'list)) 'string)))
    (read (make-string-input-stream 
	   (concatenate 'string ":" tmp)))))

(defmeth scatterplot-proto :add-modified-boxplot (y &key (x 1.0) 
                                              (width 1.0) (draw t))
  (unless (= 2 (send self :num-variables)) 
          (error "only works for 2D plots"))
  (let* ((half-box (* 0.4 width))
         (half-foot (* 0.1 width))
         (fiv (fivnum y))
         (bot (select fiv 0))
         (top (select fiv 4))
         (q1 (select fiv 1))
         (med (select fiv 2))
         (q3 (select fiv 3))
         (iqr (- q3 q1))
         (low (- q1 (* 1.5 iqr)))
         (high (+ q3 (* 1.5 iqr)))
         (outliers nil))
    (dolist (val y)
            (if (or (> val high) (<  val low))
                (setf outliers (append outliers (list val)))))
    (when outliers 
          (send self :add-points (list x x) (list bot top))
          (send self :add-points (repeat x (length outliers)) outliers)
          (send self :point-symbol (iseq (+ 2 (length outliers))) 
                'diamond))
    (send self :plotline (- x half-foot) low  (+ x half-foot) low  nil)
    (send self :plotline (- x half-foot) high (+ x half-foot) high nil)
    (send self :plotline x low x q1   nil)
    (send self :plotline x q3  x high nil)
    (send self :plotline (- x half-box) q1  (+ x half-box) q1  nil)
    (send self :plotline (- x half-box) med (+ x half-box) med nil)
    (send self :plotline (- x half-box) q3  (+ x half-box) q3  nil)
    (send self :plotline (- x half-box) q1  (- x half-box) q3  nil)
    (send self :plotline (+ x half-box) q1  (+ x half-box) q3  nil)))

(defun modified-boxplot (data &key (title "Modified Box Plot"))
"Args: (data &key (title \"Modified Box Plot\"))
DATA is a sequence, a list of sequences or a matrix. Makes a 
boxplot of the sequence or a parallel box plot of the sequences
in the list or the columns of the matrix."
  (let ((p (send scatterplot-proto :new 2 :title title :show nil 
		 :go-away nil)))
    (setq data
          (cond ((matrixp data) (column-list data))
                ((or (not (listp data)) (numberp (car data))) (list data))
                (t data)))
        (let ((range (get-nice-range (min data) (max data) 4)))
          (send p :range 1 (nth 0 range) (nth 1 range))
          (send p :y-axis t nil (nth 2 range)))
    (send p :range 0 0 (1+ (length data)))
    
    (dotimes (i (length data))
             (send p :add-modified-boxplot (nth i data) :x (1+ i)))
    (send p :show-window)
    p))

