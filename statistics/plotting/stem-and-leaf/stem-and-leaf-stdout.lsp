(defun stem-and-leaf-plot
  (the-list &key (stem-size 1) (leaf-size 1) (where *STANDARD-OUTPUT*))
  "Args: list stem-size leaf-size where
       list - a list of numbers or symbols which to plot
       stem-size - the optional number of characters to be printed to the left
                   of the stem
                   (default is 1)
       leaf-size - the optional number of characters in each leaf
                   (default is 1)
       where - an optional file name (defaults to stdout)"
  (labels ((check (x)
	    (every #'(lambda (x) (or (numberp x) (symbolp x))) x))

	  (to-string (x)
	    (cond ((numberp x) (format nil "~f" (float x)))
		  ((symbolp x) (symbol-name x))
		  (T (error "unimplement type for to-string"))))

	  (string-left (x n)
	    (do ((i 0 (+ i 1))
		 (result "" (concatenate 'string result (string (select x i)))))
		((>= i n) result)))
	  
	  (string-right-remainder (x n)
	    (do ((i n (+ i 1))
		 (result "" (concatenate 'string result (string (select x i))))
		 (string-length (length x))) ((>= i string-length) result)))

	  (pad-to-string (x n)
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

	  (list-max (x)
	    (do* ((remainder x (cdr remainder))
		  (maximum 0 maximum))
		 ((equal remainder nil) maximum)
		 (setf maximum (max maximum (car remainder)))))

	  (left-string-equal (x y n)
	    (do ((i 0 (+ i 1)))
		((or (>= i n) (char-not-equal (select x i) (select y i)))
		 (cond ((>= i n) T) (T nil)))))

	  (string-list-cat (x)
	    (do ((tmp "" (concatenate 'string tmp (car remainder)))
		 (remainder x (cdr remainder)))
		((equal remainder nil) tmp)))
	  
	  (print-plot (x f)
	    (do ((remainder x (cdr remainder)))
		((equal remainder nil) T)
		(princ (car (car remainder)) f)
		(princ " | " f)
		(princ (string-list-cat (cdr (car remainder))) f)
		(terpri f)))
	  
	  (parse-list (x stem-size leaf-size)
	    (let ((tmp nil) (stem nil) (remainder nil) (result nil))
	      (cond ((not (check x)) (error "unimplemented type")))
	      (setf tmp (mapcar #'to-string x))
	      (setf tmp (map-elements #'pad-to-string x
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
		  (setf tmp remainder)))))


	 (let ((stream-ptr nil))
	   (cond ((equal where *STANDARD-OUTPUT*)
		  (print-plot (parse-list the-list stem-size leaf-size) 
			      *STANDARD-OUTPUT*))
		 (T (with-open-file
		     (stream-ptr where :direction :output)
		     (print-plot (parse-list the-list 
					     stem-size
					     leaf-size) stream-ptr)))))))(defun stem-and-leaf-plot2
  (the-list1 the-list2 &key(stem-size 1)(leaf-size 1) (where *STANDARD-OUTPUT*))
  "Args: list stem-size leaf-size where
       list - a list of numbers or symbols which to plot
       list - a list of numbers or symbols which to plot
       stem-size - the optional number of characters to be printed to the left
                   of the stem
                   (default is 1)
       leaf-size - the optional number of characters in each leaf
                   (default is 1)
       where - an optional file name (defaults to stdout)"
  (labels (
           ;make certain the list we're passed has only numbers and symbols
           (check (x)
	    (or (every #'(lambda (x) (or (numberp x) (symbolp x))) x)
                (equal x nil)))

          ; convert every number and symbol to a string that can be broken
          ; further (later on) into a root & stem combination
	  (to-string (x)
	    (cond ((numberp x) (format nil "~f" (float x)))
		  ((symbolp x) (symbol-name x))
		  (T (error "unimplement type for to-string"))))

          ; make every string the same width
	  (pad-to-string (x n)
	    (cond ((symbolp x) (do ((i (length (symbol-name x)) (+ i 1))
				    (result (symbol-name x)
					    (concatenate 'string " " result)))
				   ((>= i n) result)))
		  ((numberp x) (do ((i (length (format nil "~f" (float x)))
				       (+ i 1))
				    (result (format nil "~f" (float x))
					    (concatenate 'string "0" result)))
				   ((>= i n) result)))
                  ((stringp x) (do ((i (length x) (+ i 1))
                                    (result x (concatenate 'string " " result)))
                                   ((>= i n) result)))
		  (T (error
		      "pad-to-string: unimplemented type for pad-to-string"))))

          ; extract the leftmost n characters from the string x
	  (string-left (x n)
	    (do ((i 0 (+ i 1))
		 (result "" (concatenate 'string result (string (select x i)))))
		((>= i n) result)))
	  
          ; return everything but the leftmost n characters from the string x
	  (string-right-remainder (x n)
	    (do ((i n (+ i 1))
		 (result "" (concatenate 'string result (string (select x i))))
		 (string-length (length x))) ((>= i string-length) result)))

	  (list-max (x)
	    (do* ((remainder x (cdr remainder))
		  (maximum 0 maximum))
		 ((equal remainder nil) maximum)
		 (setf maximum (max maximum (car remainder)))))

          ; return true if strings x and y have the same leftmost n characters
	  (left-string-equal (x y n)
	    (do ((i 0 (+ i 1)))
		((or (>= i n) (char-not-equal (select x i) (select y i)))
		 (cond ((>= i n) T) (T nil)))))

          (string-reverse (x)
                          (do ((tmp "")
                               (i (- (length x) 1) (- i 1)))
                              ((< i 0) tmp)
                              (setf tmp (concatenate 'string tmp (string (select x i))))))

	  (string-list-cat (x)
	    (do ((tmp "" (concatenate 'string tmp (car remainder)))
		 (remainder x (cdr remainder)))
		((equal remainder nil) tmp)))
	  
	  (print-plot (x y leaf-size f)
	    (do ((remainderx x)
                 (remaindery y)
                 (margin (* leaf-size (list-max (mapcar #'length (mapcar #'cdr x)))))
                )
		((and (equal remainderx nil)
                      (equal remaindery nil)) T)
                (cond ((or (equal remainderx nil)
                           (string> (caar remainderx) (caar remaindery)))
                       (princ (pad-to-string "" margin) f)
                       (princ " | " f)
		       (princ (caar remaindery) f)
		       (princ " | " f)
		       (princ (string-list-cat (cdar remaindery)) f)
                       (setf remaindery (cdr remaindery)))
                      ((or (equal remaindery nil)
                           (string< (caar remainderx) (caar remaindery)))
                       (princ (pad-to-string (string-reverse
                                            (string-list-cat (cdar remainderx)))
                                             margin) f)
                       (princ " | " f)
		       (princ (caar remainderx) f)
		       (princ " | " f)
                       (setf remainderx (cdr remainderx)))
		      ((string= (caar remainderx) (caar remaindery))
                       (princ (pad-to-string (string-reverse
                                            (string-list-cat (cdar remainderx)))
                                             margin) f)
                       (princ" | " f)
		       (princ (caar remainderx) f)
		       (princ " | " f)
		       (princ (string-list-cat (cdar remaindery)) f)
                       (setf remainderx (cdr remainderx))
                       (setf remaindery (cdr remaindery))))
		 (terpri f)))
	  
	  (parse-list (x stem-size leaf-size)
	    (let ((tmp nil) (stem nil) (remainder nil) (result nil))
	      (cond ((not (check x)) (error "unimplemented type")))
	      (setf tmp (mapcar #'to-string x))
	      (setf tmp (map-elements #'pad-to-string x
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
		  (setf tmp remainder)))))


	 (let ((stream-ptr nil))
	   (cond ((equal where *STANDARD-OUTPUT*)
		  (print-plot (parse-list the-list1 stem-size leaf-size)
                              (parse-list the-list2 stem-size leaf-size) 
                              stem-size
			      *STANDARD-OUTPUT*))
		 (T (with-open-file
		     (stream-ptr where :direction :output)
		     (print-plot (parse-list the-list1 
					     stem-size
					     leaf-size)
                                 (parse-list the-list2
                                             stem-size
                                             leaf-size)
                                 stem-size
                                 stream-ptr)))))))

