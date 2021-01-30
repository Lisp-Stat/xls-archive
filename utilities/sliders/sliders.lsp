;;;;
;;;;  sliders.lsp                    B. Narasimhan
;;;;
;;;;  This is a tool that can help a person design sliders in a
;;;;  jiffy.  (I found myself creating sliders so often that I
;;;;  decided to automate the process.)  It writes the code
;;;;  for creating sliders for inclusion in your program as is.

(provide "sliders")
(require "statistics")
(require "graphics")
(require "help")

(defun make-sliders (&optional specs &key cols fname)
  (let* (
	 (n (if specs
		(length specs)
	      (select (get-value-dialog "Enter # of sliders required"
					:initial 1) 0)))
	 (c (if cols
		cols
	      1))
	 (r (floor (/ n c)))
	 (fh (if fname
		 (open fname :direction :output)
	       (open (get-string-dialog "Output file name for code?")
		     :direction :output))))
    (format fh "(let* (~%")
    (dotimes (j n)
	     (let* ((alist (if specs
			       (select specs j)
			     nil))
		    (id (format nil "Slider ~d" j))
		    (name (if alist
			      (select alist 0)
			    (get-string-dialog
			     (format nil "Title for ~s" id))))
		    (obj (if alist
			     (select alist 1)
			   (select
			    (get-value-dialog
			     (format nil
				     "Sequence or Interval for ~s?" id)) 0)))
		    (action (if alist
				(select alist 2)
			      (get-string-dialog
			       (format nil "Action for ~s" id))))
		    (prot-name (if (< (length obj) 3)
				   "interval-scroll-item-proto"
				 "sequence-scroll-item-proto")))
	       (format fh "       (to~d (send text-item-proto :new~%" j)
	       (format fh "                   ~s))~%" name)
	       (format fh "       (vo~d (send text-item-proto :new~%" j)
	       (format fh "                   \"\" :text-length 10))~%")
	       (format fh "       (so~d (send ~g :new~%" j prot-name)
	       (format fh "                   '~g~%" obj)
	       (format fh "                   :text-item vo~d~%" j)
	       (format fh "                   :action~%")
	       (if (eql j (- n 1))
		   (format fh "                      #'~g)))~%" action)
		 (format fh "                      #'~g))~%" action))))
    (format fh "  (send dialog-proto :new~%")
    (format fh "        (list~%")
    (dotimes (j r)
	     (let* ((tmp (iseq (* j c) (- (* (+ j 1) c) 1)))
		    (s1 (format nil "~g"
				(combine
				 (map 'list #'(lambda(x)
						(list (format nil "to~d" x)
						      (format nil "vo~d" x)))
				      tmp))))
		    (os1 (string-left-trim "("
					   (string-right-trim ")" s1)))
		    (s2 (if (eql c 1)
			    (format nil "so~g" (select tmp 0))
			  (format nil "~g"
				  (map 'list
				       #'(lambda(x)
					   (format nil "so~d" x))
				       tmp))))
		    (os2 (string-left-trim "("
					   (string-right-trim ")" s2))))
	       (format fh "          (list ~g)~%" os1)
	       (if (<  c 2)
		   (if (eql (* (+ j 1) c) n)
		       (format fh "          ~g)))~%" os2)
		     (format fh "          ~g~%" os2))
		 (format fh "          (list ~g)~%" os2))))
    (when (not (eql (* r c) n))
	  (let* ((tmp (iseq (* r c) (- n 1)))
		 (len (length tmp))
		 (s1 (format nil "~g"
			     (combine (map 'list
					   #'(lambda(x)
					       (list (format nil "to~d" x)
						     (format nil "vo~d" x)))
					   tmp))))
		 (os1 (string-left-trim "("
					(string-right-trim ")" s1)))
		 (s2 (if  (eql (length tmp) 1)
			 (format nil "so~g" (select tmp 0))
		       (format nil "~g"
			       (map 'list
				    #'(lambda(x) (format nil "so~d" x))
				    tmp))))
		 (os2 (string-left-trim "("
					(string-right-trim ")" s2))))

	    (format fh "          (list ~g)~%" os1)
	    (if (< (length tmp) 2)
		(format fh "          ~g)))~%" os2)
	      (format fh "          (list ~g))))~%" os2))))
    (close fh)))

