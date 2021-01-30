
;;;  this is file gnuplot.lsp, contains
;;; methods that teach  a graph-proto to create a gnuplot file 
;;  representing itself from its own internal data
;;; frederic udina june 1994, 1995
;;; starting from a file from Jan De Leew

#|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||

history

Jan Deleeuw wrote a first version including the gnuplot-dialog on 93 (?)
Frederic Udina included it in an object-oriented first version, Jun 3, 94
That version was posted in laplace.stat.ucla.edu:pub/xlisp/..contrib/udina

This is a remake, changing the name of the game from :gnu-plot 
to :to-gnuplot and adding some key-arguments to the main method
to allow calling it without user interaction.

Other changes include storage of default values in the propoerty list
of the global symbol *gnuplot* and use of a slot named filename to
store the name for temporary files to be used in the translation.


MINI-USER'S MANUAL (you should have got a file HQGinLISP-STAT.ps
 with this file. It contains a paper by Jan Deleeuw and Frederic Udina 
 about these lisp functions and methods)

The goal of gnuplot.lsp is to implement some methods in graph-proto for giving
it the capability of creating gnuplot files for representing its window 
in a high quality output devices.


To use this xlisp-stat program you need:

0) gnuplot installed in your computer.
1) XLISP-STAT running in a Mac or Unix X11 or MS-WINDOWS 
   (I have'nt tested other platforms)
2) An xlispstat object descendant of graph-proto showing some points and/or 
   some lines. The lines must be drawn by the standard method :add-lines,
   (histograms, for example, use diferent tricks).
3) load this file
4) send to your object the message :to-gnuplot
5) arrange your preferences in the dialog
6) load the resulting file in gnuplot
   (the one named <fname>.gnu where <fname> is the filename in use.

If in Unix and using (send my-obj :to-gnuplot :run-gnuplot t) step 6 will
be automatically done. So, if you put postscript as terminal, the
postscript file is automatically created.

Gnuplot is a freeware program for plotting functions and data. It is
available, at least for Unix-X11, Mac and DOS machines. It can be
found in ftp server laplace.stat.ucla.edu and in a lot of other
servers.  The more interesting feature of gnuplot used here is the
ability to translate its graphics to postscript format.

To create the temporary files and the final file, we use the 
:title slot from the object to be reproduced. Actually, a method called
:filename is responsible of obtaining the file name and stores it in 
a new slot :filename, allowing some user interaction, see the defmeth below.

Also warning: the file creation can erase already existing files.
For your knowledge, if you send :gnuplot and give the filenaming prefix
'name', the following files can be created:

name.gnu   is the main file to be loaded into gnuplot
nameNN.lin (with NN ranging from 00 to 99) contain the lines of the graph.
name.pnt   contains the point coordinate for the points in the graph.
name.ps    if postscript format is used in the dialog
name.hpl   when hplii format is used, etc.


TECHNICAL WARNING: The way the lines added to a graph object is no
 documented in the XLISP-STAT doc available. So we use some methods like 
 :linestart-next in a somewhat experimental basis. Our :to-gnuplot works 
 with all the objects we have tried so far, but no warranty...

For bugs, ideas, etc contact the author at
udina@upf.es

||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||#

(defmacro WHILE (cond &rest exprs)
  `(loop
   (unless ,cond (return))
    (progn ,@exprs)))

;;;to store and manage defaults we will use the property mechanism of lisp
; mainly to avoid use of so many global vars, and use of blind lists
;examples of use:
;(putprop '*gnuplot* "x11" 'terminal)
;(putprop '*gnuplot* nil 'key)
;(putprop '*gnuplot* t 'border)
;(get '*gnuplot* 'terminal)
;(symbol-plist '*gnuplot*) returns the whole list of pairs
;(putprop '*gnuplot* "vga" 'terminal)
;

;;;set *gnuplot* defaults
;this defaults will be used for setting the dialog
;and will be updated from the output of the dilaog

#+unix (putprop '*gnuplot* "x11" 'terminal)
#+macintosh (putprop '*gnuplot* "mac" 'terminal)
#+msdos (putprop '*gnuplot* "vga" 'terminal)

(putprop '*gnuplot* "" 'terminal)

#+unix (putprop '*gnuplot* t 'run-it?)
#+macintosh (putprop '*gnuplot* nil 'run-it?)
#+msdos (putprop '*gnuplot* nil 'run-it?)

(putprop '*gnuplot* "dots" 'style)
(putprop '*gnuplot* "" 'x-label)
(putprop '*gnuplot* "" 'y-label)
(putprop '*gnuplot* "" 'plot-title)
(putprop '*gnuplot* 1 'x-size)
(putprop '*gnuplot* 1 'y-size)
(putprop '*gnuplot* nil 'key)
(putprop '*gnuplot* nil 'data-labels)
(putprop '*gnuplot* t 'border)
(putprop '*gnuplot* nil 'grid)
(putprop '*gnuplot* t 'axis)

;;;this decides the default, 'auto or 'ask
;when 'auto, try to get a filename from the graph object title
;when 'ask, always ask the user about the filename
(putprop '*gnuplot* 'auto 'filenaming)
;(putprop '*gnuplot* 'ask 'filenaming)
#+msdos (putprop '*gnuplot* 'ask 'filenaming)


(defmeth graph-proto :filename (&key (num nil numset) (ask nil))
  "returns a default filename to use based on slot 'title
 but replacing non alfaanum chars with _ 
 Keys: If :NUM is given, appends it to the name, format 2 digits
       if :ASK is t, the user will be asked to confirm or modify
       the filename."
  (unless (send self :has-slot 'filename)
	  (send self :add-slot 'filename nil))
  (if numset
      (let ((strnum (num-to-string num)))
	(if (< (length strnum) 2)
	    (setf strnum (concatenate 'string (send self :filename) 
				      "0" strnum))
	  (setf strnum (concatenate 'string (send self :filename) 
				    strnum))))
    (let* ((default (let* ((stringa (send self :title))
			   (newstr (coerce (repeat #\_ (length stringa)) 'string))
			   chari inti)
		      (dotimes (i (length stringa))
			       (setf inti (char-int (setf chari (elt stringa i))))
			       (when (or (char<= #\a chari #\z)
					 (char<= #\A chari #\Z)
					 (char<= #\0 chari #\9))
				     (setf (elt newstr i) chari)))
		      newstr))
	   #+macintosh (prmpt "Give a prefix for filenaming (<= 19 char)")
	   #+unix (prmpt "Give a prefix for filenaming (<= 24 char)")
	   #+msdos (prmpt "Give a prefix for filenaming (<= 6 char)")
	   (current default))
      (if ask
	  (progn
	    (setf current (get-string-dialog  prmpt :initial default))
	    (unless current (setf current default))
	    (print (list "gorra" current)))
	(if (slot-value 'filename)
	    (setq current (slot-value 'filename))
	  (setq current (get-a-valid-gp-filename default))))
      (send self :slot-value 'filename current)
      current)))

(defmeth graph-proto :to-gnuplot (&key (use-dialog t) 
				       (run-gnuplot (get '*gnuplot* 'run-it?))
				       (temp-files-dir "")
				       (output-file "" fileset)
				       (terminal (get '*gnuplot* 'terminal))
				       (style    (get '*gnuplot* 'style))
				       (x-label  (get '*gnuplot* 'x-label))
				       (y-label  (get '*gnuplot* 'y-label))
				       (plot-title (get '*gnuplot* 'plot-title))
				       (x-size   (get '*gnuplot* 'x-size))
				       (y-size   (get '*gnuplot* 'y-size))
				       (key      (get '*gnuplot* 'key))
				       (data-labels
					         (get '*gnuplot* 'data-labels))
				       (border   (get '*gnuplot* 'border))
				       (grid     (get '*gnuplot* 'grid))
				       (axis     (get '*gnuplot* 'axis))
				       (extra-gnuplot-commands nil))
 "Args: (&key (use-dialog t) (run-gnuplot nil) ... )
 constructs a gnuplot file representing self
 from the info the object contains. On UNIX systems, it also runs 
 gnuplot if given the key option ':run-gnuplot t'.
 Default is to display a dialog for choice of gnuplot options.
 This can be overriden by ':use-dialog nil'"
 (let* ((flprefix (case (get '*gnuplot* 'filenaming)
			('auto (send self :filename))
			('ask (send self :filename :ask t))
			(nil (send self :filename :ask t))))
	 (flnm (concatenate 'string temp-files-dir flprefix))
	 (np (send self :num-points))
	 x y
	 (do-it t)
	 (fnpoints (concatenate 'string flnm ".pnt"))
	 (fngnu (concatenate 'string flnm ".gnu")))
    (when
     use-dialog
     (setq do-it (gnuplot-dialogue))
     (when do-it
	  ;must reread all props from *gnuplot*
	   (setq terminal (get '*gnuplot* 'terminal))
	   (setq style    (get '*gnuplot* 'style))
	   (setq x-label  (get '*gnuplot* 'x-label))
	   (setq y-label  (get '*gnuplot* 'y-label))
	   (setq plot-title (get '*gnuplot* 'plot-title))
	   (setq x-size   (get '*gnuplot* 'x-size))
	   (setq y-size   (get '*gnuplot* 'y-size))
	   (setq key      (get '*gnuplot* 'key))
	   (setq run-gnuplot      (get '*gnuplot* 'run-it?))
	   (setq data-labels (get '*gnuplot* 'data-labels))
	   (setq border   (get '*gnuplot* 'border))
	   (setq grid     (get '*gnuplot* 'grid))
	   (setq axis     (get '*gnuplot* 'axis))))
    (when
     do-it
     (with-open-file
      (f fngnu :direction :output)
      (format *standard-output* "~%Writing gnuplot file ~s" fngnu)
      (unless (string= terminal "")
	      (format f "~%set term ~a" terminal))
      (if (or (string= terminal "postscript")
	      (string= terminal "post portrait"))
	  (format f "~%set output ~s" 
		  (concatenate 'string temp-files-dir
			       (if fileset output-file flnm) ".ps"))
	(if (string= terminal "hpljii")
	    (format f "~%set output ~a" 
		    (concatenate 'string temp-files-dir
				 (if fileset output-file flnm) ".hpl"))
	  (if (not (string= terminal ""))
	      (format f "~%set output"))))
      (format f "~%set ~agrid" (if grid "" "no"))
      (format f "~%set ~akey" (if key "" "no"))	
      (format f "~%set ~aborder" (if border "" "no"))	
      (format f "~%set ~azeroaxis" (if axis "" "no"))
      (format f "~%set title ~s" plot-title)
      (format f "~%set xlabel ~s" x-label)
      (format f "~%set ylabel ~s" y-label)
      (format f "~%set data style ~a" style)
      (format f "~%set size ~f, ~f" x-size y-size)
      ;;now write (or erase) labels
      (format f "~%set nolabel")
      (when extra-gnuplot-commands
	    (format f "~%~a" extra-gnuplot-commands))
      (if (and data-labels
	       (> np 0))
	  (let* ((xs (send self :point-coordinate 0 (iseq np)))
		 (ys (send self :point-coordinate 1 (iseq np)))
		 (labs (send self :point-label (iseq np))))
	    (dotimes (i np)
		     (format f "~%set label ~s at ~f, ~f"
			     (elt labs i) (elt xs i) (elt ys i)))))
      (unless (= 0 (+ np
		      (send self :num-lines)))
	      (if (> (send self :num-lines) 0)
		  (setf stringa (send self :write-lines-to-gnu temp-files-dir))
		(setf stringa ""))
	      (format f "~%plot ")
	      (if (> np 0)
		  (progn
		    (setf x (send self :point-coordinate 0 (iseq np)))
		    (setf y (send self :point-coordinate 1 (iseq np)))
		    (send self :write-points-to-gnu fnpoints)
		    (format f " ~s" fnpoints))
		(when (> (length stringa) 1)
		      (setf stringa (subseq stringa 1))))
	      (when (> (send self :num-lines) 0)
		    (format f stringa)))
      (if (string= terminal "postscript")
	  (format f "~%quit")
	(if (string= terminal "X11")
	    (format f "~%pause -1")
	  (if (string= terminal "x11")
	      (format f "~%pause -1")
	    (if (string= terminal "hpljii")
		(format f "~%quit")
	      (if (string= terminal "tek40xx")
		  (format f "~%pause -1")
		(if (string= terminal "mac")
		    (format f "~%")))))))
      (format f "~%")
      (format *standard-output* "~%")
      )
     (when run-gnuplot
	   ;;(and (featurep 'UNIX)
	   (format *standard-output*
		   "~%now running gnuplot on file ~s~%" fngnu)
	   (system (concatenate 'string "gnuplot " fngnu ))))));"&"

(defmeth graph-proto :write-points-to-gnu (&optional (filnm nil))
"this is method is used by :to-gnuplot method, it is not intended
 to be used by itself"
  (let* ((fn (if filnm filnm 
	       (concatenate 'string (send self :filename) ".pnt")))
	 (n (send self :num-points))
	 (x (send self :point-coordinate 0 (iseq n)))
	 (y (send self :point-coordinate 1 (iseq n))))
    (with-open-file
     (g  fn  :direction :output)
     (format *standard-output* "~%writing points to file ~s" fn)
     (dotimes (i n)
	      (format g "~f~a~f~%" (elt x i) #\Space (elt y i))))))

(defmeth graph-proto :get-one-line (start)
"this method is used by :to-gnuplot method, it is not intended
 to be used by itself"
  (let* ((next start)
	 (xcoords (list (send self :linestart-coordinate 0 start)))
	 (ycoords (list (send self :linestart-coordinate 1 start))))
    (WHILE (setq next (send self :linestart-next next))
      (setq xcoords (cons (send self :linestart-coordinate 0 next) xcoords))
      (setq ycoords (cons (send self :linestart-coordinate 1 next) ycoords)))
    (list xcoords ycoords)))

(defmeth graph-proto :get-lines ()
"this method is used by :to-gnuplot method, it is not intended
 to be used by itself. Returns the lines of self in a list of lists format."
  (let ((first 0)
	(last (send self :num-lines))
	aline 
	(resul nil))
    (WHILE (< first last)
	   (setf aline (send self :get-one-line first))
	   (setq resul (cons  aline resul))
	   (setq first (+  first (length (first aline)))))
    resul))
	

(defmeth graph-proto :write-lines-to-gnu (&optional (directory ""))
"this method is used by :to-gnuplot method, it is not intended
 to be used by itself.
 creates files filenameNN.lin for gnuplot ploting the lines of self
 Returns a string for gnuplot to load the files created"
(let* ((lines (arrange-lines-for-gnu (send self :get-lines)))
       (num-file 0)
       filen
       (to-load "")
       num-cols)
  (if (> (length lines) 99)
      (message-dialog 
       "This object has too much lines! ~%(more than 99) Trash it.")
    (dotimes
     (i (length lines))
     (setf filen (concatenate 'string 
			       directory
			      (send self :filename :num num-file) ".lin"))
     (write-lines-one-gnu-file (nth i lines) filen)
     (setf num-cols (length (first (nth i lines))))
     (WHILE (> num-cols 1)
	    (setf to-load
		  (concatenate 'string
			       (format nil ", ~s using 1:~a with lines" 
				       filen num-cols)
				to-load))
	    (setf num-cols (1- num-cols)))
     (setf num-file (1+ num-file))))
  to-load))

(defun gnuplot-dialogue ()
"Creates and display a dialog for the user choosing the desired 
 gnuplot options to be passed to method :to-gnuplot.
 Resulting options are stored as props of the symbol *gnuplot*"
(let* ((cancel (send modal-button-proto :new "Cancel"))
       (title (send edit-text-item-proto :new
		    (get '*gnuplot* 'plot-title) :text-length 20))
       (key (send toggle-item-proto :new "Key"
		  :value (get '*gnuplot* 'key)))
       (run-it (send toggle-item-proto :new "Run Gnuplot now?"
		  :value (get '*gnuplot* 'run-it?)))
       (labels (send toggle-item-proto :new "Labels"
		     :value (get '*gnuplot* 'data-labels)))
       (border (send toggle-item-proto :new "Border"
		     :value (get '*gnuplot* 'border)))
       (grid (send toggle-item-proto :new "Grid"
		   :value (get '*gnuplot* 'grid)))
       (axis (send toggle-item-proto :new "Axis"
		   :value (get '*gnuplot* 'axis)))
;aixo no va be aixi
       (str-format (list "" "postscript" "post portrait"
			 "x11" "hpjii" "tek4010" "mac"))
       (lab-format (list "Default" "Postscript landscape" "Postscript portrait" 
			 "X11" "HPLJ" "tek40xx" "Mac"))
       (format (send choice-item-proto :new lab-format
		     :value (position (get '*gnuplot* 'terminal)
				      str-format :test #'string=)))
       (str-styles (list "lines" "points" "linespoints" "impulses"
			 "dots" "errorbars"))
       (style (send choice-item-proto :new str-styles
		    :value  (position (get '*gnuplot* 'style)
				      str-styles :test #'string=)))
       (x-label (send edit-text-item-proto :new
		      (get '*gnuplot* 'x-label) :text-length 10))
       (y-label (send edit-text-item-proto :new
		      (get '*gnuplot* 'y-label) :text-length 10))
       (x-size (send text-item-proto :new "0.0" :text-length 4))
       (y-size (send text-item-proto :new "0.0" :text-length 4))
       (x-size-scroll (send interval-scroll-item-proto 
			    :new (list 0 1) :text-item x-size))
       (y-size-scroll (send interval-scroll-item-proto 
			    :new (list 0 1) :text-item y-size))
       (ok (send modal-button-proto :new "OK"
		 :action
		 #'(lambda ()
		     (putprop '*gnuplot* (send key :value) 'key) 
		     (putprop '*gnuplot* (send run-it :value) 'run-it?) 
		     (putprop '*gnuplot* (send labels :value) 'data-labels)
		     (putprop '*gnuplot* (send border :value) 'border)
		     (putprop '*gnuplot* (send grid :value) 'grid)
		     (putprop '*gnuplot* (send axis :value) 'axis)
		     (putprop '*gnuplot* (nth (send format :value)
					      str-format)
			      'terminal)
		     (putprop '*gnuplot* (nth (send style :value)
					      str-styles)
			      'style)
		     (putprop '*gnuplot* (send x-size-scroll :value) 'x-size)
		     (putprop '*gnuplot* (send y-size-scroll :value) 'y-size)
		     (putprop '*gnuplot* (send x-label :text) 'x-label)
		     (putprop '*gnuplot* (send y-label :text) 'y-label)
		     (putprop '*gnuplot* (send title :text) 'plot-title)
		     t)))
       (plot-dialog (send modal-dialog-proto :new
			  (list (list  (send text-item-proto :new "Title: ")
				       title
				       run-it)
				(list
				 (list (send text-item-proto :new "Options")
				       key labels border grid axis)
				 (list  (send text-item-proto :new "Output Format")
					format)
				 (list  (send text-item-proto :new 
					      "Style for points:")
					style))
				(list (send text-item-proto :new "X-Label:")
				      x-label
				(send text-item-proto :new "Y-Label:")
				      y-label)
				(list  (send text-item-proto :new "X-Size:")
				       x-size x-size-scroll)
				(list (send text-item-proto :new "Y-Size:")
				      y-size y-size-scroll)
				(list ok cancel)))))
  (send y-size-scroll :value (get '*gnuplot* 'x-size))
  (send x-size-scroll :value (get '*gnuplot* 'y-size))
  (send plot-dialog :modal-dialog)))

(defun lines-with-same-x-p (lin1 lin2)
"Args: line1 line2
 A line is a list of x's and y's, this function returns T if the x's are equal"
  (and (= (length (first lin1)) (length (first lin2)))
       (notany #'null (map 'list #'= (first lin1) (first lin2)))))

(defun arrange-lines-for-gnu (lines)
"given LINES, a list of lines, each one a list of x's and y's,
 returns a list of multi-lines, each one a list of x y1 y2 .. yn (n>0)"
(let* ((rest lines)
       output
       first current good bad)
  (WHILE (> (length rest) 0)
	 (setf first (first rest))
	 (setf current (cdr rest))
	 (setf good first)
	 (setf bad nil)
	 (WHILE current ;scan all lines searching for lines that match first
		(if (lines-with-same-x-p first (first current))
		    (setf good 
			  (reverse (cons (second (first current))
					 (reverse good))))
		  (setf bad (cons (first current) bad)))
		(setf current (cdr current)))
	 (setf output (cons (transpose good) output))
	 (setf rest bad))
  output))

(defun write-lines-one-gnu-file (line filename)
"LINE is a list of list, each one is x y1 .. yn (n>0)
 FILENAME must be given also."
(with-open-file 
 (f filename :direction :output)
 (format *STANDARD-OUTPUT* "~%writing lines to file ~s" filename)
 (dolist (item line)
	 (format f "~f " (first item))
	 (dotimes (i (1- (length item)))
		  (format f "~f " (nth (1+ i) item)))
	 (format f "~%"))))


;;we deal here with problems with filename

(defun featurep (sym)
  (member sym *features*))


(defun valid-gp-filename (stringa)
  (when stringa
	(if (featurep 'macintosh)
	    (< (length stringa) 20)
	  (if (featurep 'msdos)
	      (< (length stringa) 7)
	    (if (featurep 'unix)
		(< (length stringa) 25)
	      nil)))))

(defun get-a-valid-gp-filename (str)
"gives the user a chance to change the filename"
(if (valid-gp-filename str)
    str
  (let* (
	 #+macintosh (prmpt "Give a prefix for filenaming (max 19 char)")
	 #+unix (prmpt "Give a prefix for filenaming (max 24 char)")
	 #+msdos (prmpt "Give a prefix for filenaming (max 6 char)")
	 (stru (get-string-dialog  prmpt :initial str)))
    stru)))

;;;;;;;;;;for testing 
(defun testit ()
  (setq plotgnu (plot-points (uniform-rand 5) (uniform-rand 5)))
  (send plotgnu :add-lines (list (uniform-rand 5) (uniform-rand 5)))
  (send plotgnu :add-lines (list (uniform-rand 5) (uniform-rand 5)))
  (send plotgnu :add-lines (list (uniform-rand 5) (uniform-rand 5)))
  (send (send plotgnu :menu)
	:append-items
	(send menu-item-proto :new "to-gnuplot"
		    :action (lambda () (send plotgnu :to-gnuplot))))
  (format t "~%stored in 'plotgnu'~%"))


;;;;;;;;;;;;;provide
(provide "gnuplot")
