;;; dmc.lsp, A pedagogical tool for demonstrating Markov Chains.
;;; Copyright (C) 1993 B. Narasimhan & B. I. MacAlpine
;;;
;;;    This program is free software; you can redistribute it and/or modify
;;;    it under the terms of the GNU General Public License as published by
;;;    the Free Software Foundation; either version 2 of the License, or
;;;    (at your option) any later version.
;;;
;;;    This program is distributed in the hope that it will be useful,
;;;    but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;;    GNU General Public License for more details.
;;;
;;;    You should have received a copy of the GNU General Public License
;;;    along with this program; if not, write to the Free Software
;;;    Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
;;;
;;; For complete details on this program, see 
;;; the technical report 
;;; "Discrete Finite State Markov Chains in Lisp-Stat"
;;; by B. Narasimhan and Brett I. MacAlpine.
;;;         
;;; Perhaps each instance of the object should store its 
;;; own options.
;;; That would allow a person to have multiple Markov 
;;; chains in a single session, with different colors.
;;; But that is quite trivial to implement, therefore, 
;;; I am not worried now.

(require "dmc-opts")
(require "buttons")
(require "stochast")
(require "states")
(provide "dmc")

;; The Markov Chain Object.

(defproto dmc-proto '(initial-state state-objs button-objs
				    current-state time history
				    sample-paths)
  nil graph-window-proto)

(defmeth dmc-proto :isnew (transition-matrix initial 
					     &optional title)
  "Method args: (transition-matrix initial &optional title)
  Returns an instance of the dmc-proto object 
  given a transition matrix.  Initial state is initial. Title
  is used for title if given."
  (unless (stochastic-matrixp transition-matrix)
	  (error 
	   "Transition matrix is not a stochastic matrix!"))
  (setf (slot-value 'time) 0)
  (if title
      (send self :title title)
    (send self :title "Discrete Markov Chain"))
  (setf (slot-value 'initial-state) initial)
  (setf (slot-value 'current-state) initial)
  
  (let* ((n (select (array-dimensions transition-matrix) 0))
	 (rows (row-list transition-matrix))
	 (tmp nil))
    (dotimes (j n)
	     (setf tmp (append tmp 
			       (list 
				(send state-proto :new 
				      (format nil "~d" j) j
				      (select rows j))))))
    (setf (slot-value 'state-objs) tmp))
  
  (let* ((n (send self :no-of-states))
	 (ta (send self :text-ascent))
	 (td (send self :text-descent))
	 (col-sep (send self :text-width "mm"))
	 (row-sep (+ ta td))
	 (bh (round (* 1.5 (+ ta td))))
	 (bw1 (send self :text-width " Reset "))
	 (bw2 (send self :text-width " run-dmc "))
	 (bw2a (send self :text-width " # "))
	 (bw3 (send self :text-width  " Time: mmmmmmmmmm "))
	 (state-width (send self :text-width 
			    (format nil "m~dm" n)))
	 (c (floor (sqrt n)))
	 (r (floor (/ n c)))
	 (wx (+ (* 4 col-sep) bw1 bw2 bw2a bw3))
	 (cols-width (+ (* c (+ col-sep state-width)) 
			col-sep)))
    (when (< cols-width wx)
	  (setf c (floor (/ (- wx col-sep) 
			    (+ col-sep state-width))))
	  (setf r (floor (/ n c))))
    (setf wx (max wx cols-width))
    
;; Place reset , run-dmc and time buttons.

    (setf (slot-value 'button-objs)
	  (list 
	   (send button-obj-proto :new 
		 "Reset" 
		 (list col-sep row-sep bw1 bh)
		 #'(lambda () (send self :reset)))
	   (send button-obj-proto :new 
		 "run-dmc" 
		 (list (+ col-sep bw1 col-sep) row-sep bw2 bh)
		 #'(lambda () (send self :run-dmc)))
	   (send button-obj-proto :new 
		 " # " 
		 (list (+ col-sep bw1 col-sep bw2) 
		       row-sep bw2a bh)
		 #'(lambda () 
		     (let ((k (get-value-dialog 
			       "Number of steps to run?" 
			       :initial 1)))
		       (if k 
			   (send self :run-dmc 
				 (select k 0))))))
	   (send button-obj-proto :new 
		 (format nil "Time: ~d" (slot-value 'time))
		 (list (- wx col-sep bw3) row-sep bw3 bh)
		 #'(lambda ()))))

    (let* ((state-height (round (* 3 (+ ta td))))
	   (controls-height (+ row-sep 
			       (round (* 1.5 (+ ta td)))))
	   (wy (+ (if (eql (* c r) n)
		      (* r (+ row-sep state-height))
		    (* (+ r 1) 
		       (+ row-sep state-height))) row-sep))
	   (tmp (slot-value 'state-objs)))
      (send self :size wx (+ wy controls-height))
      (call-next-method)
      (send self :delete-method :do-motion)
    
      (dotimes (i n)
	       (let* ((y (+ 
			  (* (floor (/ i c)) 
			     (+ state-height row-sep)) 
			  row-sep))
		      (x (+ (* (mod i c) 
			       (+ state-width col-sep)) 
			    col-sep)))
		 (send (select tmp i) 
		       :loc-and-size 
		       (list x (+ y controls-height) 
			     state-width state-height))))))
  (send (select (slot-value 'state-objs) initial)
	:you-are-current-state self)
  (send self :add-some-menu-items))


(defmeth dmc-proto :current-state-obj ()
  (select (slot-value 'state-objs) 
	  (slot-value 'current-state)))

(defmeth dmc-proto :resize ()
  "Method args: ()
   Resizes the window and adjusts the layout."
  (let* ((but-objs (slot-value 'button-objs))
	 (wsize (send self :size))
	 (n (send self :no-of-states))
	 (ta (send self :text-ascent))
	 (td (send self :text-descent))
	 (col-sep (send self :text-width "mm"))
	 (row-sep (+ ta td))
	 (bh (round (* 1.5 (+ ta td))))
	 (bw1 (send self :text-width " Reset "))
	 (bw2 (send self :text-width " run-dmc "))
	 (bw2a (send self :text-width " # "))
	 (bw3 (send self :text-width  " Time: mmmmmmmmmm "))
	 (state-width (send self :text-width 
			    (format nil "m~dm" n)))
	 (wx (max (first wsize) (+ (* 4 col-sep) 
				   bw1 bw2 bw2a bw3)))
	 (c (floor (/ (- wx col-sep) 
		      (+ col-sep state-width))))
	 (r (floor (/ n c))))

    (send (select but-objs 0) :loc-and-size 
	  (list col-sep row-sep bw1 bh))
    (send (select but-objs 1) :loc-and-size 
	  (list (+ col-sep bw1 col-sep) row-sep bw2 bh))
    (send (select but-objs 2) :loc-and-size
	  (list (+ col-sep bw1 col-sep bw2) row-sep bw2a bh))
    (send (select but-objs 3) :loc-and-size
	  (list (- wx col-sep bw3) row-sep bw3 bh))
 
    (let* ((state-height (round (* 3 (+ ta td))))
	   (controls-height (+ row-sep 
			       (round (* 1.5 (+ ta td)))))
	   (wy (+ (if (eql (* c r) n)
		      (* r (+ row-sep state-height))
		    (* (+ r 1) (+ row-sep state-height))) 
		  row-sep))
	   (tmp (slot-value 'state-objs)))
      (send self :size wx (+ wy controls-height))
      (dotimes (i n)
	       (let* ((y (+ 
			  (* (floor (/ i c)) 
			     (+ state-height row-sep)) 
			  row-sep))
		      (x (+ (* (mod i c) 
			       (+ state-width col-sep)) 
			    col-sep)))
		 (send (select tmp i) 
		       :loc-and-size 
		       (list x (+ y controls-height) 
			     state-width state-height)))))))

(defmeth dmc-proto :do-click (x y m1 m2)
  "Method args: (x y m1 m2)
   Takes the appropriate action when the mouse is clicked on
   window."
  (catch 'done
    (dolist (obj (slot-value 'button-objs))
	    (when (send obj :my-click x y m1 m2)
		  (send obj :do-click)
		  (throw 'done nil)))
    (dolist (obj (slot-value 'state-objs))
	    (when (send obj :my-click x y m1 m2)
		  (send obj :do-click self)
		  (throw 'done nil)))))

(defmeth dmc-proto :no-of-states ()
  "Method args: ()
   Returns the number of states in the Markov Chain."
  (length (slot-value 'state-objs)))

(defmeth dmc-proto :print-transition-matrix ()
  "Method args: ()
   Prints the transition matrix in a nice form."
  (print-matrix (send self :transition-matrix)))

(defmeth dmc-proto :transition-matrix ()
  "Method args: ()
   Returns the transition matrix."
  (let ((n (send self :no-of-states))
	(tmp nil))
    (dolist (obj (slot-value 'state-objs))
	    (setf tmp 
		  (append tmp 
			  (coerce 
			   (send obj :prob-vec) 'list))))
    (matrix (list n n) (coerce tmp 'list))))

(defmeth dmc-proto :run-dmc (&optional k)
  "Method args: (&optional k)
   Runs the Markov Chain k steps, or, if k is omitted,
   one step."
  (if k
      (dotimes (j k)
	       (send self :goto-next-state)
	       (pause *run-step-delay*))
    (send self :goto-next-state)))

(defmeth dmc-proto :run-dmc-silently (k)
  "Method args: (&optional k)
   Runs the Markov Chain k steps, silently."
  (let* ((state-objs (slot-value 'state-objs))
	 (cs (slot-value 'current-state))
	 (obj (select state-objs cs))
	 (next-obj nil))
    (case (slot-value 'history)
	 (nil (dotimes (j k)
		       (setf (slot-value 'time)
			     (+ (slot-value 'time) 1))
		       (setf next-obj 
			     (select state-objs 
				     (send obj :next-state)))
		       (send next-obj :slot-value 
			     'no-of-visits 
			     (+ 
			      (send next-obj 
				    :slot-value 'no-of-visits)
			      1))
		       (setf obj next-obj)))
	 (t (dotimes (j k)
		     (setf (slot-value 'time)
			   (+ (slot-value 'time) 1))
		     (format *history-file-handle* "~d~%" 
			     (send obj :next-state))
		     (setf next-obj 
			   (select state-objs 
				   (send obj :next-state)))
		     (send next-obj :slot-value 'no-of-visits 
			   (+ 
			    (send next-obj 
				  :slot-value 'no-of-visits)
			    1))
		     (setf obj next-obj))))
    (setf (slot-value 'current-state) 
	  (send obj :slot-value 'state-no)))
;; Handle the Clock button in a special way.
  (send (select (slot-value 'button-objs) 3)
	:title (format nil "Time: ~d" (slot-value 'time)))
  (send self :redraw))

(defmeth dmc-proto :goto-next-state ()
  "Method args: ()
   Sends the markov chain to the next state from 
   current state."
  (let* ((time (slot-value 'time))
	 (state-objs (slot-value 'state-objs))
	 (cs (slot-value 'current-state))
	 (obj (select state-objs cs))
	 (next-state (send obj :next-state))
	 (next-obj (select state-objs next-state))
	 (w (slot-value 'sample-paths)))
    (when w 
	  (send w :add-lines 
		(list (list time 
			    (+ time 1)) (list cs next-state)))
	  (if (> *time-window* 0)
	      (send w :range 0 
		    (max 0 (- time *time-window* -1)) 
		    (max 50 (+ time 1)))
	    (send w :adjust-to-data)))
	  
    (setf (slot-value 'current-state) next-state)
    (send self :tick-time)

    (send obj :you-are-not-current-state self)
    (send next-obj :you-are-current-state self))
  (when (slot-value 'history)
	(format *history-file-handle* "~d~%" 
		(slot-value 'current-state))))

(defmeth dmc-proto :time (&optional value)
  "Method args: (&optional value)
   Sets and retrieves time."
  (if value
      (let ((obj (select (slot-value 'button-objs) 3)))
	(setf (slot-value 'time) 0)
	(send obj :title 
	      (format nil "Time: ~d" (slot-value 'time)))
	(send obj :redraw self))
    (slot-value 'time)))

(defmeth dmc-proto :tick-time ()
  "Method args: ()
   Ticks time once and shows new time on clock button."
  (setf (slot-value 'time) (+ (slot-value 'time) 1))
;; Handle the Clock button in a special way.
  (let ((obj (select (slot-value 'button-objs) 3)))
    (send obj :title 
	  (format nil "Time: ~d" (slot-value 'time)))
    (send obj :redraw self)))

(defmeth dmc-proto :current-state ()
  "Method args: ()
   Returns the current state."
  (slot-value 'current-state))

(defmeth dmc-proto :describe ()
  "Method args: ()
   Prints some statistics."
  (let ((time (slot-value 'time)))
    (format t "-------------------------------------------~%")
    (format t "~%State         No of visits     Proportion~%")
    (format t "-------------------------------------------~%")
    (dolist (obj (slot-value 'state-objs))
	    (if (> time 0) 
		(send obj :describe time)
	      (send obj :describe)))
  (format t "-------------------------------------------~%")))

(defmeth dmc-proto :reset ()
  "Method args: ()
   Resets the Markov Chain."
  (send self :time 0)
  (dolist (obj (slot-value 'state-objs))
	  (send obj :reset))
  (if (slot-value 'sample-paths)
      (send (slot-value 'sample-paths) :clear))
  (when (slot-value 'history)
	(send self :record-history nil))
  (let ((obj (send self :current-state-obj)))
    (setf (slot-value 'current-state)
	  (slot-value 'initial-state))
    (send (send self :current-state-obj) 
	  :you-are-current-state self)
    (send obj :you-are-not-current-state self)))

(defmeth dmc-proto :redraw ()
  "Method args: ()
   Redraws the states and other buttons in window."
  (dolist (obj (slot-value 'button-objs))
	  (send obj :redraw self))
  (dolist (obj (slot-value 'state-objs))
	    (send obj :redraw self)))

(defmeth dmc-proto :add-some-menu-items ()
  "Method args: ()
   Adds some items to the Menu."
  (let (
	(run-item 
	 (send menu-item-proto :new "Run Quietly"
	       :action 
	       #'(lambda ()
		   (let ((tmp (get-value-dialog
			       "How many steps?" :initial 1)))
		     (when tmp
			   (send self :run-dmc-silently 
				 (select tmp 0)))))))

	(sample-paths-item 
	 (send menu-item-proto :new "Sample Paths"
	       :action 
	       #'(lambda ()
		   (send self :sample-paths
			 (not 
			  (slot-value 'sample-paths))))))

	(desc-item 
	 (send menu-item-proto :new "Describe"
	       :action
	       #'(lambda () (send self :describe))))
	
        (s-color 
	 (send menu-item-proto :new "Normal State Color"
	       :action
	       #'(lambda () (send self :set-state-color))))

	(c-color 
	 (send menu-item-proto :new "Current State Color" 
	       :action 
	       #'(lambda () 
		   (send self :set-c-state-color))))
	
	(s-text-color 
	 (send menu-item-proto :new "State Text Color"
	       :action 
	       #'(lambda ()
		   (send self :set-state-text-color))))

	(c-text-color 
	 (send menu-item-proto 
	       :new "Current State Text Color"
	       :action 
	       #'(lambda ()
		   (send self :set-c-state-text-color))))

	(hist-item 
	 (send menu-item-proto :new "History"
	       :action 
	       #'(lambda ()
		   (send self :record-history
			 (not (send self :history)))))))

    (send self :menu (send menu-proto :new "Menu"))
    (send (send self :menu) :append-items run-item 
	  sample-paths-item hist-item desc-item 
	  (send dash-item-proto :new)
	  s-color c-color s-text-color c-text-color)))

(defmeth dmc-proto :history (&optional val)
  "Method args: (&optional val)
   Sets and retrieves history slot."
  (if val 
      (setf (slot-value 'history) val)
    (slot-value 'history)))

(defmeth dmc-proto :record-history (on)
  "Method args: (value)
   Toggles history recording on and off. Value must
   be boolean."
  (let ((hist-item 
	 (select (send (send self :menu) :items) 2)))
    (case on
	  (nil (setf (slot-value 'history) nil)
	       (format *history-file-handle*
		       "End Time: ~d~%" (slot-value 'time))
	       (close *history-file-handle*)
	       (send hist-item :mark nil))
	  (t (let ((fname (get-string-dialog 
			   "Name of output file: " 
			   :initial *history-file*)))
	       (when fname
		     (setf (slot-value 'history) t)
		     (setf *history-file-handle* 
			   (open fname :direction :output))
		     (format *history-file-handle* 
			     "Start Time: ~d~%" 
			     (slot-value 'time))
		     (send hist-item :mark t)))))))

(defmeth dmc-proto :sample-paths (on)
  "Method args: (value)
   Toggles sample path window on and off. Value should
   be boolean."
  (let ((menu-item (select (send (send self :menu) :items) 1))
	(boss self))
    (case on
	  (nil (send (slot-value 'sample-paths) :close))
	  (t (let ((w (plot-lines 
		       (list (slot-value 'time))
		       (list (slot-value 'current-state))))
		   (y-lim 
		    (select (get-nice-range 
			     0 (send self :no-of-states) 
			     5) 1)))
	       (send w :range 1 0 y-lim)
	       (send w :title 
		     (concatenate 'string 
				  (send boss :title)
				  ": Sample Paths"))
	       (if (eql *time-window* 0)
		   (send w :has-h-scroll 1000)
		 (send w :range 0 0 *time-window*))
	       (send menu-item :mark t)
	       (setf (slot-value 'sample-paths) w)
	       (defmeth w :close ()
		 (send menu-item :mark nil)
		 (send boss :slot-value 'sample-paths nil)
		 (send self :remove)))))))

(defmeth dmc-proto :close ()
  "Method args: ()
   Closes the window and kills the markov chain object."
  (if (slot-value 'sample-paths)
      (send (slot-value 'sample-paths) :close))
  (call-next-method))

(defmeth dmc-proto :set-state-color ()
  (message-dialog 
   "Not yet implemented!\n~
   Set the variable *normal-state-color*\n~
   to the desired color, then lower and raise\n~
   the window to see the effect.\n~
   The function color-symbols will return the\n~
   various colors available.\n\n~
   Example: (def *normal-state-color* 'red)"))

(defmeth dmc-proto :set-c-state-color ()
  (message-dialog 
   "Not yet implemented!\n~
   Set the variable *current-state-color*\n~
   to the desired color, then lower and raise\n~
   the window to see the effect.\n~
   The function color-symbols will return the\n~
   various colors available.\n\n~
   Example: (def *current-state-color* 'blue)"))

(defmeth dmc-proto :set-state-text-color ()
  (message-dialog 
   "Not yet implemented!\n~
   Set the variable *normal-state-text-color*\n~
   to the desired color, then lower and raise\n~
   the window to see the effect.\n~
   The function color-symbols will return the\n~
   various colors available.\n\n~
   Example: (def *normal-state-text-color* 'yellow)"))

(defmeth dmc-proto :set-c-state-text-color ()
  (message-dialog 
   "Not yet implemented!\n~
   Set the variable *current-state-text-color*\n~
   to the desired color, then lower and raise\n~
   the window to see the effect.\n~
   The function color-symbols will return the\n~
   various colors available.\n\n~
   Example: (def *current-state-text-color* 'yellow)"))













