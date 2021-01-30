(require "utils")

(def id (get-id))
(when (= (length id) 0)
      (message-dialog 
       "Error!\n\nI need a non-empty string since\nI will be using it for a file name.\nPlease try again!")
      (exit))
    

(def m (get-m id))
(when (< m 1)
      (message-dialog 
       "Error!\n\nYou must have run at least one Markov Chain.\nPlease try again!")
      (exit))

(def p (get-p id))
(when (< p 1)
      (message-dialog 
       "Error!\n\nYou must have at least one parameter.\nPlease try again!")
      (exit))

(def fields (get-all-fields id m nil))
(loop (let ((res (check-fields fields)))
	(if res 
	    (return)
	  (setf fields (get-all-fields id m fields)))))
 
(def tmp (select fields (iseq (- (length fields) 1))))
(def data-files  (mapcar #'(lambda(y) (select y 0)) tmp))
(def c  (mapcar #'(lambda(y) (select y 1)) tmp))
(def c (if (new-xlispstat)
	   (mapcar #'(lambda(x) (read-from-string x)) c)
	 (mapcar 
	  #'(lambda(x) (read (make-string-input-stream x))) c)))

(def hfiles  (mapcar #'(lambda(x) (select x 2)) tmp))
(def rlist (arrange (read-data-file (select (last fields) 0)) 2))
;;;
;;; Get the parameter names first.
;;;
(def tmp (select (read-data-columns (select hfiles 0) 3) 0))
(defvar *param-labels* (remove-duplicates tmp :test #'equal))

;;;
;;; Get the long list of hyper-parameters next.
;;;
(setf hlist ())
(dolist (y hfiles)
	(let ((w (read-data-columns y 3)))
	  (setf hlist (append hlist (list (arrange (select w 2) 2))))))

;;;
;;; Get the list of hyper-parameter names.
;;;
(def tmp (read-data-columns (select hfiles 0) 3))
(def hnames (mapcar #'(lambda(x y) (format nil "~a-~a" x y))
		    (select tmp 0) (select tmp 1)))

;;;
;;; Now for the window list.
;;;

(when (> p 4)
      (def prompt (format nil "You have ~d parameters in your model~%~
	           for ~a. That means you will have ~d windows.~%~
                   You can concentrate on a few windows.~%~
                   Please unselect some windows by clicking on~%~
                   the checked boxes with your mouse.~%" p id p))
      (def wlist (get-yes-no-list prompt *param-labels*)))


(def hd (length hnames))
(when (> hd 6)
      (def prompt (format nil "You have ~d hyperparameters in your model~%~
	           for ~a. That means you will have ~d sliders.~%~
                   You can concentrate on a few hyperparameters.~%~
                   Please unselect some sliders by clicking on~%~
                   the checked boxes with your mouse.~%" hd id hd))
      (def slist (arrange (get-yes-no-list prompt hnames) 2)))

(def fs (open (format nil "~a.lsp" id) :direction :output))

(format fs "(require \"master.lsp\")~%")
(format fs "(defvar *param-labels* '~s)~%" *param-labels*)
(format fs "(defvar d-files '~s)~%" data-files)
(format fs "(defvar hlist '~g)~%" hlist)
(format fs "(defvar rlist '~g)~%" rlist)
(format fs "(defvar hnames '~s)~%" hnames)
(format fs "(defvar wlist '~g)~%" wlist)
(format fs "(defvar slist '~g)~%" slist)
(format fs "(defvar constants '~g)~%" c)
(format fs "(setf posterior (send master-proto :new~%~
                     :data d-files~%~
                     :constants constants~%~
                     :p ~d~%~
                     :hyper-param-names hnames~%~
                     :identifier ~s~%~
                     :hyper-vals hlist~%~
                     :hyper-range-list rlist~%~
                     :hyper-show-list slist~%~
                     :create-window-list wlist))" p id)
(close fs)

;;; Ok, now invoke all the stuff.
(require "master")
(debug)
(setf posterior (send master-proto :new 
                     :data data-files
                     :constants c
                     :p p
                     :hyper-param-names hnames
                     :identifier id
                     :hyper-vals hlist
                     :hyper-range-list rlist
                     :hyper-show-list slist
                     :create-window-list wlist))


