;;;;
;;;;     AxisUtil.lsp © Robert A. Stine, 1992
;;;;                 Utilities shared within AXIS functions.
#||
 17 Apr 94 ... Add jitter.
  2 Apr 94 ... Minor tune-ups.
 27 Mar 94 ... Clean up form items; move in Describe.
 19 Mar 94 ... Bug in corr utility; add not-match.
 27 Feb 94 ... Saver object built from old commando methods. Bug in s-to-f
 19 Feb 94 ... Match added.
  5 Feb 94 ... Add Messenger. Add strcat for new version compatible.
  8 Jul 92 ... Modified read-labelled-array.
  5 Jul 92 ... Created
||#
;;;;
;;;; Additions to
;;;; XLISP-STAT 2.0 Copyright (c) 1988, by Luke Tierney
;;;; Xlisp 2.0 Copyright (c) 1985, 1987 by David Michael Betz
;;;;    All Rights Reserved
;;;;    Permission is granted for unrestricted non-commercial use

(provide "axisUtil")

;;;;
;;;;     LISP EXTENSIONs
;;;;

(defun REPLACE-IF (pred aList new)
  ; Replacement in list 
  (cond ((endp aList)   ())
        ((funcall pred (first aList))  (setf (car aList) new))
        (    t          (replace-if pred (rest aList) new))
    ))

(defun MATCH (x y)    
  ; vectorized matching for either argument; surrogate for =
  (cond
    ((and (listp y) (listp x))
     (let* ((lenX (length x))
            (lenY (length y))
            (short (if (< lenX lenY) x y))
            (long  (if (>= lenX lenY) x y))  )
       (mapcar #'(lambda (li) (if (member li short) t nil)) long)))
    ((listp y) (mapcar #'(lambda (yi) (eq yi x)) y))
    ((listp x) (mapcar #'(lambda (xi) (eq xi y)) x))
    (   t      (equal x y))  ))

(defun NOT= (x y)
  (mapcar #'not (match x y)))

(defun JITTER (x &optional (factor .1))
  (let ((sd (standard-deviation x))  )
    (+ x (* .1 sd (normal-rand (length x))))
    ))

;;;;
;;;;     SIMPLE STATS
;;;; 

(defun Z-INTERVAL (data cc)
  (let* ((n  (length data))
         (m  (mean data))
         (se (/ (standard-deviation data) (sqrt n)))
         (za (- (normal-quant (* .5 (- 1 cc))  )))   )
    ;  (format t "z-value is ~a~%" za)
    (format t "~a% interval --> [ ~7,3g, ~7,3g ]~%" (* 100 cc) 
            (- m (* za se)) (+ m (* za se)))
    ))

(defun T-INTERVAL (data cc)
  (let* ((n  (length data))
         (m  (mean data))
         (se (/ (standard-deviation data) (sqrt n)))
         (ta (- (t-quant (* .5 (- 1 cc))  (1- n) )))   )
    ;  (format t "t-value is ~a~%" ta)
    (format t "~a% interval --> [ ~7,3g, ~7,3g ]~%" (* 100 cc) 
            (- m (* ta se)) (+ m (* ta se)))
    ))

(defun Pct-INTERVAL (data cc)
  (let* ((half (* .5 (- 1 cc)))
         (qn (quantile data (list half (- 1 half))))   )
    (format t "~a% interval --> [ ~7,3g, ~7,3g ]~%" (* 100 cc) 
           (first qn) (second qn))
    ))



(defun DESCRIBE (data &optional name)
  (format t "Description of ~a:~%" name)
  (format t "Mean = ~10,3g, SD = ~10,3g, SE-Mean = ~10,4g~%"
          (mean data) (standard-deviation data) (se-mean data))
  (when (< 49 (length data))
   (format t "Percentiles:~%")
   (format t
     "    5%       10%       25%       50%      75%        90%       95%~%")
     (mapcar #'(lambda (x) (format t "~10,3g" x))
             (quantile data '(.05 .1 .25 .5 .75 .9 .95)))
     (terpri)  )
   (format t "Five number summary: ~a n=~d ~%--------~%"  ; 3/25/94
          (fivnum data) (length data))
  )
  


(defun CORR (x y)
  (let ((xd (- x (mean x)))
        (yd (- y (mean y)))   )
    (/ (inner-product xd yd)
       (sqrt (* (inner-product xd xd) (inner-product yd yd)))
       )))

(defun SE-MEAN (data)
  (/ (standard-deviation data) (sqrt (length data))))


(defun TRIMMED-MEAN (data &key (pct .1))
  "Trims a pct from data each tail."
  (let* ((n      (length data))
         (lo     (floor   (* pct n)))
         (hi     (ceiling (* (- 1 pct) n)))
         (sorted (sort-data data))  )
    (mean (select sorted (iseq lo hi)))   ))


;;;;
;;;;     String conversions
;;;;

(defun STRCAT (&rest args)
  (apply #'concatenate 'string args) )

(defun AS-NUMBERS (boolean)
  (mapcar #'(lambda (x) (if x 1 0)) boolean))


(defun SYMBOL-TO-STRING (sym)
  ; If given a list, checks to see if the first is a known function.
  ; For example, (symbol-to-string '(/ 1 2)) gives "(/ 1 2)"
  (if (listp sym)
      (if (fboundp (first sym))  ; 3/25/94
          (format nil "~a" sym)  ; format a function call as one thing.
          (mapcar #'string sym))
      (string sym)
      ))

(defun STRING-TO-SYMBOL (str &key prefix)
  (let ((charList (remove #\( 
                     (remove #\)
                         (coerce str 'list)))  ))       ; 4/2/94
    (when (member #\Space charList)
          (setf charList (subst #\_ #\Space charList)))
    (setf charList
          (if prefix
              (concatenate 'string prefix charList)
              (coerce charList 'string))  )
    (with-input-from-string (s charList) (read s))
    ))

; (string-to-symbol "sdfs")
; (string-to-symbol "sdfr" :prefix "bs-")
; (string-to-symbol "(my name)" :prefix "bs-")      ; 4/2/94


(defun STRING-TO-FORM (str)
  (let ((ct 0))  
    (labels ((read-all (s)                 ; 2/27/94
               (let ((x (read s nil)))
                 (if x 
                     (progn (setf ct (1+ ct)) 
                            (cons x (read-all s)))
                     nil))))
    (when str
          (setf str (string-trim " " str))
          (when (< 0 (length str)) ; some non-blank items
                (unless (and         ; add ()'s around text
                         (eq (char "(" 0) (char str 0))
                         (eq (char ")" 0) (char str (1- (length str))))  )
                        (if (member #\Space (coerce str 'list))   ; 4/2/94
                            (setf str (concatenate 'string "(" str ")"))))
                (with-input-from-string (s str)
                                        (let ((res (read-all s))  ) ;2/27/94
                                          (if (= 1 ct)
                                              (first res)
                                              res)
                                          )))))))


; (string-to-form "(test case)")
; (string-to-form "a b") 
; (string-to-form "log x")
; (string-to-form "(log x)")
; (string-to-form "(log x) y z")
; (string-to-form "12")
; (string-to-form "(= 1 2) (= 2 3) (= 3 4)") ; 2/27/94 fixed



(defun GET-FORM-DIALOG (msg &key (initial ""))
  (string-to-form
   (get-string-dialog msg :initial initial)))
; (get-form-dialog "test")
  

(defun GET-POS-INTEGER-DIALOG (msg &key (init 1))
  (let ((n (get-value-dialog msg :initial init)))
    (when n
          (setf n (first n))
          (if (> n 0)  (floor n)
              (message-dialog "Need a postive integer."))
          )))


;;
;;     LISP FORM-READING DIALOG ITEMS
;;

#|
   Use a form-item whenever you just want one argument/variable,
   and use a list-form-item when you want to allow for more than one.
   List-form-items always return a list of the elements.
|#


(defproto FORM-ITEM-PROTO
  ()
  ()
  edit-text-item-proto
  ; An edit-text item that also returns Lisp form.
)

(defmeth form-item-proto :FORM () ;    &optional (enclose nil))
  (string-to-form (send self :text)))

; Returns a form which is a list of items.
(defproto LIST-FORM-ITEM-PROTO 
  ()
  ()
  form-item-proto
)

(defmeth list-form-item-proto :FORM ()
  (let ((form (string-to-form    ; avoid stack overflow error 3/25/94
               (call-method edit-text-item-proto :text)))  )
    (cond
      ((symbolp form) (list form))
      ((numberp form) (list form))
      ((endp    form)     nil    )
      ((listp   form) (if (and (symbolp (first form))
                               (fboundp (first form)) )
                          (list form)   ; wrap (log x)
                            form)  )
      (   t     form))))
        

(defmeth list-form-item-proto :TEXT (&optional txt)  ; 3/27/94
  (if txt
      (call-next-method txt)
      (mapcar #'(lambda (x) (format nil "~a" x))
              (send self :form))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  
;;     MESSENGER Dialog
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun MESSENGER-ITEM (target dataset methods &optional desc) ; 2/26/94
  (send menu-item-proto :new "Messenger" :action
        #'(lambda ()
            (messenger target dataset methods desc))))

(defun MESSENGER (target dataset methods &optional desc)
  (if target
      (send messenger-proto :new
            target dataset
            (if methods methods (send target :method-selectors))
            desc)
      (format t "Unable to build messenger; target is nil.~%")
      ))


(defproto MESSENGER-PROTO
  '(target    ; object to be sent the message.
    dataset   ; evaluation context.  If nil, done globally (eval)
    methods   ; symbols to be browsed with scroller
    desc      ; optional description
    formItem  ; form to read for evaluation
   )
  nil
  *object*
  )

(defmeth messenger-proto :ISNEW (target dataset methods desc)
  (setf (slot-value 'target) target)
  (setf (slot-value 'dataset) dataset)
  (setf (slot-value 'methods) methods)
  (setf (slot-value 'desc) desc)
  (send self :open))

(defmeth messenger-proto :BUILD-DIALOG-ITEMS ()
  (let* ((methods (slot-value 'methods))
         (desc    (slot-value 'desc))
         (cLabel  (send text-item-proto :new "Message"))
         (cmd     (send list-form-item-proto :new           ; 3/27/94
                        (format nil "~a" (first methods))
                        :text-length 25))
         (dLabel  (send text-item-proto :new "Description:"))
         (dItem   (send text-item-proto :new 
                        (if desc (first desc) "")
                        :text-length 30))
         (helper  #'(lambda (x)
                      (send dItem :text (send self :describe x))))
         (scroll  (send sequence-scroll-item-proto :new
                        (iseq (length methods))
                        :text-item cmd  :display methods
                        :action helper))
         (doit    (send button-item-proto :new "Send It"
                        :action #'(lambda () (send self :evaluate))))  )
    (setf (slot-value 'formItem) cmd)
    (list (list cLabel cmd) 
          (list doit scroll)
          dlabel dItem)
    ))


(defmeth messenger-proto :DESCRIBE (i)
  ; Description for ith method
  (if (slot-value 'desc)
      (select desc i)
      (let ((doc (send (slot-value 'target)
                       :documentation (select (slot-value 'methods) i)))   )
        (if doc doc "None available.")
        )))


(defmeth messenger-proto :EVALUATE ()
  (let* ((res nil)
         (form (send (slot-value 'formItem) :form))
         (cmd `(send ,(slot-value 'target) ,@form))   )
    (format t ">~a~%" (append '(send it) form))
    (setf res
          (if (slot-value 'dataset)
              (send (slot-value 'dataset) :evaluate cmd) 
              (eval cmd)))
    (format t "~a~%" res)
    res ))


(defmeth messenger-proto :OPEN ()
  (let ((dialog (send dialog-proto :new
                      (send self :build-dialog-items)))   )
    (send dialog :title 
          (strcat  (format nil "~a"
                           (send (slot-value 'target)
                                 :slot-value 'proto-name))
                   " Messenger"))
    ))

#||
(def p  (plot-points (uniform-rand 20) (uniform-rand 20)))
(def msg  (list :abline :add-points :add-lines :range :adjust-to-data))
(messenger p nil msg)
||#  


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  
;;     SAVER Dialog
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun SAVER-ITEM (target dataset &optional aList)  ; 2/26/94
  (send menu-item-proto :new "Save data" :action
        #'(lambda ()
            (saver target dataset aList))))

(defun SAVER (target dataset &optional aList)
  (if target
      (send saver-proto :new
            target
            dataset
            (if aList aList (send target :save-aList))  )
      (message-dialog "Unable to build messenger; target is nil.~%")
      ))

(defproto SAVER-PROTO
  '(target    ; object which can produce the data from aList
    dataset   ; evaluation context for saving
    aList     ; assoc list of symbol.method pairs
   )
  nil
  *object*
  )


(defmeth saver-proto :ISNEW (target dataset aList)
  (setf (slot-value 'target) target)
  (setf (slot-value 'dataset) dataset)
  (setf (slot-value 'aList) aList)
  (send self :open-dialog)   )


(defmeth saver-proto :OPEN-DIALOG ()
  ; Offers the user a list of methods that create items that
  ; may be saved in the associated dataset. Input assoc list
  ; is a list of symbol . method pairs.
  (let* ((dialog (send self :build-dialog))
         (choice (send dialog :modal-dialog))   )
    (when choice
          (send self :add-variables
                (mapcar #'(lambda (s m) (cons s m))
                        (mapcar #'string-to-symbol
                                (second choice))
                        (mapcar #'cdr
                                (select (slot-value 'aList)
                                        (first choice)))   )
                ))))
                

(defmeth saver-proto :BUILD-DIALOG ()
  ; Builds the dialog used in variable dialog.
  (let* ((dialogItems (mapcar
                       #'(lambda (p)
                           (list
                            (send toggle-item-proto :new "")
                            (send edit-text-item-proto :new
                                  (symbol-to-string (car p))
                                  :text-length 15)   ))
                       (slot-value 'aList)))
         (okBut  (send modal-button-proto :new "OK"
                       :action
                       #'(lambda ()
                           (let ((in (which (mapcar #'(lambda (i)
                                                        (send (car i) :value))
                                                   dialogItems)))   )
                             (list in (select 
                                       (mapcar
                                        #'(lambda (p)
                                            (send (second p) :text))
                                        dialogItems)
                                       in))
                             ))))
         (canBut (send modal-button-proto :new "Cancel"))   )
    (send modal-dialog-proto :new
          (list (send text-item-proto :new "Choose items to save")
                (list dialogItems)
                (list okBut canBut)  )))  )


(defmeth saver-proto :ADD-VARIABLES (pairs)
  ; Input assoc list of (symbol . meth) where expr is a method
  ; to send to the target slot object.  Pass values on to dataset.
  (let ((ds (slot-value 'dataset))
        (tar (slot-value 'target))   )
    (dolist (p pairs)
            (send ds :add-variable (car p)
                  (send tar (cdr p))   )
            )))


;;;;  _____  PROGRESS INDICATOR WINDOW  _____

#|| 
(setf indicator (progress-indicator 10 "Progress"))
(send indicator :at 2)
(send indicator :at 4)
(send indicator :at 8)
(send indicator :at 10)    ; auto closes when reaches limit
||#
                  

(defun PROGRESS-INDICATOR (stop title)
  (send progress-indicator-proto :new stop title) )

(defproto PROGRESS-INDICATOR-PROTO
  '(stop stopStr  ; upper limit
    at             ; current setting
   )
  '(size)
  graph-window-proto
  "Shows a bar indicating amount of task completed."
  )

(defmeth progress-indicator-proto :isnew (stop title)
  (setf (slot-value 'stop )   stop)
  (setf (slot-value 'stopStr) (format nil "~a" stop))
  (setf (slot-value 'at)      0)
  (send self :size 200 50)
  (send self :title title)
  (call-next-method)  )

(defmeth progress-indicator-proto :AT (value)
  (setf (slot-value 'at) value)
  (if (= value (slot-value 'stop))
      (send self :close)
      (send self :redraw)   ))

(defmeth progress-indicator-proto :REDRAW ()
  (send self :frame-rect 25 20 150 20)
  (send self :draw-string "0" 20 15)
  (send self :draw-string (slot-value 'stopStr) 170 15)
  (send self :paint-rect
        25 20                            ; origin 
        (floor (* 150 (/ (slot-value 'at) (slot-value 'stop)))) ; width
        20                               ; height
        )
  (call-next-method)  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  
;;     SLIDER MENU ITEM
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defproto SLIDER-MENU-ITEM-PROTO
  '(target     ; graph window it modifies
    slider )   ; slider used to make the changes
   ()
  menu-item-proto
  )

(defmeth slider-menu-item-proto :ISNEW (&key target label action)
  (setf (slot-value 'target) target)
  (call-next-method label :action action)
  )

(defmeth slider-menu-item-proto :SLIDER-IS (slider)
  (setf (slot-value 'slider) slider))

(defmeth slider-menu-item-proto :UPDATE ()
  ;  In order to be enabled, the target has no existing slider."
  (send self :enabled
        (if (slot-value 'slider)   ; we have no slider
            nil t)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;     INTERVAL-SLIDER-DIALOG-PROTO  enhancements
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(send INTERVAL-SLIDER-DIALOG-PROTO :add-slot 'menuItem)  ; dont really need

(defmeth interval-slider-dialog-proto :MENU-ITEM-IS (item)
  (setf (slot-value 'menuItem) item)
  )

(defmeth interval-slider-dialog-proto :CLOSE ()
  (if (slot-value 'menuItem)
      (send (slot-value 'menuItem) :slider-is nil)
      (format t "ERROR: interval-slider-dialog had no menu item.~%")  )
  (call-next-method)
  )
