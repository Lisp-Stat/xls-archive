#|
     AxisIcon.lsp © Robert A. Stine

               Icons for manipulating variables of a dataset.

     11 Jul 94 ... Add method for external setting of string; do-drop-on.
     22 Jul 93 ... Added summary menu to popup; separate name from form.
      5 Jul 93 ... Added printing option to popup.
|#
;;;; Additions to
;;;; XLISP-STAT 2.0 Copyright (c) 1988, by Luke Tierney
;;;; Xlisp 2.0 Copyright (c) 1985, 1987 by David Michael Betz
;;;;    All Rights Reserved
;;;;    Permission is granted for unrestricted non-commercial use
;;;;

(require "icons")

;;;;

(provide "axisIcon")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun MAKE-VARIABLE-ICON (symbol ds loc &key form)
  (format t "Making icon for ~a.~%" symbol)
  (send variable-icon-proto :new
        symbol (if form form symbol) ds loc))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;  Variable icons represent forms to be evaluated in the context
;  of the associated data set.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defproto VARIABLE-ICON-PROTO 
  '(
    form   ; form of reference variable
   )        
  ()
  rectangle-icon-proto)


(defmeth variable-icon-proto :ISNEW (symbol form ds loc)
  (setf (slot-value 'form) form)
  (call-next-method (send ds :window) 
                    :name (format nil "~a" symbol)
                    :location loc)
  )


(defmeth variable-icon-proto :COPY (&key window)    ; OVERRIDE
  (make-variable-icon (slot-value 'form)
                      (send self :dataset)
                      (+ '(20 20) (send self :location))))

(defmeth variable-icon-proto :DATASET ()
  (send (slot-value 'window) :dataset))


;  (make-variable-icon '(log cons) (first *datasets*) (list 10 10))
;  (make-variable-icon  'income (first *datasets*) (list 30 30))
;  (send (first *datasets*) :evaluate 'cons)

(defmeth variable-icon-proto :RUN-DIALOG ()
  (let* ((nText (send      text-item-proto :new "Enter name:"))
         (nEdit (send edit-text-item-proto :new (slot-value 'name)
                      :text-length 20))
         (fText (send      text-item-proto :new "Formula:"))
         (fEdit (send edit-text-item-proto :new 
                      (format nil "~a" (slot-value 'form))
                      :text-length 30))
         (toggle (send toggle-item-proto :new "Convert to values"))
         (cancel (send modal-button-proto :new "Cancel"))
         (ok     (send modal-button-proto :new "OK"
                       :action #'(lambda ()
                                   (list (send nEdit :text)
                                         (send fEdit :text)
                                         (send toggle :value)) )))  )
     (send    (send modal-dialog-proto :new
                   (list
                    (list nText nEdit)
                    (list fText fEdit)
                    (list ok cancel)
                    (list toggle))
                   :default-button ok)
             :modal-dialog)
    ))
      

(defmeth variable-icon-proto :DO-DOUBLE-CLICK (x y s o)
  (let ((reply (send self :run-dialog)))
    (when reply ; first two are strings, last is check box
          (let ((nameForm (string-to-form (first reply)))
                (form (string-to-form (second reply)))
                (ds   (send self :dataset))   )
            (send self :name-is (first reply))
            (setf (slot-value 'form) form)
            (send ds :add-variable
                  nameForm form
                  :addIcon? nil
                  :forceEval? (select reply 2))
            ))))


(defmeth variable-icon-proto :DO-DROP-ON (icon)
  (when (kind-of-p icon feature-icon-proto)
        (send icon :string-is (send self :name))
        ))


(defmeth variable-icon-proto :BUILD-MENU ()
  ; Builds the pop-up menu for the icon.
  (let* ((menu (send menu-proto :new "Menu"))
         (ds   (send self :dataset))
         (print (send menu-item-proto :new "Print"
                      :action
                      #'(lambda ()
                          (let ((lab (send ds :case-labels)) )
                            (print-matrix
                             (bind-columns
                              lab
                              (send ds :evaluate (slot-value 'form))))
                            ))))
         (desc (send menu-item-proto :new "Describe"
                     :action
                     #'(lambda ()
                         (let ((data (send ds :evaluate (slot-value 'form))) )
                           (format t "Summary of ~a.~%" (slot-value 'name))
                           (format t "Five number summary - ~d, ~a~%"
                                   (length data) (fivnum data))
                           (format t "Mean = ~a with s.d. = ~a~%"
                                   (mean data)(standard-deviation data))
                           ))))
         (hist (send menu-item-proto :new "Histogram" 
                     :action
                     #'(lambda ()
                         (let* ((data (send ds :evaluate (slot-value 'form)
                                            :ignoreFilter? t))
                                (h (histogram  data))   )
                           (send h :title (slot-value 'name))
                           (send h :add-lines (kernel-dens data)); 7/12/94
                           (send ds :set-point-state h)  ))))
         (seq  (send menu-item-proto :new "Sequence Plot" 
                     :action
                     #'(lambda ()
                         (let* ((dat (send ds :evaluate (slot-value 'form)
                                         :ignoreFilter? t))
                                (p (plot-points (iseq (length dat)) dat
                                   :variable-labels
                                   (list "Case Number" (slot-value 'name))
                                   :point-labels
                                   (send ds :case-labels)  )))
                           (send p :title (slot-value 'name))
                           (send ds :set-point-state p)  ))))
         )
    (send menu :append-items print desc hist seq)
    (setf (slot-value 'menu) menu)
    ))
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun MAKE-FEATURE-ICON (ds name xPos action &optional wide)
  (format t "Making feature icon for ~a.~%" name)
  (send feature-icon-proto :new ds name xPos action wide)  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defproto FEATURE-ICON-PROTO
  '(
    action     ; called at double click
    wide?      ; predicate called to draw wide border
    str        ; current text form
    )
  ()
  oval-icon-proto)

(defmeth feature-icon-proto :ISNEW (ds name xPos action wide?)
  (setf (slot-value 'str)     "")
  (setf (slot-value 'action) action)
  (setf (slot-value 'wide?) wide?)
  (call-next-method (send ds :window)
                    :name name
                    :location (list xPos 150) )
  )

(defmeth feature-icon-proto :STRING-IS (str)
  ; Sets internal string to input value.   11 Jul 94
  (setf (slot-value 'str) str)
  (funcall (slot-value 'action) str)  
  (send self :draw) 
  )


(defmeth feature-icon-proto :DATASET ()
  (send (slot-value 'window) :dataset))


(defmeth feature-icon-proto :WIDE? ()
  (if (slot-value 'wide?)
      (funcall (slot-value 'wide?))
      nil))


(defmeth feature-icon-proto :BUILD-MENU () ; disabled
  )

(defmeth feature-icon-proto :SELECT ()     ; disabled
  )

(defmeth feature-icon-proto :DRAW-SHAPE ()
  (if (or (send self :wide?) 
          (if (stringp (slot-value 'str))
              (< 0 (length (slot-value 'str)))
              (slot-value 'str)))
      (let ((w (slot-value 'window)) )
        (send w :line-width 3)
        (call-next-method)
        (send w :line-width 1)
        )
      (call-next-method)
      ))
   

(defmeth feature-icon-proto :DO-OPTION-CLICK (x y)
  (format t "Feature icons do not respond to option clicks.~%") )

(defmeth feature-icon-proto :DO-DOUBLE-CLICK (x y s o)
  (let ((str (get-form-dialog                    ; 7/21/94 
              (format nil "Enter ~a for ~a." 
                      (send self :name)
                      (send (send self :dataset) :name))
              :initial (slot-value 'str)))    )
    (send self :string-is str)
    ))
