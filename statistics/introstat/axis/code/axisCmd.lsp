#|
    AxisCmd.lsp © Robert A. Stine

    11 Jul 94 ... Add commando-button-proto.
     5 Feb 94 ... Add list-forms; allow use in command objs, :Ready?, sendit.
    15 Jan 94 ... Add commands for saving methods from objects.
    22 Jul 93 ... Send it prints.  Cannot send if no target.
    29 Jun 93 ... Moved/renamed from commando.
    26 Apr 92 ... Created from dialogs.lsp
|#
;;;; Additions to
;;;; XLISP-STAT 2.0 Copyright (c) 1988, by Luke Tierney
;;;; Xlisp 2.0 Copyright (c) 1985, 1987 by David Michael Betz
;;;;    All Rights Reserved
;;;;    Permission is granted for unrestricted non-commercial use

(provide "axisCmd")

;;;;

(require "axisUtil")
(require "axisBoot")

;;
;;     COMMANDO DIALOG CLASS
;;


(defproto COMMANDO-PROTO
  '(name           ; name for the class
    cmdForm        ; function to apply to form
    textLabels     ; list of strings for static text items
    target         ; recipient of messages, thing created by eval cmdForm
    formList       ; list of form items
    toggleList     ; list of toggle items
    )  
  '()
  dialog-proto)


(defmeth commando-proto :ISNEW ()
   (let* ((labels (send self :text-items))
          (forms  (send self :form-items))
          (toggle (send self :toggle-items))
          (button (send self :button-items))
          (do-but (send button-item-proto :new (slot-value 'name) ; 7/11
                        :action #'(lambda () (send self :doit))))
          (sn-but (send button-item-proto :new "Messenger"
                        :action #'(lambda () (send self :send-it))))
          (items  (append
                   (mapcar #'list labels forms)
                   (list (list do-but sn-but))
                   button toggle
                   ))
          (title  (format nil "~a Command" (slot-value 'name)))  )
     (setf (slot-value 'formList) forms)
     (setf (slot-value 'toggleList) toggle)
     (call-next-method items :default-button do-but)
     (send self :title title)  ))

(defmeth commando-proto :INIT-SLOTS (name cmdForm labels)
  (setf (slot-value 'name) name)
  (setf (slot-value 'cmdForm) cmdForm)
  (setf (slot-value 'textLabels) 
        (if (listp labels) 
            labels
            (list labels) ))
  )

;;
;;     Override these methods (doit is skeleton)
;;

(defmeth commando-proto :READY? ()
  ; Checks if possesses a target.  If not, posts a dialog message.
  (if (slot-value 'target)
      t
      (message-dialog "No target; RUN command first."))
  )

(defmeth commando-proto :TOGGLE-ITEMS ()        ;; toggles
  () )

(defmeth commando-proto :BUTTON-ITEMS ()        ;; more buttons
  () )

(defmeth commando-proto :METHOD-LIST ()        ;; other methods
  () )

(defmeth commando-proto :DOIT ()                ;; doit gets hit 
  (let ((target (send self :eval (send self :command))))
    (def *target* target)                       ; global hook
    (if target 
        (setf (slot-value 'target) target)
        (format t "*** Nil target in ~a~%" (slot-value 'name))
        )))

;;
;;     Class methods
;;

(defmeth commando-proto :DATASET ()
  (let ((w (send self :window))   )
    (when w
          (send w :dataset)  )))

(defmeth commando-proto :NAME ()
  (slot-value 'name))

(defmeth commando-proto :WINDOW ()
  (if *active-icon-window*
      *active-icon-window*
      (message-dialog "Activate an icon window.")))

(defmeth commando-proto :TARGET ()
  (slot-value 'target))

(defmeth commando-proto :COMMAND ()
  (cons (slot-value 'cmdForm)
        (send self :form)
        ))

(defmeth commando-proto :EVAL (form)
  ; (format t "*** CP Evaluating ~a~%~%" form)
  (let ((ds (send self :dataset))  )
   (if ds
       (send ds :evaluate form)
       (eval form))))

(defmeth commando-proto :INITIAL-FORMS (nForms)
  (let ((names (mapcar #'(lambda (s) (format nil "~a " s))
                       (mapcar #'(lambda (icon) (send icon :name))
                               (send (send self :window)
                                     :icons-in-state 'selected))))  )
    (cond
      ((endp names) (repeat "" nForms))
      ((= 1 nForms) (list (apply #'strcat names)))
      ((= 2 nForms) (list (first names)
                          (apply #'strcat (rest names))) )
      (   t         (select (append names (repeat "" nForms))
                            (iseq nForms)))
      )))
 
  

(defmeth commando-proto :FORM (&optional item)
  ; Returns a LIST or the item whose elements are the form-item symbols.
  (if item
      (send (select (slot-value 'formList) item) :form)
      (mapcar #'(lambda (x) (send x :form))
              (slot-value 'formList))
      ))
      


(defmeth commando-proto :TEXT ()
  ; List of the text found in the input forms.
  (mapcar #'(lambda (f) (send f :text))
          (slot-value 'formList)))
 

(defmeth commando-proto :TEXT-ITEMS ()
  (mapcar #'(lambda (txt)
              (send text-item-proto :new (if (listp txt) (first txt) txt)))
          (slot-value 'textLabels)))

  
(defmeth commando-proto :FORM-ITEMS ()
  ; List item in input signals need for a list-form-item.  Strip wrapper.
  (mapcar #'(lambda(list? initForm)
              (if list?    ; 2/2/94
                  (send list-form-item-proto :new initForm :text-length 35)
                  (send   form-item-proto   :new initForm :text-length 35))  )
          (mapcar #'listp (slot-value 'textLabels))
          (send self :initial-forms
                (length (slot-value 'textLabels))   )))
 

(defmeth commando-proto :EVAL-IT ()
  ;  Evaluates input form in the context of the data base,
  ;  replacing "it" with the the target object.  Acts via side-effect.
  (when (send self :ready?)
        (let ((form (get-form-dialog "Enter expression to evaluate:"))  )
          (format t "Evaluating ~a gives: ~a~%" form
                  (send self :eval (subst (send self :target) 'it form))  ))
        ))
    

(defmeth commando-proto :SEND-IT ()
  ;  Evaluates input form in the context of the data base,
  ;  replacing "it" with the the target object.
  (when (send self :ready?)
        (messenger (send self :target)
                   (send self :dataset)
                   (send self :method-list))
        ))


#|
          COMMANDO BUTTON
                              11 Jul 94

    Action function of these expects the target to be the
    argument, unlike the usual button behavior.
|#

(defproto COMMANDO-BUTTON-PROTO
  ()      
  ()
  button-item-proto)

(defmeth commando-button-proto :do-action ()
  (let ((target (send (send self :dialog) :target))  )
    (if target
        (funcall (slot-value 'action) target)
        (let ((name (send (send self :dialog) :name))  )
          (message-dialog
           (format nil 
                   "Error:  no target for this command. Run ~a cmd first."
                   name)
           )))))

