(provide "v12-par")

;-----------------------------------------------------------------------
; v12-par class.
; this is a mix in class; it provides symbols, accessors methods
; and utility functions to objects of class derived from dist
; whose histogram data is fitted using v1 or (v1 && v2) parameters.
;-----------------------------------------------------------------------
 
( defproto v12-par 

; slots
  '(
    v1-param
    v2-param
    guessed-v1
    guessed-v2
  ) 

; shared slots
  '()

; ancestor
  *object*

; documentation
  nil
)

;-------------------------- implementation -------------------------------

;-------------------------------------------------------------------------
; owned slots accessors methods
;-------------------------------------------------------------------------

( defmeth v12-par :v1-param (&optional value)
  (
    cond
    ( (eq value nil) (slot-value 'v1-param) )
    ( t (setf (slot-value 'v1-param) value) )
  ) ; cond
) ; method

( defmeth v12-par :v2-param (&optional value)
  (
    cond
    ( (eq value nil) (slot-value 'v2-param) )
    ( t (setf (slot-value 'v2-param) value) )
  ) ; cond
) ; method

( defmeth v12-par :guessed-v1 (&optional value)
  (
    cond
    ( (eq value nil) (slot-value 'guessed-v1) )
    ( t (setf (slot-value 'guessed-v1) value) )
  ) ; cond
) ; method

( defmeth v12-par :guessed-v2 (&optional value)
  (
    cond
    ( (eq value nil) (slot-value 'guessed-v2) )
    ( t (setf (slot-value 'guessed-v2) value) )
  ) ; cond
) ; method

;-------------------------------------------------------------------------
; v12-par methods
;-------------------------------------------------------------------------

( defmeth v12-par :show-solution ()
  (
    let*
    (
      ( name (send (send self :graph) :title) )
      ( real-v1 "v1                 " )
      ( guessed-v1 "guessed   v1" )
      ( real-v2 "v2                 " )
      ( guessed-v2 "guessed   v2" )
      ( r-v1 (format nil "~,3f" (send self :v1-param)) )
      ( g-v1 (format nil "~,3f" (send self :guessed-v1)) )
      ( r-v2 (format nil "~,3f" (send self :v2-param)) )
      ( g-v2 (format nil "~,3f" (send self :guessed-v2)) )
      
      ( title-row name )
      ( real-v1-row (list real-v1  r-v1) )
      ( guessed-v1-row (list guessed-v1 g-v1) )
      ( real-v2-row (list real-v2  r-v2) )
      ( guessed-v2-row (list guessed-v2 g-v2) )
       
      ( message (list title-row real-v1-row real-v2-row guessed-v1-row guessed-v2-row) )
    )
    (
      let ((dialog (send message-dialog-proto :new message)))
      (send dialog :modal-dialog)
    )
  )
)

( defmeth v12-par :show-solution-v1 ()
  (
    let*
    (
      ( name (send (send self :graph) :title) )
      ( real-v "v                  " )
      ( guessed-v "guessed  v" )
      ( r-v (format nil "~,3f" (send self :v1-param)) )
      ( g-v (format nil "~,3f" (send self :guessed-v1)) )
      
      ( title-row name )
      ( real-v-row (list real-v  r-v) )
      ( guessed-v-row (list guessed-v g-v) )
       
      ( message (list title-row real-v-row guessed-v-row) )
    )
    (
      let ((dialog (send message-dialog-proto :new message)))
      (send dialog :modal-dialog)
    )
  )
)
