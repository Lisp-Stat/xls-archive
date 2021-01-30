(provide "dist")
(require "dist-win")
(require "global.lsp")

;-------------------------------------------------------------------------
; abstract distribution class
;-------------------------------------------------------------------------

( defproto dist 

; slots
  '(
    graph       ; histogram window object
    sample      ; histogram data
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

( defmeth dist :graph (&optional value)
  (
    cond
    ( (eq value nil) (slot-value 'graph) )
    ( t (setf (slot-value 'graph) value) )
  ) ; cond
) ; method

( defmeth dist :sample (&optional value)
  (
    cond
    ( (eq value nil) (slot-value 'sample) )
    ( t (setf (slot-value 'sample) value) )
  ) ; cond
) ; method

;-------------------------------------------------------------------------
; dist methods
;-------------------------------------------------------------------------

( defmeth dist :do-command (command)
  (
    cond
    ( (eq command 'going-away)    (send self :close-all-windows) )
    ( (eq command 'update-graph)  (send self :update-graph) )
    ( t nil )
  )
)

( defmeth dist :close-all-windows ()
  ( send (send self :graph) :close t )
  ( setf front-dist nil )
)

( defmeth dist :update-graph ()
  ( send (send self :graph) :clear-lines )
  ( send (send self :graph) :add-lines (list (send self :make-x) (send self :make-y)) )
)

;-------------------------------------------------------------------------
; pure virtual methods, must be overridden in subclasses
;-------------------------------------------------------------------------

( defmeth dist :make-x () )
( defmeth dist :make-y () )

;-------------------------------------------------------------------------
; overridden methods
;-------------------------------------------------------------------------

( defmeth dist :isnew ()
  ( call-next-method )
  ( send self :graph (send dist-win :new self 1) )
)
