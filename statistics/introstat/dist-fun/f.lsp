(provide "f")
(require "dist")
(require "v12-par")
(require "slid-win")

;-------------------------------------------------------------------------
; abstract f distribution class
; no instances of this class are ever created.
;-------------------------------------------------------------------------

( defproto f 

; slots
  '() 

; shared slots
  '()

; ancestor
  (list dist v12-par)

; documentation
  nil
)

;-------------------------- implementation -------------------------------

;-------------------------------------------------------------------------
; overridden methods
;-------------------------------------------------------------------------

( defmeth f :make-x ()
  (
    let
    (
      ( x-value (+ (round (max (send self :sample))) 1) )
    )
    ( rseq (- x-value) x-value 400 )
  )
) ; method


( defmeth f :isnew ()
  ( call-next-method )
  (
    let*
    (
      ( v1-param (+ 1 (random 119)) )
      ( v2-param (round (+ 5 (/ v1-param (+ 2 (random 3))))) )
      ( rand-seq (f-rand 100 v1-param v2-param) )
    )
    (send self :sample rand-seq)
    (send self :v1-param v1-param)
    (send self :v2-param v2-param)
  )
)
