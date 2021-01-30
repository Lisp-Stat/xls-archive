(provide "b")
(require "dist")
(require "v12-par")
(require "slid-win")

;-------------------------------------------------------------------------
; abstract beta distribution class
; no instances of this class are ever created.
;-------------------------------------------------------------------------

( defproto b 

; slots
  '() 

; shared slots
  '()

; ancestors
  (list dist v12-par)

; documentation
  nil
)

;-------------------------- implementation -------------------------------

( defmeth b :make-x ()
  (
    let
    (
      ( x-value (+ (round (max (send self :sample))) 1) )
    )
    ( rseq (- x-value) x-value 400 )
  )
) ; method


( defmeth b :isnew ()
  ( call-next-method )
  (
    let*
    (
      ( v1-param (+ 5 (random 35)) )
      ( v2-param (+ 1 (random 4) (round (* 0.2 v1-param))) )
;      ( v1-param (+ 11 (random 29)) )
;      ( v2-param (+ 7 (random 33)) )
      ( rand-seq (beta-rand 200 v1-param v2-param) )
    )
    (send self :sample rand-seq)
    (send self :v1-param v1-param)
    (send self :v2-param v2-param)
  )
)
