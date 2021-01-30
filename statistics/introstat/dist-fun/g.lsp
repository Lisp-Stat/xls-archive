(provide "g")
(require "dist")
(require "v12-par")
(require "slid-win")

;-------------------------------------------------------------------------
; abstract gamma distribution class.
; no instances of this class are ever created.
;-------------------------------------------------------------------------

( defproto g 

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

;-------------------------------------------------------------------------
; overridden methods
;-------------------------------------------------------------------------

( defmeth g :make-x ()
  (
    let
    (
      ( x-value (+ (round (max (send self :sample))) 1) )
    )
    ( rseq (- x-value) x-value 400 )
  )
) ; method


( defmeth g :isnew ()
  ( call-next-method )
  (
    let*
    (
      ( v1-param (+ 2 (random 119)) )
      ( v2-param (+ 1 (round (/ v1-param 2))) )
      ( r-seq (gamma-rand 100 v1-param) )
      ( rand-seq (/ r-seq v2-param) )
    )
    (send self :sample rand-seq)
    (send self :v1-param v1-param)
    (send self :v2-param v2-param)
  )
)
