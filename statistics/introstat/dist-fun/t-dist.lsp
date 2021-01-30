(provide "t-dist")
(require "dist")
(require "v12-par")
(require "v12-sld")
(require "slid-win")

;-------------------------------------------------------------------------
; t distribution class
;-------------------------------------------------------------------------

( defproto t-dist 

; slots
  '() 

; shared slots
  '()

; ancestors
  (list dist v12-par v12-sld)

; documentation
  nil
)

;-------------------------- implementation -------------------------------

;-------------------------------------------------------------------------
; overridden methods
;-------------------------------------------------------------------------

( defmeth t-dist :update-graph ()
  ( send self :guessed-v1 (send (send self :v1-slider) :value) )
  ( call-next-method )
) ; method

( defmeth t-dist :close-all-windows ()
  ( send (send self :v1-slider) :close t )
  ( call-next-method )
) ; method

( defmeth t-dist :do-command(command)
  (
    cond
    ( (eq command 'show-solution) (call-method v12-par :show-solution-v1) )
    ( t (call-next-method command) )
  )
)

( defmeth t-dist :make-x ()
  (
    let
    (
      ( v1-value
        (+ (round (* (max (t-dens (send self :sample) (send self :v1-param))) 100)) 1)
      )
    )
    ( rseq (- v1-value) v1-value 500 )
  )
) ; method

( defmeth t-dist :make-y ()
  (
    let
    ( 
      ( x (send self :make-x) )
    ) ; end of local declarations

    ( t-dens x (send (send self :v1-slider) :value) )
  ) ; let
) ; method

( defmeth t-dist :isnew ()
  (call-next-method)
; some local declarations
    (
      let*
      ( 
        ( graph-name "T" )
        ( v1-slider-name "V" )
        ( v1-param (+ (mod (random 11213) 29) 1) )
        ( rand-seq (t-rand 100 v1-param) )
        ( v1-range (* 1.3 v1-param) )
      ) ; end of local declarations

      (send self :sample rand-seq)
      (send self :v1-param v1-param)

; set up histogram window
      ( send (send self :graph) :size 384 384)
      ( send (send self :graph) :title graph-name )
      ( send (send self :graph) :add-points rand-seq )
      ( send (send self :graph) :adjust-to-data )
      ( send (send self :graph) :num-bins 25 )

; create v1-slider
      ( send self :v1-slider
        ( send slid-win :new
          self
          (list 1 30)
          :points 29
          :action #'(lambda(x) (send self :do-command 'update-graph))
          :title v1-slider-name
        )
      )
      ( send (send self :v1-slider) :location 450 100 ) 
    ) ; let*

; give a random default value to the parameter slider
  ( send (send self :v1-slider) :value (+ (random 29) 1) )
  ( send self :guessed-v1 (send (send self :v1-slider) :value) )
  ( send self :update-graph )
) ; method
