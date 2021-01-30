(provide "g-v1-v2")
(require "g")
(require "v12-sld")
(require "slid-win")

;-------------------------------------------------------------------------
; gamma distribution class.
; histogram data is fitted using v1 && v2 parameters.
;-------------------------------------------------------------------------

( defproto g-v1-v2 

; slots
  '() 

; shared slots
  '()

; ancestors
  (list g v12-sld)

; documentation
  nil
)

;-------------------------- implementation -------------------------------

;-------------------------------------------------------------------------
; overridden methods
;-------------------------------------------------------------------------

( defmeth g-v1-v2 :update-graph ()
  ( send self :guessed-v1 (send (send self :v1-slider) :value) )
  ( send self :guessed-v2 (send (send self :v2-slider) :value) )
  ( call-next-method )
) ; method

( defmeth g-v1-v2 :close-all-windows ()
  ( send (send self :v1-slider) :close t )
  ( send (send self :v2-slider) :close t )
  ( call-next-method )
) ; method


( defmeth g-v1-v2 :do-command(command)
  (
    cond
    ( (eq command 'show-solution) (call-method v12-par :show-solution) )
    ( t (call-next-method command) )
  )
)

( defmeth g-v1-v2 :make-y ()
  (
    let*
    (
      ( v1 (send (send self :v1-slider) :value) )
      ( v2 (send (send self :v2-slider) :value) )
      ( x (send self :make-x) )
    ) ; end of local declarations
    ( * v2 (gamma-dens (* v2 x) v1) )
  ) ; let
) ; method

( defmeth g-v1-v2 :isnew ()
  (call-next-method)
; some local declarations
    (
      let*
      ( 
        ( graph-name "Gamma" )
        ( v1-slider-name "V1" )
        ( v2-slider-name "V2" )
      ) ; end of local declarations

; set up histogram window
      ( send (send self :graph) :size 384 384)
      ( send (send self :graph) :title graph-name )
      ( send (send self :graph) :add-points (send self :sample) )
      ( send (send self :graph) :adjust-to-data )
      ( send (send self :graph) :num-bins 25 )

; create v1-slider
      ( send self :v1-slider
        ( send slid-win :new
          self
          (list 1 120)
          :points 119
          :action #'(lambda(x) (send self :do-command 'update-graph))
          :title v1-slider-name
        )
      )
      ( send (send self :v1-slider) :location 450 100 ) 

; create v2-slider
      ( send self :v2-slider
        ( send slid-win :new
          self
          (list 1 120)
          :points 119
          :action #'(lambda(x) (send self :do-command 'update-graph))
          :title v2-slider-name
        )
      )
      ( send (send self :v2-slider) :location 450 210 ) 
    ) ; let*

; give a random default value to the v1 & v2 sliders
  ( send (send self :v1-slider) :value (+ (random 119) 1) )
  ( send self :guessed-v1 (send (send self :v1-slider) :value) )
  ( send (send self :v2-slider) :value (+ (random 119) 1) )
  ( send self :guessed-v2 (send (send self :v2-slider) :value) )
  ( send self :update-graph )
) ; method
