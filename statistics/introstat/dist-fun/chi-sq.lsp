(provide "chi-sq")
(require "dist")
(require "v12-par")
(require "v12-sld")
(require "slid-win")

;-------------------------------------------------------------------------
; chi-squared distribution class.
;-------------------------------------------------------------------------

( defproto chi-sq 

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

( defmeth chi-sq :do-command(command)
  (
    cond
    ( (eq command 'show-solution) (call-method v12-par :show-solution-v1) )
    ( t (call-next-method command) )
  )
)

( defmeth chi-sq :update-graph ()
  ( send self :guessed-v1 (send (send self :v1-slider) :value) )
  ( call-next-method )
) ; method

( defmeth chi-sq :close-all-windows ()
  ( send (send self :v1-slider) :close t )
  ( call-next-method )
) ; method

( defmeth chi-sq :make-x ()
  (
    let
    (
      ( v-value (+ (round (max (send self :sample))) 1) )
    )
    ( rseq (- v-value) v-value 100 )
  )
) ; method

( defmeth chi-sq :make-y ()
  (
    let
    ( 
      ( x (send self :make-x) )
    ) ; end of local declarations

    ( chisq-dens x (send (send self :v1-slider) :value) )
  ) ; let
) ; method

( defmeth chi-sq :isnew ()
  (call-next-method)
; initialize count or increment it by one

; some local declarations
    (
      let*
      ( 
        ( n1 "Chi-squared" )
        ( n2 "v1" )
        ( graph-name "chi-squared" )
        ( v1-slider-name "v1" )
        ( v1-param (+ (mod (random 11213) 29) 1) )
        ( rand-seq (chisq-rand 100 v1-param) )
      ) ; end of local declarations

      (send self :sample rand-seq)
      (send self :v1-param v1-param)

; set up histogram window
      ( send (send self :graph) :size 384 384)
      ( send (send self :graph) :title graph-name )
      ( send (send self :graph) :add-points rand-seq )
      ( send (send self :graph) :adjust-to-data )
;      ( send (send self :graph) :fixed-aspect t )
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

; give a random default value to the v1 slider
  ( send (send self :v1-slider) :value (+ (random 29) 1) )
  ( send self :guessed-v1 (send (send self :v1-slider) :value) )
  ( send self :update-graph )
) ; method
