(provide "g-mu-si2")
(require "g")
(require "mu-si2")
(require "slid-win")

;-------------------------------------------------------------------------
; gamma distribution class.
; histogram data is fitted using E[x] && Var[x] parameters.
;-------------------------------------------------------------------------

( defproto g-mu-si2 

; slots
  '() 

; shared slots
  '()

; ancestors
  (list g mu-si2)

; documentation
  nil
)

;-------------------------- implementation -------------------------------

;-------------------------------------------------------------------------
; overridden methods
;-------------------------------------------------------------------------

( defmeth g-mu-si2 :update-graph ()
  ( send self :guessed-mu (send (send self :mu-slider) :value) )
  ( send self :guessed-si2 (send (send self :si2-slider) :value) )
  ( call-next-method )
) ; method

( defmeth g-mu-si2 :close-all-windows ()
  ( send (send self :mu-slider) :close t )
  ( send (send self :si2-slider) :close t )
  ( call-next-method )
) ; method

( defmeth g-mu-si2 :do-command(command)
  (
    cond
    ( (eq command 'show-solution) (call-method mu-si2 :show-solution) )
    ( t (call-next-method command) )
  )
)

( defmeth g-mu-si2 :make-y ()
  (
    let*
    (
      ( mu (send (send self :mu-slider) :value) )
      ( si2 (send (send self :si2-slider) :value) )
      ( x (send self :make-x) )
      ( v1 (round (abs (/ (^ mu 2) si2))) )
      ( v2 (round (abs (/ mu si2))) )     
    ) ; end of local declarations
    ( cond ((< v1 1) (setf v1 1)) )
    ( cond ((< v2 1) (setf v2 1)) )
    ( * v2 (gamma-dens (* v2 x) v1) )
  ) ; let
) ; method

( defmeth g-mu-si2 :isnew ()
  (call-next-method)
; some local declarations
    (
      let*
      ( 
        ( graph-name "Gamma" )
        ( mu-slider-name "Mu" )
        ( si2-slider-name "Si2" )
        ( v1 (send self :v1-param) )
        ( v2 (send self :v2-param) )
        ( mu (/ v1 v2 ) )
        ( si2 (/ v1 (^ v2 2)) )
        ( est-mu (mean (send self :sample)) )
        ( est-si2 (mean (* (- (send self :sample) est-mu) (- (send self :sample) est-mu))) )
        ( mu-seq (list 0.01 (* mu 1.25)) )
        ( si2-seq (list 0.01 (* si2 1.25)) )
      ) ; end of local declarations

      ( send self :real-mu mu )
      ( send self :real-si2 si2 )
      ( send self :estimated-mu est-mu )
      ( send self :estimated-si2 est-si2 )

; set up histogram window
      ( send (send self :graph) :size 384 384)
      ( send (send self :graph) :title graph-name )
      ( send (send self :graph) :add-points (send self :sample) )
      ( send (send self :graph) :adjust-to-data )
      ( send (send self :graph) :num-bins 25 )

; create mu-slider
      ( send self :mu-slider
        ( send slid-win :new
          self
          mu-seq
          :points 400
          :action #'(lambda(x) (send self :do-command 'update-graph))
          :title mu-slider-name
        )
      )
      ( send (send self :mu-slider) :location 450 100 ) 

; create si2-slider
      ( send self :si2-slider
        ( send slid-win :new
          self
          si2-seq
          :points 400
          :action #'(lambda(x) (send self :do-command 'update-graph))
          :title si2-slider-name
        )
      )
      ( send (send self :si2-slider) :location 450 210 ) 
    ) ; let*

; give a random default value to the mu && si2 sliders
  ( send (send self :mu-slider) :value (* (send self :real-mu) (/ 1 (+ 2 (random 6)))) )
  ( send self :guessed-mu (send (send self :mu-slider) :value) )
  ( send (send self :si2-slider) :value (* (send self :real-si2) (/ 1 (+ 2 (random 6)))) )
  ( send self :guessed-si2 (send (send self :si2-slider) :value) )
  ( send self :update-graph )
) ; method
