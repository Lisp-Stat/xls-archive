(provide "f-mu-si2")
(require "f")
(require "mu-si2")
(require "slid-win")

;-------------------------------------------------------------------------
; f distribution class.
; histogram data is fitted using E[x] && Var[x] parameters.
;-------------------------------------------------------------------------

( defproto f-mu-si2 

; slots
  '() 

; shared slots
  '()

; ancestors
  (list f mu-si2)

; documentation
  nil
)

;-------------------------- implementation -------------------------------

;-------------------------------------------------------------------------
; overridden methods
;-------------------------------------------------------------------------

( defmeth f-mu-si2 :update-graph ()
  ( send self :guessed-mu (send (send self :mu-slider) :value) )
  ( send self :guessed-si2 (send (send self :si2-slider) :value) )
  ( call-next-method )
) ; method

( defmeth f-mu-si2 :close-all-windows ()
  ( send (send self :mu-slider) :close t )
  ( send (send self :si2-slider) :close t )
  ( call-next-method )
) ; method

( defmeth f-mu-si2 :do-command(command)
  (
    cond
    ( (eq command 'show-solution) (call-method mu-si2 :show-solution) )
    ( t (call-next-method command) )
  )
)

( defmeth f-mu-si2 :make-y ()
  (
    let*
    (
      ( mu (send (send self :mu-slider) :value) )
      ( si2 (send (send self :si2-slider) :value) )
      ( x (send self :make-x) )
      ( v2 (round (/ (* 2 mu) (- mu 1))) )
      ( v1-num (- (* 2 (^ v2 3)) (* 4 (^ v2 2))) )
      ( v1-den (- (* si2 (^ (- v2 2) 2) (- v2 4)) (* 2 (^ v2 2))) )
      ( v1 (round (abs (/ v1-num v1-den))) ) 
    ) ; end of local declarations
    ( cond ((eq v1 0) (setf v1 1)) )
    ( f-dens x v1 v2 )
  ) ; let
) ; method

( defmeth f-mu-si2 :isnew ()
  (call-next-method)
; some local declarations
    (
      let*
      ( 
        ( graph-name "F" )
        ( mu-slider-name "Mu" )
        ( si2-slider-name "Si2" )
        ( v1 (send self :v1-param) )
        ( v2 (send self :v2-param) )
        ( mu (/ v2 (- v2 2)) )
        ( si2-num (* (* 2 (^ v2 2)) (+ (- v2 2) v1)) )
        ( si2-den (* (* v1 (^ (- v2 2) 2)) (- v2 4)) )
        ( si2 (/ si2-num si2-den) )
        ( est-mu (mean (send self :sample)) )
        ( est-si2 (mean (* (- (send self :sample) est-mu) (- (send self :sample) est-mu))) )

        ( mu-seq (list 1.017 (* mu 1.325)) )
        ( si2-seq (list 0.035 (* si2 1.325)) )
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
          :points 100
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
          :points 200
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
