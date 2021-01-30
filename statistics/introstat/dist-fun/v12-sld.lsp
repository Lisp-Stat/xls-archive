(provide "v12-sld")

;--------------------------------------------------------------
; v12-sld class.
; this is a mix in class; it provides sliders symbols and
; accessors methods to objects of class derived from dist whose
; histogram data is fitted using v1 or (v1 && v2) parameters.
;--------------------------------------------------------------
 
( defproto v12-sld 

; slots
  '(
    v1-slider
    v2-slider
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

( defmeth v12-sld :v1-slider (&optional value)
  (
    cond
    ( (eq value nil) (slot-value 'v1-slider) )
    ( t (setf (slot-value 'v1-slider) value) )
  ) ; cond
) ; method

( defmeth v12-sld :v2-slider (&optional value)
  (
    cond
    ( (eq value nil) (slot-value 'v2-slider) )
    ( t (setf (slot-value 'v2-slider) value) )
  ) ; cond
) ; method
