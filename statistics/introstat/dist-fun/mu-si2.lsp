(provide "mu-si2")

;-----------------------------------------------------------------------
; mu-si2 class.
; this is a mix in class; it provides sliders symbols, accessors methods
; and utility functions to objects of class derived from dist
; whose histogram data is fitted using mu && si2 parameters.
;-----------------------------------------------------------------------

( defproto mu-si2 

; slots
  '(
    real-mu
    real-si2
    guessed-mu
    guessed-si2
    estimated-mu     ; x=[x1,x2,...,xn]; E[x]=(x1+...+xn)/n
    estimated-si2    ; var(x)=E[(x-E[x])^2]
    mu-slider
    si2-slider
  ) 

; shared slots
  '()

; ancestor
  *object*

; documentation
  nil
)

( defmeth mu-si2 :real-mu (&optional value)
  (
    cond
    ( (eq value nil) (slot-value 'real-mu) )
    ( t (setf (slot-value 'real-mu) value) )
  ) ; cond
) ; method

( defmeth mu-si2 :real-si2 (&optional value)
  (
    cond
    ( (eq value nil) (slot-value 'real-si2) )
    ( t (setf (slot-value 'real-si2) value) )
  ) ; cond
) ; method

( defmeth mu-si2 :guessed-mu (&optional value)
  (
    cond
    ( (eq value nil) (slot-value 'guessed-mu) )
    ( t (setf (slot-value 'guessed-mu) value) )
  ) ; cond
) ; method

( defmeth mu-si2 :guessed-si2 (&optional value)
  (
    cond
    ( (eq value nil) (slot-value 'guessed-si2) )
    ( t (setf (slot-value 'guessed-si2) value) )
  ) ; cond
) ; method

( defmeth mu-si2 :estimated-mu (&optional value)
  (
    cond
    ( (eq value nil) (slot-value 'estimated-mu) )
    ( t (setf (slot-value 'estimated-mu) value) )
  ) ; cond
) ; method

( defmeth mu-si2 :estimated-si2 (&optional value)
  (
    cond
    ( (eq value nil) (slot-value 'estimated-si2) )
    ( t (setf (slot-value 'estimated-si2) value) )
  ) ; cond
) ; method

( defmeth mu-si2 :mu-slider (&optional value)
  (
    cond
    ( (eq value nil) (slot-value 'mu-slider) )
    ( t (setf (slot-value 'mu-slider) value) )
  ) ; cond
) ; method

( defmeth mu-si2 :si2-slider (&optional value)
  (
    cond
    ( (eq value nil) (slot-value 'si2-slider) )
    ( t (setf (slot-value 'si2-slider) value) )
  ) ; cond
) ; method

( defmeth mu-si2 :show-solution()
  (
    let*
    (
      ( error-g-mu-str "     |MU - GUESSED MU|  =  " )
      ( error-g-si2-str "     |SI2 - GUESSED SI2|  =  " )
      ( error-e-mu-str "     |MU - ESTIMATED MU|  =  " )
      ( error-e-si2-str "     |SI2 - ESTIMATED SI2|  =  " )

      ( g-mu-err (abs (- (send self :real-mu) (send self :guessed-mu))) )
      ( g-si2-err (abs (- (send self :real-si2) (send self :guessed-si2))) )
      ( e-mu-err (abs (- (send self :real-mu) (send self :estimated-mu))) )
      ( e-si2-err (abs (- (send self :real-si2) (send self :estimated-si2))) )

      ( mu-str (format nil "~,3f" (send self :real-mu)) )
      ( si2-str (format nil "~,3f" (send self :real-si2)) )
      ( g-mu-str (format nil "~,3f" (send self :guessed-mu)) )
      ( g-si2-str (format nil "~,3f" (send self :guessed-si2)) )
      ( e-mu-str (format nil "~,3f" (send self :estimated-mu)) )
      ( e-si2-str (format nil "~,3f" (send self :estimated-si2)) )

      ( g-mu-err-str (format nil "~,3f" g-mu-err) )
      ( g-si2-err-str (format nil "~,3f" g-si2-err) )
      ( e-mu-err-str (format nil "~,3f" e-mu-err) )
      ( e-si2-err-str (format nil "~,3f" e-si2-err) ) 

      ( name (send (send self :graph) :title) )

      ( mu-row (concatenate 'string "MU  =  " mu-str) )
      ( si2-row (concatenate 'string "SI2  =  " si2-str) )
      ( g-mu-row (concatenate 'string "GUESSED MU  =  " g-mu-str error-g-mu-str g-mu-err-str) )
      ( g-si2-row (concatenate 'string "GUESSED SI2  =  " g-si2-str error-g-si2-str g-si2-err-str) )
      ( e-mu-row (concatenate 'string "ESTIMATED MU  =  " e-mu-str error-e-mu-str e-mu-err-str) )
      ( e-si2-row (concatenate 'string "ESTIMATED SI2  =  " e-si2-str error-e-si2-str e-si2-err-str) )

      ( message (list name mu-row si2-row g-mu-row g-si2-row e-mu-row e-si2-row) )
    ) ; let*
    (
      let ((dialog (send message-dialog-proto :new message)))
      (send dialog :modal-dialog)
    )
  )
)
