(provide "dist-win")
; distribution window class

;----------------------------------------------------------------------------------
; An object of class dist usually owns an histogram window object
; and one or more slider windows. The purpose of this class is to signal
; (overriding the 'close' member function)
; an object of class dist when the user clicks the mouse on the histogram window
; close box; this allows class dist to close all the window objects it has at once.
;----------------------------------------------------------------------------------

( defproto dist-win
; slots
  '(commander)
; shared slots
  '()
; ancestor
  histogram-proto
; documentation
  nil
)

;-------------------------- implementation -------------------------------

;-------------------------------------------------------------------------
; owned slots accessors methods
;-------------------------------------------------------------------------

( defmeth dist-win :commander (&optional supervisor)
  (
    cond
    ( (eq supervisor nil) (slot-value 'commander) )
    ( t (setf (slot-value 'commander) supervisor) )
  ) ; cond
) ; method

;-------------------------------------------------------------------------
; overridden methods
;-------------------------------------------------------------------------

( defmeth dist-win :close (&optional isCommanderCalling)
  (
    cond
    ( (eq isCommanderCalling t) (call-next-method) )
    ( t (send (send self :commander) :do-command 'going-away) )
  )
)

( defmeth dist-win :isnew (supervisor &rest args)
  (apply #'call-next-method args)
  (send self :commander supervisor)
)
