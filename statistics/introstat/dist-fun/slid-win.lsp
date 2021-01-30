(provide "slid-win")
; slid-win class

;----------------------------------------------------------------------------------
; slid-win class.
; An object of class dist usually owns an histogram window object
; and one or more slider windows. The purpose of this class is to signal
; (overriding the 'close' member function)
; an object of class dist when the user clicks the mouse on a slider window's
; close box; this allows class dist to close all the window objects it has at once.
;----------------------------------------------------------------------------------

( defproto slid-win
; slots
  '(commander)
; shared slots
  '()
; ancestor
  interval-slider-dialog-proto
; documentation
  nil
)

;-------------------------- implementation -------------------------------

;-------------------------------------------------------------------------
; owned slots accessors methods
;-------------------------------------------------------------------------

( defmeth slid-win :commander (&optional supervisor)
  (
    cond
    ( (eq supervisor nil) (slot-value 'commander) )
    ( t (setf (slot-value 'commander) supervisor) )
  ) ; cond
) ; method

;-------------------------------------------------------------------------
; overridden methods
;-------------------------------------------------------------------------

( defmeth slid-win :close (&optional isCommanderClosing)
  (
    cond
    ( (eq isCommanderClosing t) (call-next-method) )
    ( t (send (send self :commander) :do-command 'going-away) )
  )
)

( defmeth slid-win :isnew (supervisor &rest args)
  (apply #'call-next-method args)
  (send self :commander supervisor)
)
