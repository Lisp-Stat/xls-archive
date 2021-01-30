(provide "global")

;-----------------------------------------------------------------
; this global is a pointer to the current distribution object;
; it is nil only if no object of class derived from class dist
; has ever been created or the user has closed all of the windows.
;-----------------------------------------------------------------

( def front-dist nil )
