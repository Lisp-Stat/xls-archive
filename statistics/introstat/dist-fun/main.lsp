(require "normal")
(require "log-n")
(require "chi-sq")
(require "t-dist")
(require "f-v1-v2")
(require "f-mu-si2")
(require "g-v1-v2")
(require "g-mu-si2")
(require "b-v1-v2")
(require "b-mu-si2")

; main program

;-----------------------------------------------------------
;         menus...
;-----------------------------------------------------------

( setf misc-menu (send menu-proto :new "Miscellaneous") )
( setf new-menu (send menu-proto :new "New distribution") )
( setf solution-menu (send menu-proto :new "Solution") )

;--------------------------------------------------------------------------------
;         pops up a dialog to let the user choose which type
;         of parametrization he/she is interested in; ((v1 && v2) or (mu && si2))
;         for a suitable distribution (e.g. the f distribution)
;--------------------------------------------------------------------------------

( defun choose-param-type (dist-name param-names)
  (
    let*
    (
      ( choice (send choice-item-proto :new param-names :value 0) )
      ( ok (send modal-button-proto :new "OK" :action #'(lambda()(send choice :value))) )
      ( dialog (send modal-dialog-proto :new (list dist-name choice ok)) )
    )
    ( send dialog :modal-dialog )
  )
)

;----------------------------------------------------------------------------
;         About box function
;----------------------------------------------------------------------------

( defun about()
  (
    let
    (
      ( blank "" )
      ( m1 "Distribution fun v1.0.1" )
      ( m2 "Developed in spring 1996 for the computational statistics course" )
      ( m3 "run by professor Andrea Pastore, e-mail pastore@unive.it," )
      ( m4 "at the University of Venice, Italy." )
      ( m5 "Written by Mirko Ravagnan, e-mail mravagna@dsi.unive.it" )
    )
    ( let
      ( (dialog (send message-dialog-proto :new (list m1 blank m2 m3 m4 blank m5 blank))) )
      ( send dialog :modal-dialog )
    )
  )
)

;-----------------------------------------------------------
;         misc-menu items
;-----------------------------------------------------------

(
    setf info-item
    (
        send menu-item-proto
            :new "About Distribution fun..."
            :action #'about
    )
)

(
    setf exit-item
    (
        send menu-item-proto
            :new "Exit"
            :action #'(lambda()
              (
                cond
                ( (eq front-dist nil) nil )
                ( t (send front-dist :close-all-windows) )
              )
              (send solution-menu :remove)
              (send new-menu :remove)
              (send misc-menu :remove)
            ) ; lambda
    )
)

;-----------------------------------------------------------
;         new-menu items
;-----------------------------------------------------------

(
    setf n-item
    (
        send menu-item-proto
            :new "Normal"
            :action #'(lambda()
              (
                cond
                ( (eq front-dist nil) nil )
                ( t (send front-dist :close-all-windows) )
              )
              (setf front-dist (send normal :new))
            ) ; lambda
    )
)

(
    setf log-n-item
    (
        send menu-item-proto
            :new "Log normal"
            :action #'(lambda()
              (
                cond
                ( (eq front-dist nil) nil )
                ( t (send front-dist :close-all-windows) )
              )
              (setf front-dist (send log-n :new))
            ) ; lambda
    )
)

(
    setf t-item
    (
        send menu-item-proto
            :new "T"
            :action #'(lambda()
              (
                cond
                ( (eq front-dist nil) nil )
                ( t (send front-dist :close-all-windows) )
              )
              (setf front-dist (send t-dist :new))
            ) ; lambda
    )
)

(
    setf chi-item
    (
        send menu-item-proto
            :new "Chi-squared"
            :action #'(lambda()
              (
                cond
                ( (eq front-dist nil) nil )
                ( t (send front-dist :close-all-windows) )
              )
              (setf front-dist (send chi-sq :new))
            ) ; lambda
    )
)

(
    setf f-item
    (
        send menu-item-proto
            :new "F"
            :action #'(lambda()
              (
                cond
                ( (eq front-dist nil) nil )
                ( t (send front-dist :close-all-windows) )
              )
              (
                cond
                ( (eq (choose-param-type "f" (list "mu & si2" "v1 & v2")) 0)
                  (setf front-dist (send f-mu-si2 :new))
                )
                ( t (setf front-dist (send f-v1-v2 :new)))
              )
            ) ; lambda
    )
)

(
    setf g-item
    (
        send menu-item-proto
            :new "Gamma"
            :action #'(lambda()
              (
                cond
                ( (eq front-dist nil) nil )
                ( t (send front-dist :close-all-windows) )
              )
              (
                cond
                ( (eq (choose-param-type "Gamma" (list "mu & si2" "v1 & v2")) 0)
                  (setf front-dist (send g-mu-si2 :new))
                )
                ( t (setf front-dist (send g-v1-v2 :new)))
              )
            ) ; lambda
    )
)

(
    setf b-item
    (
        send menu-item-proto
            :new "Beta"
            :action #'(lambda()
              (
                cond
                ( (eq front-dist nil) nil )
                ( t (send front-dist :close-all-windows) )
              )
              (
                cond
                ( (eq (choose-param-type "Beta" (list "mu & si2" "v1 & v2")) 0)
                  (setf front-dist (send b-mu-si2 :new))
                )
                ( t (setf front-dist (send b-v1-v2 :new)))
              )
            ) ; lambda
    )
)

;-----------------------------------------------------------
;         show-menu items
;-----------------------------------------------------------

( setf show-item 
  (send menu-item-proto
    :new "Show"
    :action
      #'(lambda()
          (
            cond
            ( (eq front-dist nil) nil )
            ( t (send front-dist :do-command 'show-solution) )
          )
        ) ; lambda
  ) ; send menu-item-proto
)

;-----------------------------------------------------------
;         menus installation
;-----------------------------------------------------------

( send misc-menu :append-items info-item exit-item )
( send misc-menu :install )

( send new-menu :append-items n-item log-n-item t-item chi-item f-item g-item b-item )
( send new-menu :install )

( send solution-menu :append-items show-item )
( send solution-menu :install )
