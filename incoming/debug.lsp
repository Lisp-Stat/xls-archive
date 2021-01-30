
;;;
;;; A basic menu for debugging commands
;;;


(setf debug-menu (send menu-proto :new "Debug"))


;;;
;;; Debug menu items:
;;;

(setf debug-item (send menu-item-proto :new "Debug"
                 :action #'debug))

(setf nodebug-item (send menu-item-proto :new "Nodebug"
                 :action #'(lambda () 
                             (nodebug)
                             (clean-up)
                             (top-level))))
                             

(setf baktrace-item (send menu-item-proto :new "Baktrace"
                 :action #'baktrace))

(setf toplevel-item (send menu-item-proto :new "Toplevel"
                 :action #'top-level))

(setf clean-up-item (send menu-item-proto :new "Clean-up"
                 :action #'clean-up))

(setf dash-item (send dash-item-proto :new))

;; must append items here in the undef part if 
;;  new items are introduced! (undef doesnt work)
(setf remove-item (send menu-item-proto :new "Remove"
                :action #'(lambda ()
                   (send debug-menu :remove)
                   (send debug-menu :dispose))))

(setf continue-item (send menu-item-proto :new "Continue"
                :action #'continue))

(setf close-plots-item (send menu-item-proto :new "Close all plots"
                :action #'close-all-plots))

;;; this is the basic debug commands, might need some more...

;;;
;;; support for enabling/disabling items:
;;;

(defmeth debug-menu :update ()
   (send debug-item    :enabled  (not *breakenable*))
   (send nodebug-item  :enabled *breakenable*)
   (send baktrace-item :enabled *breakenable*)
   (send toplevel-item :enabled *breakenable*)
   (send clean-up-item :enabled *breakenable*)
   (send continue-item :enabled *breakenable*))

;;;
;;; Innstalling items in menu:
;;;

(send debug-menu :install)

(send debug-menu :append-items debug-item
                               nodebug-item
                               baktrace-item
                               toplevel-item
                               clean-up-item
                               continue-item
                               dash-item
                               close-plots-item
                               remove-item
)

;;; EOF ;;;
