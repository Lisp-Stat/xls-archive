#|
    ICONS

    11 Jul 94 ... Add :do-drop-on to handle drag and drop; :draw-center.
    17 Jan 94 ... Maintain selection order (:update-select-list)
    24 Aug 93 ... Fix the eraser icon behavior using XOR drawing.
    18 May 93 ... Remove handler mixin;  copy/cut/paste within window; 
    26 Apr 93 ... Faster drawing modes, shift-click.
    20 Apr 93 ... Lots of changes, particularly via arrow.
    16 Apr 91 ... Some method polishing. Double click
     7 Apr 91 ... Created using graph overlays.  Use icond handler mixin
|#

(provide "icons")
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  ICONS  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Questions:  Dynamic reparenting to change shape?
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;;   Globals provided
;;;;
;;;;       *active-icon-window*   -- most recent window activated/clicked
;;;;
(setf *active-icon-window* nil)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defproto ICON-PROTO
  '(location    ; location in the drawing window
    size        ; size of the icon
    window      ; in what window do I reside
    name        ; what is my name
    menu        ; what is my popup menu
    color       ; my color
    state       ; selected, normal, hidden...
    drawFast?   ; use speed-up drawing?
                ))


(defmeth icon-proto :ISNEW (window
                            &key (state 'normal)
                                 (color 'cyan)
                                 (name "Icon")
                                 (location '(0 0))
                                 (size     '(40 25))  )
  (setf (slot-value 'window) window)
  (setf (slot-value 'state) state)
  (setf (slot-value 'drawFast?) nil)
  (setf (slot-value 'color) color)
  (setf (slot-value 'location) location)
  (setf (slot-value 'size) size)   ; set size first, setting name adjusts
  (send self :name-is name)
  (send window :add-icon self)
  (send self :build-menu)
  )


(defmeth icon-proto :COPY (&key window)
;  builds a copy of itself using its own prototype
  (let ((theCopy (send (first (send self :parents))
                       :new
                       (if window
                           window
                           (send self :window))
                       :state (slot-value 'state)
                       :color (slot-value 'color)
                       :name (slot-value 'name)
                       :location (slot-value 'location)
                       :size     (slot-value 'size)  )
                 ))
    (send self :copy-slots theCopy)
    ))

(defmeth icon-proto :COPY-SLOTS (to)
  )  

(defmeth icon-proto :LOCATION ()
  (slot-value 'location))
(defmeth icon-proto :LOCATION-IS (loc)
  (setf (slot-value 'location) loc))

(defmeth icon-proto :CENTER ()              ; 7/11/94
  (+ (slot-value 'location)
     (floor (/ (slot-value 'size) 2))))

(defmeth icon-proto :SIZE ()
  (slot-value 'size))
(defmeth icon-proto :SIZE-IS (size)
  (setf (slot-value 'size) size))

(defmeth icon-proto :NAME ()
  (slot-value 'name))
(defmeth icon-proto :NAME-IS (name)
  (setf (slot-value 'name) name)
  (let ((sz (slot-value 'size))
        (ln (first (send self :name-size)))  )
    (send self :size-is (list (max ln (first sz)) (second sz)))
    (send self :erase)
    (send self :draw)
    ))
  

(defmeth icon-proto :WINDOW ()
  (slot-value 'window))
(defmeth icon-proto :WINDOW-IS (window)
  (setf (slot-value 'window) window))

(defmeth icon-proto :COLOR ()
  (slot-value 'color))
(defmeth icon-proto :COLOR-IS (color)
  (setf (slot-value 'color) color)
  (if (slot-value 'window)
      (send self :draw)
      ))

(defmeth icon-proto :STATE ()
  (slot-value 'state))
(defmeth icon-proto :STATE-IS (state &key (draw t))
  (if (eq state 'hidden)
      (send self :erase)
      (progn
       (when draw  (send self :erase))
       (setf (slot-value 'state) state)
       (when draw  (send self :draw))
       )))

(defmeth icon-proto :DRAW-FAST? ()
  (slot-value 'drawFast?))
(defmeth icon-proto :DRAW-FAST-IS (fast?)
  (unless (eq fast? (slot-value 'drawFast?))
          (send self :erase)  ; get rid of old image
          (setf (slot-value 'drawFast?) fast?)
          (send self :draw)   ; draw in new mode
          ))

#||
Decided to use an internal slot for drawing in a fast
mode.  Alternatives were to have two drawing functions
(which had lots of redundant code) or to have an argument
to drawing, which generally was superfluous.
||#

;;  Menu

(defmeth icon-proto :BUILD-MENU ()
  "Builds the pop-up menu for the icon."
  (let* ((menu (send menu-proto :new "Icon Menu"))
         (cFun #'(lambda ()
                   (let ((c (choose-item-dialog "Choose icon color" 
                              (mapcar #'string *colors*) :initial
                              (position (send self :color) *colors*) )))
                     (when c
                           (send self :color-is (nth c *colors*))
                           ))))
         (item (send menu-item-proto :new "Color" :action cFun))  )
    (send menu :append-items item)
    menu))


(defmeth icon-proto :MENU ()
  (slot-value 'menu))
(defmeth icon-proto :MENU-IS (menu)
  (setf (slot-value 'menu) menu))

;;     EXPLODE

        
(defmeth icon-proto :EXPLODE ()
  (let ((w (send self :window))
        (c (+ (send self :location)
              (round (* .5 (send self :size)))))   )
    (flet ((points ()
                 (split-list 
                  (+ (repeat c 10)
                     (round (* 10 (normal-rand 20)))  )
                  2)))
      (dotimes (i 3)
             (send w :draw-color 'yellow)
               (send w :paint-poly (points))
               (send w :draw-color 'red)
               (send w :paint-poly (points))
               )
      (send w :draw-color 'black)
      (mapcar #'(lambda (p) (apply #'send w :draw-point p)) (points))
      )))



;;     NAME

(defmeth icon-proto :NAME-LOCATION ()  ; in local coor system
  (list 5 10) )

(defmeth icon-proto :NAME-SIZE ()
  (let* ((window  (slot-value 'window))
         (name   (slot-value 'name))
         (name-w (send window :text-width name))
         (name-h (send window :text-ascent))
         (pad     (floor (/ name-h 2)))
         (descent (send window :text-descent))
         (width   (+ name-w (* 3 pad)))
         (height  (+ name-h descent (* 2 pad)))  )
    (list width height)
    ))

(defmeth icon-proto :DRAW-NAME ()
  (let ((window (slot-value 'window))
        (name  (slot-value 'name))
        (at     (+ (send self :location) (send self :name-location)))  )
    (apply #'send window :draw-string name at)
    ))


;;  Selection and the State

(defmeth icon-proto :SELECTED? ()
  (eq 'selected (slot-value 'state)))

(defmeth icon-proto :HIDDEN? ()
  (eq 'hidden (slot-value 'state)))

(defmeth icon-proto :UNSELECT () 
  (when (send self :selected?)
        (send self :state-is 'normal :draw t)
        ))

(defmeth icon-proto :SELECT () 
  ; Complements selection status."
  (send self :state-is
        (if (eq 'selected (slot-value 'state))
                'normal
                'selected)
            ))


;;    Drawing and Erasing

(defmeth icon-proto :DRAW ()
  (unless (eq 'hidden (slot-value 'state))
          (if (slot-value 'drawFast?)
              (send self :draw-center)
              (if (send self :selected?)
                  (send self :fill-shape))  )
          (send self :draw-shape)
          (unless (slot-value 'drawFast?)
                  (send self :draw-name))
          ))

(defmeth icon-proto :DRAW-CENTER ()
  (let* ((w (slot-value 'window))
         (center (+ (slot-value 'location)
                    (floor (/ (slot-value 'size) 2))))   )
    (apply #'send w :draw-line (+ '(3 0 -3 0)
                                  (select center '(0 1 0 1))))
    (apply #'send w :draw-line (+ '(0 3 0 -3)
                                  (select center '(0 1 0 1))))
    ))
    
(defmeth icon-proto :DRAW-SHAPE ()
  () )

(defmeth icon-proto :FILL-SHAPE ()
  () )


(defmeth icon-proto :ERASE ()
  (apply #'send (slot-value 'window)
         :erase-rect (send self :extent-rect)))

(defmeth icon-proto :MOVE-TO (x y)
  ; Use Xor drawing so that not like an eraser.
  (send (slot-value 'window) :draw-mode 'xor)
  (send self :draw)
  (send self :location-is (list x y))
  (send self :draw)
  (send (slot-value 'window) :draw-mode 'normal)
  )

;(defmeth icon-proto :OLD-MOVE-TO (x y)
;  (send self :erase)
;  (send self :location-is (list x y))
;  (send self :draw)
;  )

(defmeth icon-proto :EXTENT-RECT ()
  (append (slot-value 'location) (send self :size))
  )



;;     MOUSE RESPONSES

 
(defmeth icon-proto :DO-CLICK (x y sh op)
  (cond 
    (op     (send self :do-option-click x y))
    (sh     (send self :do-shift-click x y))
    (t (let* ( (window (send self :window))  ; move me around
               (state  (slot-value 'state))
               (loc    (slot-value 'location))
               (xOff   (- x (first loc)))
               (yOff   (- y (second loc)))  )
         (send self :select)
         (send self :draw-fast-is t)        ; use fast drawing while drag
         (send window :while-button-down
               #'(lambda (x y)
                   (send self :move-to (- x xOff) (- y yOff))))
         (send self :draw-fast-is nil)
         (let ((icon (apply #'send window :icon-under-point   ; 7/25/94
                            (send self :center)) ))
           (when (not (equal icon self))
                (send self :do-drop-on icon)
                (send self :location-is loc)  ))
         (send window :redraw)              ; clean up trash from dragging
       ))))

(defmeth icon-proto :DO-DROP-ON (icon)
  )

(defmeth icon-proto :DO-OPTION-CLICK (x y)
;  Pops up a menu for the item; alter by changing the menu setup function.
  (send (slot-value 'menu) :popup x y (slot-value 'window))
  )

(defmeth icon-proto :DO-SHIFT-CLICK (x y)
  (send self :select)
  )

(defmeth icon-proto :DO-DOUBLE-CLICK (x y sh op)
  (format t "Icon ~a double clicked...~%" (send self :name))
  )


;;;;;;;;;;;;;;;;;;;;   OVAL MIXIN   ;;;;;;;;;;;;;;;;;;;;;;;;;


(defproto OVAL-SHAPE-MIXIN
  ()
  ()
  )

(defmeth oval-shape-mixin :SHAPE ()
  'oval)

(defmeth oval-shape-mixin :DRAW-SHAPE ()
  (let ((window (slot-value 'window))
        (rect   (send self :extent-rect)) )
    (if (eq (slot-value 'state) 'normal)
        (send window :draw-color (slot-value 'color)))
    (apply #'send window :frame-oval rect)
    (send window :draw-color 'black)
    ))


(defmeth oval-shape-mixin :FILL-SHAPE ()
  (let ((window (send self :window))
        (rect   (send self :extent-rect)) )
    (send window :draw-color (slot-value 'color))
    (apply #'send window :paint-oval rect)
    (send window :draw-color 'black)
    ))


(defmeth oval-shape-mixin :HAS-POINT? (x y)
  (let* ((rect   (send self :extent-rect))
         (left   (first rect))
         (right  (+ left (third rect)))
         (top    (second rect))
         (bottom (+ top (fourth rect)))  )
    (if (and (< left x right) (< top y bottom))
        t
        nil)
    ))


(defproto oval-icon-proto
  ()
  ()
  (list oval-shape-mixin icon-proto)
  )


;;;;;;;;;;;;;;;;;;;;   RECTANGLE MIXIN   ;;;;;;;;;;;;;;;;;;;;;;;;;


(defproto RECTANGLE-SHAPE-MIXIN
  ()
  ()
  )

(defmeth rectangle-shape-mixin :SHAPE ()
  'rectangle)

(defmeth rectangle-shape-mixin :DRAW-SHAPE ()
  (let ((window (slot-value 'window))
        (rect   (send self :extent-rect)) )
    (if (eq (slot-value 'state) 'normal)
        (send window :draw-color (slot-value 'color)))
    (apply #'send window :frame-rect rect)
    (send window :draw-color 'black)
    ))


(defmeth rectangle-shape-mixin :FILL-SHAPE ()
  (let ((window (send self :window))
        (rect   (send self :extent-rect)) )
    (send window :draw-color (slot-value 'color))
    (apply #'send window :paint-rect rect)
    (send window :draw-color 'black)
    ))


(defmeth rectangle-shape-mixin :HAS-POINT? (x y)
  (let* ((rect   (send self :extent-rect))
         (left   (first rect))
         (right  (+ left (third rect)))
         (top    (second rect))
         (bottom (+ top (fourth rect)))  )
    (if (and (< left x right) (< top y bottom))
        t
        nil)
    ))

(defproto rectangle-icon-proto
  ()
  ()
  (list rectangle-shape-mixin icon-proto)
  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defproto ARROW-PROTO
  ()
  ()
  icon-proto)


(defmeth arrow-proto :ISNEW (window leftPoint rightPoint)
  (call-next-method
               window
               :location leftPoint
               :size (- rightPoint leftPoint)
               ))

(defmeth arrow-proto :COPY (&key window)
  (send arrow-proto :new
        (if window
            window
            (send self :window))
        (send self :location)
        (+ (send self :location)
           (send self :size))
           ))


(defmeth arrow-proto :HAS-POINT? (x y)
  (let* ((tol  5)                            ; how close for valid selection
         (dev  10)
         (x (- x (first  (slot-value 'location)))); shift relative to origin
         (y (- y (second (slot-value 'location))))
         (sx (first (slot-value 'size)))
         (sy (second (slot-value 'size)))  )
    (if (< 0 sx)
        (if (< 0 x sx)   ; new point between
            (setf dev (- y (* (/ sy sx) x)))   )
        (if (< sx x 0)
            (setf dev (- y (* (/ sy sx) x)))   )  )
    (if (< (abs dev) tol)
        t nil)))
            


(defmeth arrow-proto :ERASE ()
  (let* ((selected? (send self :selected?))
         (fromX (first (slot-value 'location)))
         (fromY (second (slot-value 'location)))
         (toX   (+ fromX (first (slot-value 'size))))
         (toY   (+ fromY (second (slot-value 'size)))) 
         (w     (send self :window))   )
    (unless (slot-value 'drawFast?)
            (send w :draw-mode 'xor)
            (send self :draw)
            (send w :draw-mode 'normal)  )
    (if selected? (send w :line-width 2))
    (send w :draw-line fromX fromY toX toY)
    (send w :draw-mode 'xor)   ; pick up pieces of line
    (send w :draw-line fromX fromY toX toY)
    (send w :draw-mode 'normal)
    (if selected?  (send w :line-width 1))
    ))


(defun atan-deg (dy dx)
;  Inverse tangent in degrees
  (+ 180 (* (/ 180 pi)
            (if (= 0 dx) 
                (if (> 0 dy)
                    (/ pi 2)
                    (- (/ pi 2))  )
                (atan (/ dy dx))
                ))))
      

(defmeth arrow-proto :DRAW ()
  (let* ((select? (send self :selected?))
         (fromX (first (slot-value 'location)))
         (fromY (second (slot-value 'location)))
         (toX   (+ fromX (first (slot-value 'size))))
         (toY   (+ fromY (second (slot-value 'size))))
         (w     (send self :window))   )
    (send w :draw-line fromX fromY toX toY)
    (unless (slot-value 'drawFast?)
            (when select?
                  (send w :line-width 2)
                  (send w :draw-line fromX fromY toX toY)
                  (send w :line-width 1)  )
            (let* ((deg    30)   ; degrees of rotation for arrow
                   (size   30)   ; size of bounding rectangle
                   (dx     (- toX fromX))
                   (dy     (- fromY toY))   ; screen coor run down
                   (theta  (atan-deg dy dx))   )
              (if (> dx 0)
                  (send w :paint-arc (- toX 15) (- toY 15)  size size
                        (- theta (/ deg 2))  deg)
                  (send w :paint-arc (- toX 15) (- toY 15)  size size
                        (- (- theta 180) (/ deg 2))  deg)
                  ))
            )))


(defmeth arrow-proto :FILL-SHAPE ()
  (let* ((fromX (first (slot-value 'location)))
         (fromY (second (slot-value 'location)))
         (toX   (+ fromX (first (slot-value 'size))))
         (toY   (+ fromY (second (slot-value 'size))))
         (w     (send self :window))   )
    (send w :line-width 3)
    (send w :draw-line fromX fromY toX toY)
    (send w :line-width 1)  ))

 
;;;;;;;;;;;;;;;;;;;;;;;;;;;  ICON WINDOW  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;                       (to be added to a graph-window-proto descendant)
;;   "Class of objects that know how to manage icons."
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(setf *edit-list* ())    ; Global shared edit list.


(defproto icon-window-proto
  '(selectList     ; list of icons in order of selection
    iconBuilder    ; item to clone when inserting new icons
    iconList       ; the list of icons begin managed
    lastClickTime  ; time of the last mouse click
    lastClickX     ; where the last click occured
    lastClickY)
  ()
  graph-window-proto)


;;     Initialize

(defmeth icon-window-proto :ISNEW (iconBuilder)
  (call-next-method)
  (setf (slot-value 'iconBuilder) iconBuilder)
  (send self :use-color t)
  (setf (slot-value 'lastClickTime) 0)
  (setf (slot-value 'lastClickX) -777)
  (setf (slot-value 'lastClickY) -777)
  (send self :build-menu))

(defmeth icon-window-proto :COPY-WINDOW ()
  (let ((w (send (first (send self :parents)) :new)))
    (send self :icons-do
          #'(lambda(i) (send i :copy :window w))  )
    (send w :title "Copy")
    ))

;(defmeth icon-window-proto :ACTIVATE (arg) 
;  (setf *active-icon-window* self)         ; only works for Mac
;  (call-next-method arg)
;  )

(defmeth icon-window-proto :CLOSE ()
  (if (equal self *active-icon-window*)
      (setf *active-icon-window* nil))
  (call-next-method)
  )

;;     Add / remove icons

(defmeth icon-window-proto :ADD-NEW-ICON ()
  (funcall (slot-value 'iconBuilder) self))
 
(defmeth icon-window-proto :ADD-ICON (icon)
  (setf (slot-value 'iconList) (cons icon (slot-value 'iconList)))
  (send icon :draw)
  )

(defmeth icon-window-proto :REMOVE-ICON (icon)
  (setf (slot-value 'iconList) (delete icon (slot-value 'iconList)))
  (send icon :window-is nil)
  )

;;     Cut and Paste

(defmeth icon-window-proto :CUT-SELECTED-ICONS ()
  (setf *edit-list*                 ; find selected icons
        (send self :icons-in-state 'selected))
  (mapcar #'(lambda (icon)
              (send icon :explode) (pause 20)
              (send icon :state-is 'hidden)
              )  ; hide them
          *edit-list*)
  (setf (slot-value 'iconList)                  ; remove from display list
        (remove-if #'(lambda (i) (member i *edit-list*))
                   (slot-value 'iconList)))
  (send self :redraw)
  )
         
(defmeth icon-window-proto :COPY-SELECTED-ICONS ()
  (setf *edit-list* (send self :icons-in-state 'selected)))


(defmeth icon-window-proto :PASTE-ICONS ()
;  Pastes copy from the edit list, not from the current selection.
;  Not so efficient since may only need to turn on, but very simple.
  (mapcar #'(lambda (icon) (send icon :copy :window self))
          *edit-list*))



;;     Mouse Handling and Selection

(defmeth icon-window-proto :HAS-SELECTED-ICON? ()
  (if (slot-value 'selectList) t nil))

(defmeth icon-window-proto :HAS-EDIT-ICON? ()
  (if *edit-list*
      t nil))


(defmeth icon-window-proto :ICON-UNDER-POINT (x y)
  (dolist (icon (slot-value 'iconList) nil)
          (if (and (not (send icon :hidden?))
                   (send icon :has-point? x y))
              (return icon)
              )))

(defmeth icon-window-proto :ICONS-IN-STATE (state)
  ;  List of the icons in the window in state 'state'.
  (if (eq state 'selected)  ; use selection order
      (reverse (slot-value 'selectList))
      (remove-if-not #'(lambda(i) (eq state (send i :state)))
                     (slot-value 'iconList)))  )
  

(defmeth icon-window-proto :ICONS-DO (f)
  (if (slot-value 'iconList)
      (mapcar f (slot-value 'iconList))  
      ))


(defmeth icon-window-proto :DO-CLICK (x y sh op)
  (setf *active-icon-window* self)                   ; here for Dos/Unix
  (let ((time (get-internal-real-time))
        (icon (send self :icon-under-point x y))  )
    (when icon  ; send it click or double-click
          (let* ((tDev (- time (slot-value 'lastClickTime)))
                 (xDev (abs (- x (slot-value 'lastClickX))))
                 (yDev (abs (- y (slot-value 'lastClickY))))  )
            (if (and (< xDev 2) (< yDev 2) (< tDev 25))
                (send icon :do-double-click x y sh op)
                (send icon :do-click x y sh op)   )
            ))
    (unless sh
            (mapcar #'(lambda (i) (send i :unselect)) 
                    (remove icon (slot-value 'iconList))
                    ))
    (send self :update-select-list icon sh op)
    (setf (slot-value 'lastClickTime) time)
    (setf (slot-value 'lastClickX   )  x)
    (setf (slot-value 'lastClickY   )  y)
    ))
    
(defmeth icon-window-proto :UPDATE-SELECT-LIST (icon sh op)
  ; Maintains the window's selection list. Assumes that 'icon'
  ; has been sent a do-click message. If icon is nil, resets.
  (setf (slot-value 'selectList)
        (if icon
            (if sh 
                (if (send icon :selected?) 
                    (cons icon (slot-value 'selectList)) ; add to selList
                    (remove icon (slot-value 'selectList)))
                (if (send icon :selected?)
                    (list icon)
                    nil)  )
            nil)))
                       

(defmeth icon-window-proto :REDRAW ()
  (call-next-method)
  (send self :erase-window)
  (send self :icons-do
        #'(lambda (icon)  (send icon :draw))   )
  )
 

;;     __________  ICON WINDOW MENU  __________

(defproto SELECTED-ITEM-PROTO
  '(window)
  ()
  menu-item-proto)

(defmeth selected-item-proto :WINDOW-IS (window)
  (setf (slot-value 'window) window)
  )

(defmeth selected-item-proto :UPDATE ()
  (send self :enabled
        (if (send (slot-value 'window) :has-selected-icon?)
              t nil)))


(defproto PASTE-ITEM-PROTO
  '(window)
  ()
  menu-item-proto)

(defmeth paste-item-proto :WINDOW-IS (window)
  (setf (slot-value 'window) window)
  )

(defmeth paste-item-proto :UPDATE ()
  (send self :enabled
        (if (send (slot-value 'window) :has-edit-icon?)
            t nil)))

(defmeth icon-window-proto :BUILD-MENU ()
  (let* ((nItem (send menu-item-proto :new "New icon"
                      :action #'(lambda ()
                                  (send self :add-new-icon))))
         (cItem (send selected-item-proto :new "Copy Selection"
                      :action #'(lambda ()
                                  (send self :copy-selected-icons))))
         (xItem (send selected-item-proto :new "Cut Selection"
                      :action #'(lambda ()
                                  (send self :cut-selected-icons))))
         (pItem (send paste-item-proto :new "Paste Icons"
                      :action #'(lambda ()
                                  (send self :paste-icons))))
         (menu  (send menu-proto :new "Icons"))
         )
    (send cItem :window-is self)
    (send xItem :window-is self)
    (send pItem :window-is self)                              
    (send menu :append-items
          nItem 
          (send dash-item-proto :new) 
          cItem xItem pItem
          (send dash-item-proto :new)  )
    (send self :menu menu)
    ))

           

#||

  (def f #'(lambda (w)  ; function called when build new icon
             (send rectangle-icon-proto :new
                   w :name "New" :location '(120 0))))

  (def w  (send icon-window-proto :new f))
  (send w :title "An Icon Window")

  (def i1 (send rectangle-icon-proto :new w 
                :name "icon1" :location '(  0   0) ))
  (def i2 (send rectangle-icon-proto :new w 
                :name "icon2222" :location '( 50  50) ))
  (def i3 (send oval-icon-proto      :new w 
                :name "icon3" :location '(100  50) ))
  (def a1 (send arrow-proto :new w '(20 20)  '(100 100)))

   (send i1 :copy)
   (send a1 :copy)

  (def w2 (send icon-window-proto :new))
  (send w2 :title "Copy")     

   (send w  :copy-window)


  (send a1 :has-point? 50 50)

  (send w :icons-in-state 'selected)


;;;;
;;;;     Bitmaps and double buffering
;;;;

(def w (send graph-window-proto :new))

(send w :size)
(send w :canvas-width)

(defmeth w :redraw ()
  (send self :paint-rect 100 100 150 150)
  )

(defmeth w :do-click (x y s o)
  (format t "~a~%" 
          (send w :drag-grey-rect 100 100 20 20)))

(dotimes (i 10)
         (send w :start-buffering)
         ; (send w :erase-window)
         (send w :frame-rect (* 10 i) 100 20 20)
         (send w :buffer-to-screen)
         )
 
||#
