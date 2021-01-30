(defvar Plot-tool-menu (send menu-proto :new "Plot-tools"))

(setf color-16-menu-item 
      (send menu-item-proto :new "Color16 Palette" 
           :action #'(lambda () 
                       (setf c-16 (send graph-proto :new 0
                                        :location (list 523 448)
                                        :size (list 100 29)
                                        :title "Color16"
                                        ))
                      (send c-16 :use-color t)
                      (setf color-16-overlay (send color-16-overlay-proto :new))
                      (send c-16 :add-overlay color-16-overlay)
                      (defmeth c-16 :redraw ()
                         (call-next-method)
                         (send color-16-overlay :redraw))
                      (defmeth c-16 :redraw-overlays ())                     
                      )))

(setf gray-16-menu-item 
      (send menu-item-proto :new "Gray16 Palette" 
           :action #'(lambda () 
                       (setf g-16 (send graph-proto :new 0
                                        :location (list 523 448)
                                        :size (list 100 29)
                                        :title "Gray 16"
                                        ))
                       (send g-16 :use-color t)
                       (setf gray-16-overlay (send gray-16-overlay-proto :new))
                       (send g-16 :add-overlay gray-16-overlay)
                       (defmeth g-16 :redraw ()
                         (call-next-method)
                         (send gray-16-overlay :redraw))
                       (defmeth g-16 :redraw-overlays ())
                       )))

(setf color-64-menu-item 
      (send menu-item-proto :new "64 Palette"
           :action #'(lambda () 
                      (setf w-64 (send graph-proto :new 0
                                        :location (list 536 300)
                                        :size (list 87 87)
                                        :title "64 Palette"
                                        ))
                      (setf color-64-overlay (send color-64-overlay-proto :new))
                      (send w-64 :add-overlay color-64-overlay)
                      (defmeth w-64 :redraw ()
                         (call-next-method)
                         (send color-64-overlay :redraw))
                      (defmeth w-64 :redraw-overlays ())
                      )))

(setf symbol-menu-item 
      (send menu-item-proto :new "Symbol Palette" 
           :action #'(lambda () 
                       (setf symbol-12 (send graph-proto :new 0
                                        :location (list 536 245)
                                        :size (list 87 29)
                                        :title "Symbol"
                                        ))
                       (setf symbol-overlay (send symbol-overlay-proto :new))
                       (send symbol-12 :add-overlay symbol-overlay)
                       (defmeth symbol-12 :redraw ()
                         (call-next-method)
                         (send symbol-overlay :redraw))
                       (defmeth symbol-12 :redraw-overlays ())                     
                       )))

(setf CAP-menu-item
      (send menu-item-proto
            :new "Close All Plots"
            :action #'(lambda () (close-all-plots))
            :enabled t))

(setf DASH1-menu-item (send dash-item-proto :new))
(setf DASH2-menu-item (send dash-item-proto :new))
(setf DASH3-menu-item (send dash-item-proto :new))
(setf DASH4-menu-item (send dash-item-proto :new))

(setf Remove-menu-item
      (send menu-item-proto 
            :new "Remove Menu"
            :action #'(lambda ()
                        (send Plot-tool-menu :delete-items
                              color-16-menu-item
                              gray-16-menu-item
                              DASH1-menu-item
                              color-64-menu-item
                              DASH2-menu-item
                              symbol-menu-item
                              DASH3-menu-item
                              CAP-menu-item
                              DASH4-menu-item
                              Remove-menu-item
                              )
                        (send Plot-tool-menu :remove))))

(send Plot-tool-menu :append-items
                              color-16-menu-item
                              gray-16-menu-item
                              DASH1-menu-item
                              color-64-menu-item
                              DASH2-menu-item
                              symbol-menu-item
                              DASH3-menu-item
                              CAP-menu-item
                              DASH4-menu-item
                              Remove-menu-item
      )
(send Plot-tool-menu :install)
;===============================================================

(setf comp-color-16-list '((0 0 0) (0.25 0.25 0.25) (0.5 0.5 0.5) 
                    (0.75 0.75 0.75) 
                    (0.75 0.25 0.25) (0.5 0 0) (0 0.5 0) (0 1 0)
                    (0 1 1) (0 0 1) (0.5 0 0.5) (1 0 1) 
                    (1 0 0) (1 0.5 0.5) (1 1 0) (1 1 1)))

(setf color-16-list (list 'c-16-1  'c-16-2  'c-16-3  'c-16-4
                          'c-16-5  'c-16-6  'c-16-7  'c-16-8
                          'c-16-9  'c-16-10 'c-16-11 'c-16-12
                          'c-16-13 'c-16-14 'c-16-15 'c-16-16))

(dotimes (i 16)
         (setf comp-color-16 (nth i comp-color-16-list))
         (make-color (nth i color-16-list) (nth 0 comp-color-16) 
                     (nth 1 comp-color-16) (nth 2 comp-color-16)))

(defproto color-16-overlay-proto '() nil graph-overlay-proto)

(defmeth color-16-overlay-proto :which-color (x y)
    (cond
      ((and (<  3 x 12) (<  2 y 13)) (select color-16-list   0))
      ((and (< 15 x 24) (<  2 y 13)) (select color-16-list   1))
      ((and (< 27 x 36) (<  2 y 13)) (select color-16-list   2))
      ((and (< 39 x 48) (<  2 y 13)) (select color-16-list   3))
      ((and (< 51 x 60) (<  2 y 13)) (select color-16-list   4))
      ((and (< 63 x 72) (<  2 y 13)) (select color-16-list   5))
      ((and (< 75 x 84) (<  2 y 13)) (select color-16-list   6))
      ((and (< 87 x 96) (<  2 y 13)) (select color-16-list   7))
      ((and (<  3 x 12) (< 14 y 25)) (select color-16-list   8))
      ((and (< 15 x 24) (< 14 y 25)) (select color-16-list   9))
      ((and (< 27 x 36) (< 14 y 25)) (select color-16-list  10))
      ((and (< 39 x 48) (< 14 y 25)) (select color-16-list  11))
      ((and (< 51 x 60) (< 14 y 25)) (select color-16-list  12))
      ((and (< 63 x 72) (< 14 y 25)) (select color-16-list  13))
      ((and (< 75 x 84) (< 14 y 25)) (select color-16-list  14))
      ((and (< 87 x 96) (< 14 y 25)) (select color-16-list  15))
      ))

(defmeth color-16-overlay-proto :redraw ()
  (let* (
         (graph (send self :graph))
         (color (send graph :draw-color))
         )
    (send graph :use-color t)
    (dotimes (i 16)
             (send graph :draw-color 'black)
             (when (< i 8) (send graph :frame-rect (+ 3 (* i 12)) 3 10 10))
            (when (> i 7) (send graph :frame-rect(+ 3 (* (- i 8) 12)) 16 10 10))
             )
    (dotimes (i 16)
             (send graph :draw-color (nth i color-16-list))
             (when (< i 8) (send graph :paint-rect (+ 4 (* i 12)) 4 8 8))
             (when (> i 7)(send graph :paint-rect (+ 4 (* (- i 8) 12)) 17 8 8))
             )
    (send graph :draw-color color)
    ))

(defmeth color-16-overlay-proto :do-click (x y m1 m2)
  (let* (
         (graph (send self :graph))
         (link-list (linked-plots))
         (color (send self :which-color x y))
         hilight
         )
    (when (and link-list color)
          (setf hilight (send (nth 0 link-list) :points-selected))
          (dolist (plot link-list)
                  (send plot :use-color t)
                  (send plot :point-color hilight color)
                  (send plot :redraw-content)
                  (send plot :points-selected hilight)
                  ))))
;===============================================================

(setf comp-gray-16-list '((0 0 0) (0.15 0.15 0.15) (0.2 0.2 0.2) 
                    (0.25 0.25 0.25) (0.3 0.3 0.3) (0.4 0.4 0.4) (0.5 0.5 0.5) 
                    (0.55 0.55 0.55) (0.6 0.6 0.6) (0.65 0.65 0.65) 
                    (0.7 0.7 0.7) (0.75 0.75 0.75) 
                    (0.8 0.8 0.8) (0.85 0.85 0.85) (0.9 0.9 0.9) (1 1 1)))

(setf gray-16-list (list 'g-16-1  'g-16-2  'g-16-3  'g-16-4
                         'g-16-5  'g-16-6  'g-16-7  'g-16-8
                         'g-16-9  'g-16-10 'g-16-11 'g-16-12
                         'g-16-13 'g-16-14 'g-16-15 'g-16-16))

(dotimes (i 16)
         (setf comp-gray-16 (nth i comp-gray-16-list))
         (make-color (nth i gray-16-list) (nth 0 comp-gray-16) 
                     (nth 1 comp-gray-16) (nth 2 comp-gray-16)))

(defproto gray-16-overlay-proto '() nil graph-overlay-proto)

(defmeth gray-16-overlay-proto :which-color (x y)
    (cond
      ((and (<  3 x 12) (<  2 y 13)) (select gray-16-list   0))
      ((and (< 15 x 24) (<  2 y 13)) (select gray-16-list   1))
      ((and (< 27 x 36) (<  2 y 13)) (select gray-16-list   2))
      ((and (< 39 x 48) (<  2 y 13)) (select gray-16-list   3))
      ((and (< 51 x 60) (<  2 y 13)) (select gray-16-list   4))
      ((and (< 63 x 72) (<  2 y 13)) (select gray-16-list   5))
      ((and (< 75 x 84) (<  2 y 13)) (select gray-16-list   6))
      ((and (< 87 x 96) (<  2 y 13)) (select gray-16-list   7))
      ((and (<  3 x 12) (< 14 y 25)) (select gray-16-list   8))
      ((and (< 15 x 24) (< 14 y 25)) (select gray-16-list   9))
      ((and (< 27 x 36) (< 14 y 25)) (select gray-16-list  10))
      ((and (< 39 x 48) (< 14 y 25)) (select gray-16-list  11))
      ((and (< 51 x 60) (< 14 y 25)) (select gray-16-list  12))
      ((and (< 63 x 72) (< 14 y 25)) (select gray-16-list  13))
      ((and (< 75 x 84) (< 14 y 25)) (select gray-16-list  14))
      ((and (< 87 x 96) (< 14 y 25)) (select gray-16-list  15))
      ))

(defmeth gray-16-overlay-proto :redraw ()
  (let* (
         (graph (send self :graph))
         (color (send graph :draw-color))
         )
    (send graph :use-color t)
    (dotimes (i 16)
             (send graph :draw-color 'black)
             (when (< i 8) (send graph :frame-rect (+ 3 (* i 12)) 3 10 10))
            (when (> i 7) (send graph :frame-rect(+ 3 (* (- i 8) 12)) 16 10 10))
             )
    (dotimes (i 16)
             (send graph :draw-color (nth i gray-16-list))
             (when (< i 8) (send graph :paint-rect (+ 4 (* i 12)) 4 8 8))
             (when (> i 7)(send graph :paint-rect (+ 4 (* (- i 8) 12)) 17 8 8))
             )
    (send graph :draw-color color)
    ))

(defmeth gray-16-overlay-proto :do-click (x y m1 m2)
  (let* (
         (graph (send self :graph))
         (link-list (linked-plots))
         (color (send self :which-color x y))
         hilight
         )
    (when (and link-list color)
          (setf hilight (send (nth 0 link-list) :points-selected))
          (dolist (plot link-list)
                  (send plot :use-color t)
                  (send plot :point-color hilight color)
                  (send plot :redraw-content)
                  (send plot :points-selected hilight)
                  ))))
;===============================================================

(setf r-list (repeat (list 0 (/ 1 3) (/ 2 3) 1) (list 16 16 16 16)))

(setf g-list (repeat (repeat (list 0 (/ 1 3) (/ 2 3) 1) (list 4 4 4 4)) 4))

(setf b-list (repeat (list 0 (/ 1 3) (/ 2 3) 1) 16))

(setf comp-color-64-list-1 (transpose (list r-list g-list b-list)))

(setf comp-color-64-list   (append (select comp-color-64-list-1 (iseq  0  7))
                                   (select comp-color-64-list-1 (iseq 16 23))
                                   (select comp-color-64-list-1 (iseq 32 39))
                                   (select comp-color-64-list-1 (iseq 48 55))
                                   (select comp-color-64-list-1 (iseq 8 15))
                                   (select comp-color-64-list-1 (iseq 24 31))
                                   (select comp-color-64-list-1 (iseq 40 47))
                                   (select comp-color-64-list-1 (iseq 56 63))))

(setf color-64-list (list
                          'c-64-1   'c-64-2   'c-64-3 'c-64-4   'c-64-5  'c-64-6  'c-64-7  'c-64-8
                          'c-64-9   'c-64-10 'c-64-11 'c-64-12  'c-64-13 'c-64-14 'c-64-15 'c-64-16
                          'c-64-17  'c-64-18 'c-64-19 'c-64-20  'c-64-21 'c-64-22 'c-64-23 'c-64-24
                          'c-64-25  'c-64-26 'c-64-27 'c-64-28  'c-64-29 'c-64-30 'c-64-31 'c-64-32
                          'c-64-33  'c-64-34 'c-64-35 'c-64-36  'c-64-37 'c-64-38 'c-64-39 'c-64-40
                          'c-64-41  'c-64-42 'c-64-43 'c-64-44  'c-64-45 'c-64-46 'c-64-47 'c-64-48
                          'c-64-49  'c-64-50 'c-64-51 'c-64-52  'c-64-53 'c-64-54 'c-64-55 'c-64-56
                          'c-64-57  'c-64-58 'c-64-59 'c-64-60  'c-64-61 'c-64-62 'c-64-63 'c-64-64
                          ))

(dotimes (i 64)
         (setf comp-color-64 (nth i comp-color-64-list))
         (make-color (nth i color-64-list) 
            (nth 0 comp-color-64) (nth 1 comp-color-64) (nth 2 comp-color-64)))

(defproto color-64-overlay-proto '() nil graph-overlay-proto)

(defmeth color-64-overlay-proto :which-color (x y)
  (let*(
        (loc-x (floor (/ (- x 4) 10)))
        color
        )
    (when (and (< 3 x 84) (< 3 y 84))
          (when (< 3 y 14)
                (setf color (select color-64-list loc-x)))
          (when (< 13 y 24)
                (setf color (select color-64-list (+ 8 loc-x))))
          (when (< 23 y 34)
                (setf color (select color-64-list (+ 16 loc-x))))
          (when (< 33 y 44)
                (setf color (select color-64-list (+ 24 loc-x))))
          (when (< 43 y 54)
                (setf color (select color-64-list (+ 32 loc-x))))
          (when (< 53 y 64)
                (setf color (select color-64-list (+ 40 loc-x))))
          (when (< 63 y 74)
                (setf color (select color-64-list (+ 48 loc-x))))
          (when (< 73 y 84)
                (setf color (select color-64-list (+ 56 loc-x))))
          )
    color))

(defmeth color-64-overlay-proto :redraw ()
  (let* (
         (graph (send self :graph))
         (color (send graph :draw-color))
         )
    (send graph :use-color t)
    (dotimes (i 64)
             (send graph :draw-color (nth i color-64-list))
             (when (< i 8)     (send graph :paint-rect (+ 4 (*    i     10))  4 8 8))
             (when (<  7 i 16) (send graph :paint-rect (+ 4 (* (- i  8) 10)) 14 8 8))
             (when (< 15 i 24) (send graph :paint-rect (+ 4 (* (- i 16) 10)) 24 8 8))
             (when (< 23 i 32) (send graph :paint-rect (+ 4 (* (- i 24) 10)) 34 8 8))
             (when (< 31 i 40) (send graph :paint-rect (+ 4 (* (- i 32) 10)) 44 8 8))
             (when (< 39 i 48) (send graph :paint-rect (+ 4 (* (- i 40) 10)) 54 8 8))
             (when (< 47 i 56) (send graph :paint-rect (+ 4 (* (- i 48) 10)) 64 8 8))
             (when (> i 55)    (send graph :paint-rect (+ 4 (* (- i 56) 10)) 74 8 8))
             )
    (send graph :draw-color color)
    ))

(defmeth color-64-overlay-proto :do-click (x y m1 m2)
  (let* (
         (graph (send self :graph))
         (link-list (linked-plots))
         (color (send self :which-color x y))
         hilight
         )
    (when (and link-list color)
          (setf hilight (send (nth 0 link-list) :points-selected))
          (dolist (plot link-list)
                  (send plot :use-color t)
                  (send plot :point-color hilight color)
                  (send plot :redraw-content)
                  (send plot :points-selected hilight)
                  ))))
;===============================================================

(defproto symbol-overlay-proto '() nil graph-overlay-proto)

(defmeth symbol-overlay-proto :which-symbol (x y)
    (cond
      ((and (<  3 x 12) (<  2 y 13)) (select *PLOT-SYMBOLS*   0))
      ((and (< 15 x 24) (<  2 y 13)) (select *PLOT-SYMBOLS*   1))
      ((and (< 27 x 36) (<  2 y 13)) (select *PLOT-SYMBOLS*   2))
      ((and (< 39 x 48) (<  2 y 13)) (select *PLOT-SYMBOLS*   3))
      ((and (< 51 x 60) (<  2 y 13)) (select *PLOT-SYMBOLS*   4))
      ((and (< 63 x 72) (<  2 y 13)) (select *PLOT-SYMBOLS*   5))
      ((and (<  3 x 12) (< 14 y 25)) (select *PLOT-SYMBOLS*   6))
      ((and (< 15 x 24) (< 14 y 25)) (select *PLOT-SYMBOLS*   7))
      ((and (< 27 x 36) (< 14 y 25)) (select *PLOT-SYMBOLS*   8))
      ((and (< 39 x 48) (< 14 y 25)) (select *PLOT-SYMBOLS*   9))
      ((and (< 51 x 60) (< 14 y 25)) (select *PLOT-SYMBOLS*  10))
      ((and (< 63 x 72) (< 14 y 25)) (select *PLOT-SYMBOLS*  11))
      ))

(defmeth symbol-overlay-proto :redraw ()
  (let* (
         (graph (send self :graph))
         )
    (dotimes (i 12)
             (when (< i 6) (send graph :frame-rect (+ 3 (* i 12)) 3 10 10))
            (when (> i 5) (send graph :frame-rect(+ 3 (* (- i 6) 12)) 16 10 10))
             )
    (dotimes (i 12)
             (when (< i 6) (send graph :draw-symbol (nth i *PLOT-SYMBOLS*) 
                                 nil (+ 8 (* i 12)) 8))
             (when (> i 5) (send graph :draw-symbol (nth i *PLOT-SYMBOLS*) 
                                 nil (+ 9 (* (- i 6) 12)) 22))
             )))

(defmeth symbol-overlay-proto :do-click (x y m1 m2)
  (let* (
         (graph (send self :graph))
         (link-list (linked-plots))
         (symbol (send self :which-symbol x y))
         hilight
         )
    (when (and link-list symbol)
          (setf hilight (send (nth 0 link-list) :points-selected))
          (dolist (plot link-list)
                  (send plot :depth-cuing nil)
                  (send plot :point-symbol hilight symbol)
                  (send plot :redraw-content)
                  (send plot :points-selected hilight))
          t)))
