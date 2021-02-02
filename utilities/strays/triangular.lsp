;;; Triangular graph proto ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Prototype to produce plots for tree variables with
;;; constant sum=1, using triangular coordinates. If the
;;; constant sum is different from 1, needs changes.
;;;
;;; Variable 1 mapped to upper corner, variable 2 to
;;; right corner, variable 3 to left corner.
;;;
;;; Constructor function #'triangular takes one data argument,
;;; assumed to be a list of 3 lists. Also takes additional
;;; arguments, which are sent to the :new method.
;;;
;;; Usage:
;;; (triangular col)
;;; (triangular col :title "Colors in rgb coo"
;;;                 :variable-labels '("Red" "Green" "Blue"))
;;;
;;; Kjetil Halvorsen, La Paz, november 1997
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defproto triangular-proto () () graph-proto)

(send triangular-proto :title "Triangular graph")

(send triangular-proto :fixed-aspect t)

(send triangular-proto :menu-title "Triangular")

(defmeth triangular-proto :x-axis (&rest args)
  (princ "Not meaningful for triangulat graph") nil)

(defmeth triangular-proto :y-axis (&rest args)
  (princ "Not meaningful for triangulat graph") nil)

(defmeth triangular-proto :isnew (&rest args)
  (apply #'call-next-method 3 args)
  (send self :transformation
        '#2a((0.5         1 0)
             (0.866025403 0 0)
             (1           1 1))  :draw nil))

(defmeth triangular-proto :adjust-to-data (&key (draw t))
   (call-next-method :draw nil)
  (send self :scaled-range 0 -0.1 1.1)
  (send self :scaled-range 1 -0.1 1))

;;; lets redefine :redraw-content, to put axis labels, if defined:

(defmeth triangular-proto :redraw-content ()
(call-next-method)
(let ((labels (send self :variable-label (iseq 3))))
(when (or (> (length (first labels))  0)
          (> (length (second labels)) 0)
          (> (length (third labels))  0))
      (let ((posx (send self :scaled-to-canvas 0.5 0.866025))
            (posy (send self :scaled-to-canvas 1 0))
            (posz (send self :scaled-to-canvas 0 0)))
        (send self :draw-text (first labels) (first posx)
                                             (second posx) 1 0)
        (send self :draw-text (second labels) (first posy)
                                             (second posy) 1 1)
        (send self :draw-text (third labels) (first posz)
                                             (second posz) 1 1)))))



(defun triangular (data &rest args)
"Args: (data &rest args) Constructs triangular co-ordinate plot
 for tree variables with constant sum 1.
 data: list of 3 lists.
 args: sent to the :new method.     "
  (unless (and (= 3 (length data))
               (listp (first data)) (listp (second data))
               (listp (third data)))
      (error "Triangular called with invalid data"))
  (let ((p (apply #'send triangular-proto :new args)))
    (send p :add-points data :draw nil)
    (send p :adjust-to-data :draw nil)
    (send p :add-lines '((1 0 0 1) (0 1 0 0) (0 0 1 0)))
  p))


;; Artificial test-data:

; (def x '((0.16666666666666666 0.5 0.3333333333333333 0.6666666666666666) (0.5 0.3333333333333333 0.3333333333333333 0.16666666666666666) (0.3333333333333333 0.16666666666666666 0.3333333333333333 0.3333333333333333)))

;; remember that this prototype assumes that the three variables
;; always sum to 1!

;; (triangular x)
            

;; test-data:

#|
(def col '((0.47 0.70 0.40 0.33 0.49)
           (0.47 0.26 0.40 0.33 0.02)
           (0.06 0.04 0.20 0.33 0.49)))
|#

;;; This are the colors canary, orange, brown, white, violet
;;; expresed as fractions of red-green-blue.   

#|
(triangular col :title "Colors in rgb coo"
                 :variable-labels '("Red" "Green" "Blue"))
|#             
