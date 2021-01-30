;;;
;;; AGNES-PLOT
;;;

;;; rutinas para producir dendrograma a partir del output 
;;; que produce agnes.

;;; version 0.1, La Paz, julio 1997.

;;; Kjetil Halvorsen.

;;; Rutina principal: agnes-plot-list, que toma un argumento, la lista 
;	de output de agnes.
;	El efecto de esta rutina es definir un variable global, *draw-list*,
;	que es una lista de rango 3, que tiene pares de puntos, que qualquuier
;	rutina para dibujar puede usar. (otras rutinas no son necesarios para el
;	usuario y deberia estar ocultados en un "package".

; las rutinas usa tambien otras variable globales. *start*,*pasos ,para comunicacion. 
; (deberia evitar esto, introduciendo un objeto tipo cluster para representar 
; al analisis cluster).

; uso: asumiendo que el resultado de usar agnes esta en una lista RES, se da 
; los comandos: (agnes-plot-list RES)
;	(setf agnes-plot (send agnes-plot-proto :new))

;ENGLISH: Asuming that the result of using agnes is in the list RES, use the 
;	commands:
;	(agnes-plot-list RES)
;	(setf agnes-plot (send agnes-plot-proto :new))


(def *draw-list* () )
(def *start* ()	)
(def *pasos* () )

(defun ind (last)
(let ((n (length last))
(p ))
(dotimes (i n)
(setf p (combine p (position i last))))
(remove-if #'(lambda (x) (eql x nil)) p ))) 

(defun tomar2 (a)
(let (( p ()))
(dolist (i a)
(push (second i) p))
p))

(defun tomar1 (a)
(let (( p ()))
(dolist (i a)
(push (first i) p))
p))

(defun step1 (lab)
(let* ((a (select *pasos* lab))
(a2 (tomar2 a))
(a1 (tomar1 a))
(xmax (max a1))
(amin (min a2))
(amax (max a2))
(nco (/ (+ amax amin) 2))
(an (mapcar #'(lambda (item)
(list (+ 1 xmax) nco)) a))
(to-draw (transpose-list (list a an))))
(mapcar #'(lambda (item) (push item *draw-list*)) 
to-draw )
(dolist (i lab)
(setf (select *pasos* i)(pop an )))))



(defun agnes-plot-list (res)
"syntax: (agnes-plot-list res)
where res es la lista de output de agnes." (let* ((n (length res))
(f (first res))
(last (first (elt res (- n 1))))
(st (ind last))
(start (mapcar #'(lambda (x) (list 1 x)) st))) (setf *start* last)
(setf *pasos* (copy-list start))
(setf *draw-list* () )
(pop res)	; el primer elemento de res es in-interesante
(dolist (ll res)
(step1 (first ll)))
(setf *draw-list* (reverse *draw-list*)))) 


;;;;
;;;; Codigo para actualmente dibujar
;;;;

(defproto agnes-plot-proto '(agnes-max-x 
agnes-max-y
origen-x-pixels
origen-y-pixels
max-x-pixels
max-y-pixels) () graph-window-proto
"Objeto para representar dendrogramas de aglomeraciones	producido por agnes"
)

(defmeth agnes-plot-proto :new ()
(let (( a (call-next-method)))
(if (boundp '*draw-list*)
(let* ((td (transpose *draw-list*))
(tdlist (append (first td) (second td))) (x-max (max (first (transpose tdlist)))) (y-max (max (second (transpose tdlist)))) )
(send a :agnes-max-x x-max)
(send a :agnes-max-y y-max)
(send a :title "Agnes-plot") )
(error "*draw-list* no existe") )
a
))



(defmeth agnes-plot-proto :agnes-max-x (&optional (val nil)) 
(if val (setf (slot-value 'agnes-max-x) val)) 
(slot-value 'agnes-max-x))

(defmeth agnes-plot-proto :agnes-max-y (&optional (val nil)) 
(if val (setf (slot-value 'agnes-max-y) val)) 
(slot-value 'agnes-max-y))

(defmeth agnes-plot-proto :origen-x-pixels (&optional (val nil)) 
(if val (setf (slot-value 'origen-x-pixels) val)) 
(slot-value 'origen-x-pixels))

(defmeth agnes-plot-proto :origen-y-pixels (&optional (val nil)) 
(if val (setf (slot-value 'origen-y-pixels) val)) 
(slot-value 'origen-y-pixels))

(defmeth agnes-plot-proto :max-x-pixels (&optional (val nil)) 
(if val (setf (slot-value 'max-x-pixels) val)) 
(slot-value 'max-x-pixels))

(defmeth agnes-plot-proto :max-y-pixels (&optional (val nil)) 
(if val (setf (slot-value 'max-y-pixels) val)) 
(slot-value 'max-y-pixels))


(defmeth agnes-plot-proto :resize ()
(send self :origen-x-pixels
(/ (send self :canvas-width) 10))
(send self :origen-y-pixels
(/ (send self :canvas-height) 10))
(send self :max-x-pixels
(* (send self :canvas-width) 0.9))
(send self :max-y-pixels
(* (send self :canvas-height) 0.9)) )

(defmeth agnes-plot-proto :redraw ()
(let ((x-mult (/ (- (send self :max-x-pixels) 
(send self :origen-x-pixels))
(- (send self :agnes-max-x) 1)))
(y-mult (/ (- (send self :max-y-pixels)
(send self :origen-y-pixels))
(send self :agnes-max-y) )))
(flet ((xp (x) (round (+ (send self :origen-x-pixels) 
(* (- x 1) x-mult))))
(yp (y) (round (+ (send self :origen-y-pixels) 
(* y y-mult)))))
(dolist (i *draw-list*)
(send self :draw-line
(round (xp (first (first i))) )
(round (yp (second (first i))))
(round (xp (first (second i)) ))
(round (yp (second (second i)))) ))
(let ((contador -1))
(dolist (i *start*)
(setf contador (1+ contador))
(send self :draw-text (num-to-string i)
(xp 1) (yp contador) 2 0))))))