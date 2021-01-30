;;; Charlie Geyer's cute little normal growing histogram.

(load "mylisp")

(def x (rseq -4 4 100))
(def y (normal-dens x))

(defun histdemo (&key (model #'normal-rand) (n 5) (reps 200))
  (def h-data (apply model (list n)))
  (def h (histogram h-data))
  (send h :num-bins 15)
  (let ((mn (mean h-data))
	(std (standard-deviation h-data)))
    (apply #'send h :range 0 (+ (* '(-4 4) std) mn))
    (send h :clear)
    (send h :add-lines (list (+ (* x std) mn) (/ y std))))

  (break)
  (dotimes (i reps)
	   (add-data (apply model (list n))))
  )

(defun add-data (new-data)
  (def h-data (append h-data new-data))
  (let ((mn (mean h-data))
	(std (standard-deviation h-data)))
    (apply #'send h :range 0 (+ (* '(-4 4) std) mn))
    (send h :add-points new-data)
    (send h :clear-lines)
    (send h :add-lines (list (+ (* x std) mn) (/ y std)))))


(defun more (&key (model #'normal-rand) (n 5) (reps 200))
  (dotimes (i reps)
	   (add-data (apply model (list n)))))


;(setf (function normal-box) #'normal-rand)


(def xx (normal-rand 100))
(def zz (normal-rand 100))

(def yy zz)


(defun set-yy (r)
  (setq yy (+ (* r xx)
	      (* (sqrt (- 1 (* r r))) zz))))


(defun correllation-demo ()
 (def gra-xxyy (plot-points xx yy))
 (make-r-slider))

(defun new-r (r)
  (set-yy r)
  (replot))

(defun replot ()
  (send gra-xxyy :clear)
  (send gra-xxyy :add-points xx yy))


(defun multiply-all (x)
  (setq xx (* x xx))
  (setq yy (* x yy))
  (setq zz (* x zz))
  (replot))

(defun make-r-slider ()
  (def r-slider (interval-slider-dialog '(-1 1) :points 101
					:action #'new-r
					:title "Correlation Coefficient"
					:value 0)))

