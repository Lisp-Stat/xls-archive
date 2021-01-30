;;;
;;; Utilities for CEREBRUM
;;;


(load "pp")  ; The pretty-printing file


;; Some functions to make XLISP act like Common Lisp


(defun copy-list (lis)
 (let ((result nil))
  (dolist (item lis)
   (setq result (append result (list item))))
  result))

(defun copy-tree (lis)
 (let ((result nil))
  (if (atom lis)
      (setq result lis)
      (dolist (item lis)
       (if (atom item)
           (setq result (append result (list item)))
           (setq result (append result (list (copy-tree item)))))))
  result))

#|
(defun position (item lis)
 (dotimes (i (length lis) nil)
  (if (equal (nth i lis) item)
      (return i))))
|#

(defun substitute (new old lis)
 (let ((result nil))
  (dolist (item lis)
   (if (equal old item)
    (setq result (append result (list new)))
    (setq result (append result (list item)))))
  result))


;; This one is from the common.lsp file that came with XLISP 2.1d


(defmacro with-open-file (stream-file-args &rest body)
 (let ((stream (first stream-file-args))
       (file-args (rest stream-file-args)))
  `(let ((,stream (open ,@file-args)))
    (unwind-protect 
     (progn ,@body)
     (when ,stream (close ,stream))))))


(defun make-list (size &key ((:initial-element initial-element) nil))
 (let ((result nil))
  (dotimes (i size result)
   (setq result (cons initial-element result)))))


;; Graphing stuff


#|
(load "turtle")  ; Load the turtlegraphis file


(setq white 15
      black 0
      blue 1
      green 2
      red 4
      grey 8
      orange 12
      brown 6
      violet 13
      yellow 14
      periwinkle 9
      bright-blue 11)


(defun draw-box (&optional (bottom 0) (top 350)
                           (left 0) (right 600)
                           (c white))
;;
;; Draws a box.  Surprise!
;;
 (color c)
 (move left bottom)
 (draw left top)
 (draw right top)
 (draw right bottom)
 (draw left bottom))


(defun graph (max-x max-y y-list &optional (bottom 0) (top 350)
                                           (left 0) (right 600)
                                           (c white))
;;
;; Draws a graph.  Max-x and max-y are the highest possible ratings along
;; each axis, and y-list is a list of y-values that correspond to the integer
;; values of x.
;;
 ;; Don't want any of that nasty zero-division!
 (if (> max-x 0)
     (let* ((x-unit (float (/ (- right left) max-x)))
            (y-unit (float (/ (- top bottom) max-y)))
            (x nil)
            (y nil))
      (color c)
      (setq y (round (+ bottom (* (car y-list) y-unit))))
      (move left y)
      (do ((i 1 (1+ i)))
       ((= i (length y-list)))
       (setq x (round (+ left (* i x-unit))))
       (setq y (round (+ bottom (* (nth i y-list) y-unit))))
       (draw x y)))))
|#
