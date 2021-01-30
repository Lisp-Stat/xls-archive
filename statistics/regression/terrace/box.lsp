;;;  
;;;    Reads and structures data so that it can be used by TERRACE
;;;


(defun defcol (col)
   (def *tmp* (rest col))
   (eval (list 'def (first col) '*tmp*))
   (undef '*tmp*)
   (first col))

(defun read-master (file number-of-columns)
   (mapcar #'defcol (read-data-columns file number-of-columns)))

(read-master "box.data" 8)

(def box-case
  (let ((y-box (combine (transpose (list w0 w1 w2 w3 w4))))
        (case-id (repeat (iseq 1 27) (repeat 5 27)))
        (time (repeat (iseq 5) 27))
        (time2 (repeat (list 0 1 4 9 16) 27)))
    (bind-columns case-id time time2 y-box)))
(def box-unit (bind-columns (iseq 1 27) z1 z2))


;;;    make terrace model
(require "ter2")

(setf box (make-terrace box-unit box-case 
            :x-labels (list "Intercept" "Time" "Time^2")
            :z-labels (list "BaseLine" "Thyroxin" "Thiouracil")))
