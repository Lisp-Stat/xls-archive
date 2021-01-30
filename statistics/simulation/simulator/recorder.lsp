;;;;
;;;;     Recorder.lsp   © Robert A. Stine
;;;;                 Objects that record information by
;;;;                 memorization (as in a bag) or by compression.
;;;;
;;;;                  2 Jul 92 ... Merge forms of this file.
;;;;                 28 May 92 ... Modify memorizer plot.
;;;;                 18 Sep 92 ... Subclass from collection class.
;;;;
;;;; Additions to
;;;; XLISP-STAT 2.0 Copyright (c) 1988, by Luke Tierney
;;;; Xlisp 2.0 Copyright (c) 1985, 1987 by David Michael Betz
;;;;    All Rights Reserved
;;;;    Permission is granted for unrestricted non-commercial use

(provide "recorder")

;;;;

(require "collect")

;;;;



#||     EXAMPLES  

  (def tr (send memorizer-proto :new :desc "Test"))

  (def tr (send memorizer-proto :new :desc "Test"
                :locFunc #'median :sclFunc #'interquartile-range))

  (def tr (send condenser-proto :new :desc "Test"))

  (send tr :add '(1 2 3))
  (send tr :add '(4 5 6))
  (send tr :add '(7 8 9))
  (send tr :lands)
  (send tr :print-summary)
  (send tr :plot-summary)
||#



(defproto RECORDER-PROTO
  '(
    desc         ; description field
    labels       ; labels for the recorded items       
  ))

(defmeth recorder-proto :ISNEW (&key desc labels)
  (send self :reset)
  (setf (slot-value 'desc) desc)
  (setf (slot-value 'labels) labels)
  )

(defmeth recorder-proto :DESC ()
  (slot-value 'desc))

(defmeth recorder-proto :DESC-IS (desc)
  (setf (slot-value 'desc) desc))

(defmeth recorder-proto :ELEMENT-SIZE ()                     ;OVERRIDE
  )

(defmeth recorder-proto :LABELS ()
  (if (not (slot-value 'labels))
      (setf (slot-value 'labels)
            (mapcar #'(lambda (i) (format nil "~a" i))
                    (iseq (send self :element-size)))   )
      (slot-value 'labels)
      ))

(defmeth recorder-proto :LABELS-ARE (labels)
  (setf (slot-value 'labels) labels))


(defmeth recorder-proto :RESET ()                ; OVERRIDE
  )

(defmeth recorder-proto :LOCATION ()             ; OVERRIDE
  )

(defmeth recorder-proto :SCALE ()                ; OVERRIDE
  )

(defmeth recorder-proto :LANDS () 
  "List holding n, location, scale, se."
  (let* (( n   (send self :size))
         (loc  (send self :location))
         (scl  (send self :scale))
         (se   (/ scl (sqrt n)))  )
    (list n loc scl se)
    ))


(defmeth recorder-proto :PRINT-SUMMARY ()
  (when (< 0 (send self :size))
        (let* ((desc   (slot-value 'desc))
               (labels (send self :labels))
               (loc    (send self :location))
               (scl    (send self :scale))
               (rows   (1+ (iseq (length loc))))
               (out    (make-array (list (1+ (length loc)) 3)))   )
          (if desc (format t "Description: ~a~%" desc))
          (setf (select out 0 '(0 1 2)) '#2a(("Label" "Location" "Scale")))
          (dotimes (i (length loc))
                   (setf (aref out (1+ i) 0) (select labels i))
                   (setf (aref out (1+ i) 1) (select loc    i))
                   (setf (aref out (1+ i) 2) (select scl    i))  )
          (print-matrix out)
          )))

(defmeth recorder-proto :PLOT-SUMMARY (&key title (color 'black))
  "Wandering mean +/- one scale unit plot."
  (let* ((l  (send self :location))
         (s  (send self :scale))
         (n  (length s))
         (p  (plot-lines (iseq n) l :color color))  )
    (send p :add-points (iseq n) (+ l s) :color color)
    (send p :add-points (iseq n) (- l s) :color color)
    (send p :adjust-to-data)
    (send p :title (format nil "~a: ~a" (slot-value 'desc) title))
    p
    ))
 
(defmeth recorder-proto :WRITE-TO-FILE (fileName)
  (format t "WRITE-TO-FILE: Not implemented~%")
  nil )

(defmeth recorder-proto :READ-FROM-FILE (fileName)
  (format t "READ-FROM-FILE: Not implemented~%")
  nil )

(defmeth recorder-proto :APPEND-TO-STREAM (stream prefix)
  (format t "APPEND-TO-STREAM: Not implemented~%")
  nil )


;;
;;     __________  MEMORIZER RECORDER  __________
;;

(defproto MEMORIZER-PROTO
  '(list locFunc sclFunc)
  ()
  (list  bag-proto recorder-proto))

(defmeth memorizer-proto :ISNEW (&key desc labels
                                      (locFunc #'mean)
                                      (sclFunc #'standard-deviation))
  (setf (slot-value 'desc) desc)
  (setf (slot-value 'labels) labels)   ; RAS??? How to get proper init
  (send self :list-is nil)
  (setf (slot-value 'locFunc) locFunc)
  (setf (slot-value 'sclFunc) sclFunc)
  )


(defmeth memorizer-proto :COLUMN-LIST ()
  (column-list (apply #'bind-rows (send self :list))))

(defmeth memorizer-proto :RESET ()
  (send self :list-is nil))

(defmeth memorizer-proto :ELEMENT-SIZE ()
  (let ((list (send self :list)))
    (if list
        (if (listp (first list))
            (length (first list))
            1))))

(defmeth memorizer-proto :LOCATION ()
    (mapcar (slot-value 'locFunc) (send self :column-list)))

(defmeth memorizer-proto :SCALE ()
    (mapcar (slot-value 'sclFunc) (send self :column-list)))


(defmeth memorizer-proto :PRINT-SUMMARY ()
  (call-next-method)
  (let ((res (slot-value 'list)))
    (when (and res (< 2 (length res)))
          (format t "  First  --> ~a ~%" (first res))
          (format t "  Second --> ~a ~%" (second res))
          (format t "  ~3d th --> ~a ~%" (length res) (first (last res))) )
    (format t "  ~%")
    ))

 
(defmeth memorizer-proto :PLOT-SUMMARY (&key recoverFun title labels
                                             kernels? dots? (boxplots? t)
                                            (vertical? t) (color 'black))
  "Comparison plot of the items."
  (send comparison-plot-proto :new
        (send self :column-list)
        recoverFun
        :vertical? vertical?
        :kernels? kernels? :dots? dots? :boxplots? boxplots?
        :title (format nil "~a: ~a" (slot-value 'desc) title)
        :labels (if labels labels (send self :labels))
  ))

(defmeth memorizer-proto :WRITE-TO-FILE (fileName)
  (with-open-file (file filename :direction :output)
                  (print (slot-value 'list  ) file)
                  (print (slot-value 'desc  ) file)
                  ))

(defmeth memorizer-proto :READ-FROM-FILE (fileName)
  (with-open-file (file fileName)
                  (setf (slot-value 'list ) (read file))
                  (setf (slot-value 'desc ) (read file))
                  ))


(defmeth memorizer-proto :APPEND-TO-STREAM (stream prefix)
  "Append values to the given stream, prefixing each line."
  (let ((desc (if (slot-value 'desc) desc "NA")))
    (mapcar #'(lambda (i) (format stream "~a ~a ~a~%" prefix desc i))
            (send self :list))
    ))


;;
;;     __________  CONDENSER RECORDER  __________
;;

(defproto CONDENSER-PROTO
  '(n ss total)
  ()
  (list recorder-proto))


(defmeth condenser-proto :RESET ()
  (setf (slot-value 'n    )  0  )
  (setf (slot-value 'total)  0.0)
  (setf (slot-value 'ss   )  0.0)
  )

(defmeth condenser-proto :ADD (item)
  (send self :update item)
  (setf (slot-value 'n) (1+ (slot-value 'n)))
  ()  )

(defmeth condenser-proto :UPDATE (item)
  (let* ((n      (slot-value 'n))
         (total  (+ (slot-value 'total) item))
         (aveN   (/ (slot-value 'total) (if (< 0 n) n 1)))
         (aveN+1 (/ total (1+ n)))
         (dev    (- item aveN+1))
         (devAve (- aveN aveN+1))    )
    (setf (slot-value 'total) total)
    (setf (slot-value 'ss)
          (+ (slot-value 'ss) (* dev dev) (* n devAve devAve)))
    ))

(defmeth condenser-proto :SIZE ()
  (slot-value 'n))

(defmeth condenser-proto :ELEMENT-SIZE ()
  (if (slot-value 'total)
      (length (slot-value 'total))))


(defmeth condenser-proto :LOCATION ()
  (/ (slot-value 'total) (slot-value 'n)))

(defmeth condenser-proto :SCALE ()
  (sqrt (/ (slot-value 'ss) (1- (slot-value 'n)))))


(defmeth condenser-proto :WRITE-TO-FILE (fileName)
  (with-open-file (file filename :direction :output)
                  (print (slot-value 'desc  ) file)
                  (print (slot-value 'n     ) file)
                  (print (slot-value 'total ) file)
                  (print (slot-value 'ss    ) file)
                  )
  nil)

(defmeth condenser-proto :READ-FROM-FILE (fileName)
  (with-open-file (file fileName)
                  (setf (slot-value 'desc    ) (read file))
                  (setf (slot-value 'n       ) (read file))
                  (setf (slot-value 'total   ) (read file))
                  (setf (slot-value 'ss      ) (read file))
                  )
  nil)
