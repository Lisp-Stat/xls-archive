(require "spreadsheet")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; The item-analysis-proto is a template for doing all kinds of item
;; analysis. It inherits from the multi-variable-proto, and can thus
;; be spreadsheet-edited.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defproto item-analysis-proto
  '(row-sums column-sums)
  () multi-variable-proto)

(defmeth item-analysis-proto :isnew (data)
  (call-next-method data)
  (send self :set-row-sums
        (send self :make-row-sums))
  (send self :set-column-sums
        (send self :make-column-sums))
  )

(defmeth item-analysis-proto :make-row-sums ()
  (apply #'+ (send self :set-data)))

(defmeth item-analysis-proto :make-column-sums ()
  (element-seq (mapcar #'sum (send self :set-data))))

(make-assessors item-analysis-proto
                (data) (row-sums column-sums))


(provide "item-analysis")








