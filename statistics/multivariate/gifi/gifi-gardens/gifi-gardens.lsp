;;;
;;;     Gifi-Gardens, a place to grow nonlinear multivariate
;;;     analyses of the Gifi variety, by James Hilden-Minton.
;;;

(provide "gifi-gardens")

;;   
;;    Define the flowerbed of the analysis, gifi-center-proto.
;;

(defproto gifi-center-proto
  '(title         ; name for analysis
    raw-data      ; a list of columns, interpreted as variables
    n             ; number of records (rows of raw-data)
    p             ; number of solutions sought
    obj-scores    ; X matrix (n by p)
    obj-labels    ; list of strings or index to column of raw-data
    obj-weights   ; interpreted as (repeat 1 n) if nil
    in-analysis   ; list of gifi-vars in the analysis
    active        ; Boolean list for in-analysis 
    loss-history  ; list of total-loss
    stream        ; output stream for report generation
    graphs        ; list of gifi-graphs
  )
  () ()
"Gifi-center interacts with gifi-vars to yeild a gifi-analysis.")

(require "g-center")

(defun gifi-center 
  (raw-data &key (title "garden") (p 2) obj-labels obj-weights file-name)
  (let ((raw-data (cond
                    ((matrixp raw-data) raw-data)
                    ((vectorp raw-data) (bind-columns raw-data))
                    (t (apply #'bind-columns raw-data))))
        (m (send gifi-center-proto :new)))
    (send m :title title)
    (send m :raw-data raw-data)
    (send m :n t)
    (send m :p p)
    (send m :obj-labels obj-labels)
    (send m :obj-weights obj-weights)
    (send m :open-file file-name)
    m))


;;
;;    Define most generic species, gifi-var-proto.
;;    This will be literal (matrix) interpretation of
;;    Gifi (pp 167-171, 1990).
;;

(defproto gifi-var-proto
  '(var-label     ; name of var, string with no white spaces
    var-level     ; descriptor of level or type of analysis
    center        ; gifi-center for linked analysis
    column-ind    ; index of column of raw-data to be interpreted
    coded-data    ; G, indicator matrix
    diagonal      ; diagonal of D = G'G
    d-inv-g-prime ; D^-1*G'
    quant-scores  ; Y, category quantifications
    quant-labels  ; list of strings
    active-values ; Boolean for present values, (repeat t n) if nil 
  )
  () ()
"gifi-var linked with a gifi-center codes data and updates
quantifications.")

(require "g-var")

(send gifi-var-proto :slot-value 'var-level "VAR--UNCODED")

(defmeth gifi-var-proto :isnew 
  (center col &key var-label active-values (active t))
  (setf (slot-value 'var-label)
    (cond 
      ((stringp var-label) var-label)
      ((slot-value 'var-label) (slot-value 'var-label))
      (t (format nil "gifi-var-~d"
           (length (send center :slot-value 'in-analysis))))))
  (setf (slot-value 'center) center)
  (setf (slot-value 'column-ind) col)
  (if active-values
    (send self :active-values active-values))
  (unless (member self (send center :slot-value 'in-analysis))
    (send center :add-gifi-var self :active active)))

;;
;;    Define a more efficient categorical variable, gifi-cat-proto.
;;    Data is represented as indices, rather than matrices.
;;

(defproto gifi-cat-proto
  '(raw-values)   ; list of lists of original values
  ()
  gifi-var-proto
"gifi-cat is useful for multiple nominal representations.")

(send gifi-cat-proto :slot-value 'var-level "CAT--UNCODED")

(defmeth gifi-cat-proto :isnew (&rest args)
  (require "g-cat")
  (apply #'call-next-method args))

;;
;;    Define an extension of gifi-cat with rank one restriction,
;;    gifi-single-proto.
;;

(defproto gifi-single-proto 
  '(loadings      ; a'
    centroids     ; matrix of centroids, for quant-scores use accessor
  )
  ()
  gifi-cat-proto
"Gifi-single is useful for variables with rank one restriction.")

(send gifi-single-proto :slot-value 'var-level "SINGLE--UNCODED")

(defmeth gifi-single-proto :isnew (&rest args)
  (require "g-single")
  (apply #'call-next-method args)
  (setf (slot-value 'loadings)
    (make-array (list 1 (send (slot-value 'center) :p))
      :initial-element 1)))

;;
;;    Define an ordinal and numerical extensions of gifi-single, 
;;    gifi-single-ordinal-proto and gifi-single-numerical-proto.
;;

(defproto gifi-single-ordinal-proto 
  '(restricted-levels)  ; number of levels with ordinal restrictions
                        ; all other will be nominal or passive.
  ()
  gifi-single-proto
"Gifi-single-ordinal-proto is useful for ordinal variables")

(defproto gifi-single-numerical-proto 
  () () gifi-single-ordinal-proto
"Gifi-single-numerical-proto is used for numerical casting of variables.")

; Particular methods in g-single.

;;
;;    Define an extension of gifi-var suitible for fuzzy coding,
;;    gifi-fuzzy-proto.
;;

(defproto gifi-fuzzy-proto  
  '(knots               ; bspline knots including exterior knots
    order               ; order of spline
    graph-raw           ; independent var for plotting curve
    graph-data          ; matrix for ploting curve
  )
  () gifi-var-proto
"Gifi-fuzzy-proto treats the slot DIAGONAL as an arbitrary matrix,
not neccessarily diagonal, good for fuzzy codings.")

(defmeth gifi-fuzzy-proto :isnew (&rest args)
  (require "g-fuzzy")
  (apply #'call-next-method args))


;;
;;    Define gifi-graph-proto.
;;

(defproto gifi-graph-proto 
  '(center               ; gifi-center
    obj-points           ; begin and end indices for object-points
    quant-points         ; list of begin and end indices 
    quant-stars          ; ditto
    quant-curves         ; ditto
    quant-to-centroids   ; ditto
    centroid-points      ; ditto
    centroid-stars       ; ditto
    loadings-arrows      ; list of (linestart line-end head-point) indices 
    loadings-scale       ; a number
    file-prefix          ; string for generating gnu-plot file names
  )
  ()
  scatterplot-proto
"Gifi-graph interacts with a gifi-center and makes gnu-plot files")

(require "g-graph")

(defmeth gifi-graph-proto :isnew (center &rest args)
  (send self :gifi-center center)
  (send center :add-graph self)
  (apply #'call-next-method (send self :hub :p) args)
  (unless (member ':title args)
    (send self :title "Gifi Graph"))
  (unless (member ':variable-labels args)
    (let* ((ind (iseq (send self :hub :p)))
           (labels (mapcar
                     #'(lambda (x) (format nil "Dimension ~d" x))
                     (+ 1 ind))))
      (send self :variable-label ind labels))))

;;
;;     Define gifi-quant-graph-proto
;;

(defproto gifi-quant-graph-proto
  '(gifi-var            ; gifi-var to which graph is related
  )
  ()
  scatterplot-proto
"Gifi-quant-graph interacts with a gifi-var to plot quantifications.")

(defmeth gifi-quant-graph-proto :isnew (gv &rest args)
  (require "g-graph2")
  (send self :gifi-var gv)
  (apply #'call-next-method (send self :get-num-var) args)
  (send self :set-title)
  (send self :set-variable-label))

(defproto gifi-coding-graph-proto () () gifi-quant-graph-proto
"Gifi-coding-graph-proto displays fuzzy coding.")

(defmeth gifi-coding-graph-proto :isnew (&rest args)
  (if (kind-of-p (car args) gifi-fuzzy-proto)
    (apply #'call-next-method args)
    (error "Not gifi-fuzzy - ~a" (car args))))


