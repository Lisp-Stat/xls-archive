;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;;     SimView.lsp
;;;;
;;;;                18 Sep 92 ... Cut from simulate.lsp
;;;;                29 Sep 91 ... Patched to work with new cell format.
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(require "simulate")
(require "compare")

(provide "simview")

;;;;


#||          Example


(def simView (send simulator-view-proto :new))

||#

;;;
;;;     __________  SIMULATION VIEWER  ______________
;;;

(defproto simulator-view-proto
  '(simulator     ; the actual simulation object
    menu          ; appearing in menu bar

    cellItems     ; indicate which cell to act upon
    cellDialog    ; the dialog holding the cell view

    multFactor    ; factor associated with repeated choices
    cellAssocList ; labels for chosen cells
   ))

;;
;;     Create
;;

(defun new-simulator-view (&optional simulation)
  (send simulator-view-proto :new simulation))

(defmeth simulator-view-proto :ISNEW (simulation)
  (setf (slot-value 'simulator) simulation)
  (send self :build-menu)
  (send (slot-value 'menu) :install)
)

;;
;;     Access
;;

(defmeth simulator-view-proto :SIMULATOR ()
  (slot-value 'simulator))

(defmeth simulator-view-proto :MENU ()
  (slot-value 'menu))

(defmeth simulator-view-proto :CELL-ITEMS ()
  (slot-value 'cellItems))


(defmeth simulator-view-proto :CELL-SELECTION-LIST ()
  "List of the selected items in current cell view."
  (mapcar #'(lambda (lst)
              (let ((sel (mapcar #'(lambda (item) (send item :value))
                                 (rest lst)))
                    )
                sel))
          (send self :cell-items)
          ))


(defmeth simulator-view-proto :CELL-ASSOC-LIST ()
  "Parsed and checked association list of selected cell types."
  (if (slot-value 'cellAssocList)
      (slot-value 'cellAssocList)
      (setf (slot-value 'cellAssocList) (send self :find-cell-assoc-list))
      ))
            
(defmeth simulator-view-proto :FIND-CELL-ASSOC-LIST ()
  (let* ((aList (send self :raw-cell-assoc-list))
         (cts   (mapcar #'length aList))
         (mult  (which (< 1 cts)))  )
    (when mult
          (setf (slot-value 'multFactor)
                (first (first (select aList (first mult))))))
    (cond
      ((which (= 0 cts))    ; no value selected from some factor
       (message-dialog "Need to select at least one from each factor.")
       nil   )
      ((< 1 (length mult))  ; mult values from at least 2 factors
       (message-dialog "Cannot select more than 1 from two factors.")
       nil   )
      (  t   (labels ((flatten (a &optional soFar)  ; apply #'append aList CRASH!
                               (cond 
                                 ((endp a) soFar)
                                 ((endp (first a)) (flatten (rest a) soFar))
                                 (  t   (flatten (cons (rest (first a)) (rest a))
                                                 (cons (first (first a)) soFar)))
                                 )))
               (flatten aList)))   )))

(defmeth simulator-view-proto :RAW-CELL-ASSOC-LIST ()
  "Association list of selected cell properties."
  (let ((factors  (send (slot-value 'simulator) :factors))
        (selected (send self :cell-selection-list))  )
    (mapcar #'(lambda (f sel)
                (select (send f :tagged-list) (which sel)))
            factors selected)
    ))


(defmeth simulator-view-proto :BUILD-MENU ()
  (let ((menu  (send menu-proto :new "SimView"))
        (sItem (send menu-item-proto :new "Set Sim..."
                     :action #'(lambda ()
                                 (send self :get-sim-dialog))))
        (rItem (send menu-item-proto :new "Run..."
                     :action #'(lambda ()
                                 (send self :run-sim-dialog))))
        (vItem (send menu-item-proto :new "Cell view..."
                     :action #'(lambda ()
                                 (send self :show-cell-dialog))))
        )
    (send menu :append-items
          sItem (send dash-item-proto :new)
          rItem (send dash-item-proto :new)
          vItem )
    (setf (slot-value 'menu) menu)
    ))

;;
;;     VIEW DIALOGS
;;

(defmeth simulator-view-proto :GET-SIM-DIALOG ()
  (let ((sim (get-value-dialog "Enter simulation name."))  )
    (when sim
          (when (slot-value 'simulator) ; had one
                (setf (slot-value 'cellItems) nil)
                (setf (slot-value 'cellDialog) nil)  )
          (setf (slot-value 'simulator) (first sim)) ; wrapped in list
          )))


(defmeth simulator-view-proto :RUN-SIM-DIALOG ()
  (let ((reps (get-value-dialog "Enter number of replications."))  )
    (if reps
        (send (slot-value 'simulator) :run (first reps))
        )))


(defmeth simulator-view-proto :BUILD-CELL-DIALOG-ITEMS ()
  (let ((factors (send (slot-value 'simulator) :factors)))
    (setf (slot-value 'cellitems)
          (mapcar #'(lambda (c)
                      (let ((name (format nil "~s" (send c :name)))
                            (labs (if (dictionary? c)
                                      (send c :keys-as-text)
                                      (send c :text-list)))  )
                        (append 
                         (list (send text-item-proto :new name))
                         (mapcar #'(lambda (l) 
                                     (send toggle-item-proto :new l))
                                 labs)  )))
                  factors)
          )))


(defmeth simulator-view-proto :SHOW-CELL-DIALOG ()
  (setf (slot-value 'cellAssocList) nil)
  (setf (slot-value 'multFactor) nil)
  (unless (slot-value 'cellItems)
          (send self :build-cell-dialog-items))
  (let ((dItems   (slot-value 'cellItems))
        (plotBut  (send modal-button-proto :new "Plot"
                       :action #'(lambda ()
                                   (send self :plot-selected-cells))))
        (writeBut (send modal-button-proto :new "Print"
                        :action #'(lambda ()
                                    (send self :print-selected-cells))))
        (cancel   (send modal-button-proto :new "Cancel"))   )
    (setf (slot-value 'cellDialog)
          (send modal-dialog-proto :new
                (list 
                 (list dItems)
                 (list plotBut writeBut cancel))))
    (send (slot-value 'cellDialog) :modal-dialog)))

 
(defmeth simulator-view-proto :SELECTED-CELLS ()
  "Extract the selected simulator cells using dialog choices."
  (let ((aList (send self :cell-assoc-list))  )
    (if aList
        (send (slot-value 'simulator) :select-cells aList)
        nil
        )))
  

(defmeth simulator-view-proto :PLOT-SELECTED-CELLS ()
  (let ((cells  (send self :selected-cells))  )
    (if cells
        (let* ((nCells (length cells))
               (mf     (slot-value 'multFactor))
               (aList  (send self :cell-assoc-list))
               (title  (remove-if #'(lambda (p)  ; remove repeated factor
                                      (equal (first p) mf)) aList))
               (thePlot
                (if (= nCells 1)
                    (let ((data (send (first cells) :data))  )
                      (if (numberp (first data))
                          (boxplot data)
                          (send self :build-comparison-plot cells)) )
                    (send self :build-comparison-plot cells)))  )
          (send thePlot :title (format nil "~a" title))
          theplot)
        nil
        )))

(defmeth simulator-view-proto :BUILD-COMPARISON-PLOT (cells)
  "Vertical if several cells, horizonal if one."
  (let* ((max    (- (send (slot-value 'simulator) :n-trials) 1))
         (aList  (send self :cell-assoc-list))
         (data   (mapcar #'(lambda(c) (send c :data)) cells))
         (pairs  (remove-if-not
                  #'(lambda (p)  ;which have repeated factor
                      (equal (first p) (slot-value 'multFactor)))
                  aList))
         (item   (first (first data)))
         (nCols  (if (listp item) (length item) 1))
         (col    (if (and (< 1 (length cells)) (listp item)) ; pick one
                     (first (get-value-dialog (format nil
                         "Data has ~d columns. Which to plot?" nCols)))))  )
    (format t "nCols = ~a   nCells is ~a~%" nCols (length cells))
    (send comparison-plot-proto :new
          (cond
            (col  (mapcar #'(lambda (m) (select (transpose m) col)) data))
            ((< 1 nCols) (transpose (first data)))
            ( t   data))
          #'(lambda(i) (send (slot-value 'simulator) 
                             :recover-block-data 
                             (- max i) aList))
          :vertical? (< 1 (length cells))
          :subLabel (if col (format nil "Column ~d shown." col))
          :labels (if (< 1 (length cells))
                      (mapcar #'(lambda (p) (format nil "~a" (cdr p)))
                               (reverse pairs))
                      (mapcar #'(lambda (i) (format nil "~d" i))
                              (iseq nCols))  )
          )))


(defmeth simulator-view-proto :PRINT-SELECTED-CELLS ()
  (mapcar #'(lambda (c)  (send c :print-summary))
          (send self :selected-cells)))

