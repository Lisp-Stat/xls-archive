;;;;
;;;;     AxisData.lsp
;;;;
;;;;     31 Jan 94 ... File write missing :key; :add-var recognize rename.  
;;;;     29 Jul 93 ... Remove auto-open of next dataset.
;;;;     23 Jul 93 ... Start to polish again.  Use build-alist in read.
;;;;     21 Jul 93 ... Add variable checks for existing name; updates.
;;;;     18 Jul 93 ... Better target checking and labelling.
;;;;      5 Jul 93 ... Add "Define" to special forms in evaluate.
;;;;     26 Jun 93 ... Manage a data set of variables
;;;; 
;;;; ???  Handling missing data with listwise deletion...
;;;;

(require "axisIcon")

;;;;

(provide "axisData")

;;;;

#||

(def ds (make-dataset-from-file))

(def ds (make-dataset-from-values "test"
         '(one) (list '(a b c d e f)))   )



(def ds (make-dataset-from-values "test"
         '(one two) (list (iseq 10) (iseq 10))))

(send ds :add-variable 'trans '(* one two))
(send ds :slot-value 'aList)
(send ds :evaluate 'trans)
(send ds :print-contents :stream t)


||#


;;;;
;;;;  Globals provided
;;;;                    *datasets*  *dataset-menu*
;;;;          used     
;;;;                    *active-icon-window*
;;;;


(defun INSTALL-DATA-MENU ()
  (remove-data-menu)
  (def *datasets* nil)
  (def *dataset-menu* (send menu-proto :new "Data"))
  (let ((readItem  (send menu-item-proto :new "Open dataset file..."
                         :action #'make-dataset-from-file))
        (writeItem (send menu-item-proto :new "Write dataset file..."
                         :action #'write-dataset-to-file))
        )
    (defmeth writeItem :update ()
      (send writeItem :enabled 
            (if *datasets* t nil)))
    (send *dataset-menu* :append-items
          readItem
          writeItem
          (send dash-item-proto :new)  )
    (send *dataset-menu* :install)
    ))

(defun REMOVE-DATA-MENU ()
  (if (member '*dataset-menu* *variables*)
      (send *dataset-menu* :remove)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;                DATASET BUILDERS
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun MAKE-DATASET-FROM-FILE ()
  (let* ((file (open-file-dialog))
         (ds   (when file (send 
                           (send dataset-proto :new file)
                           :read-from-file file)))   )
    (when ds
          (send ds :add-to-menu)
          (send ds :open-window))
    ds))

          
(defun MAKE-DATASET-FROM-VALUES (name vars data &key features)
  (let ((ds (send dataset-proto :new name)) )
    (send ds :build-aList vars data)
    (if features
        (send ds :add-features features))
    (send ds :add-to-menu)
    (send ds :open-window)
    ds
    ))

(defun WRITE-DATASET-TO-FILE ()
  (let ((file (set-file-dialog "Enter new file name" "axis.dat")) )
    (when file
          (with-open-file (f file :direction :output)
                          (send (send *active-icon-window* :dataset)
                                :print-contents :stream f)
                          ))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;                    DATASET WINDOW
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defproto DATASET-WINDOW-PROTO
  '(dataset)
  ()
  icon-window-proto)

(defmeth dataset-window-proto :ISNEW (dataset)
  (setf (slot-value 'dataset) dataset)
  (call-next-method #'(lambda (w)
                        (make-variable-icon "Blank" dataset '(125 0)
                                            :form nil))   )
  )

(defmeth dataset-window-proto :DATASET ()
  (slot-value 'dataset))

(defmeth dataset-window-proto :CLOSE ()
  (let ((ch (choose-item-dialog "Closing window will..."
                                '("Hide view." "Delete view.")))  )
    (when ch
          (cond
            ((= ch 0)  (send self :hide-window))
            ((= ch 1)  (progn
                        (format t "Deleting dataset ~a.~%"
                                (send (slot-value 'dataset) :name))
                        (setf *datasets* 
                              (delete (slot-value 'dataset) *datasets*))
                        (send *dataset-menu* :delete-items
                              (send (slot-value 'dataset) :menu-item))
                        (call-next-method)
                        ))   )
          (unless *datasets*
                  (message-dialog "You have closed the last dataset."))
        )))
  
 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;      
;
;                DATASET OBJECT
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defproto DATASET-PROTO
  '(name      ; defaults to file name
    nObs      ; initial construction number of observations
    doc       ; documentation
    aList     ; var/data association list
    cases     ; case labels
    filter    ; boolean vector
    missing   ; code for missing data
    nextPos   ; position for next new added variable

    features  ; feature icons assoc list
    menuItem  ; appearing in data menu
    window    ; dataset window displaying contents
   ))


(defmeth dataset-proto :ISNEW (name)
  (setf (slot-value 'nextPos) 0)
  (setf (slot-value 'missing) 'na)
  (setf (slot-value 'name ) name)
  (setf (slot-value 'features)
        (list 
         (list "Filter" 
               #'(lambda (s) (send self :filter-is s))
               #'(lambda () (if (send self :filter) t nil))  )
         (list "Labels"  #'(lambda (s) (send self :case-labels-are s)))
         ))
  )


;;;;    ACCESSORS

(defmeth dataset-proto :NAME ()
  (slot-value 'name))

(defmeth dataset-proto :MENU-ITEM ()
  (slot-value 'menuItem))

(defmeth dataset-proto :FILTER ()
  (slot-value 'filter)
  )

(defmeth dataset-proto :N-VARS ()
  (length (slot-value 'aList)))

(defmeth dataset-proto :N-OBS ()    
  ;  Set when first variable is loaded by build-aList
  (slot-value 'nObs))

(defmeth dataset-proto :DATA ()
  ; List of values of all variables
  (mapcar #'cdr (slot-value 'aList)))

(defmeth dataset-proto :DATA-IS (new)
  ; Better have right number of non-symbolic columns!
  ; New by assumption only has the numeric data.
  (let* ((formPairs (send self :formula-pairs))
         (  vars    (send self :variables))    )
    (send self :build-aList
          vars
          (if formPairs  ; add formulas to new data
              (append (mapcar #'rest formPairs) new)
              new))))
          
  

(defmeth dataset-proto :VARIABLES ()
  (mapcar #'first (slot-value 'aList)))

(defmeth dataset-proto :FORMULA-PAIRS ()
  ; Returns the pairs associated with formulas.
  (select (slot-value 'aList)
          (which (mapcar #'(lambda (x)
                             (send self :is-formula? x))
                         (slot-value 'aList)))
          ))

(defmeth dataset-proto :IS-FORMULA? (pair)
  ; Assumes input is pair from internal aList
  (cond 
    ((symbolp (rest   pair))  t)
    ((numberp (rest   pair))  nil)
    ((numberp (second pair))  nil)
    ((fboundp (second pair))  t)
    (t (format t "*** Uncertain of formula status~%"))
    ))



(defmeth dataset-proto :WINDOW ()
  (slot-value 'window))

(defmeth dataset-proto :CASE-LABELS ()
  (send self :filter-data
        (if (slot-value 'cases)
            (slot-value 'cases)
            (mapcar #'(lambda (i) (format nil "~d" i))
                    (iseq (send self :n-obs)))
            )))
  

(defmeth dataset-proto :CASE-LABELS-ARE (form)
  (setf (slot-value 'cases)
        (mapcar #'(lambda (x) (format nil "~a" x))
                (send self
                      (if (stringp form)
                          :evaluate-string
                          :evaluate)
                      form :ignoreFilter? t))))


(defmeth dataset-proto :FILTER-IS (filterForm)
  (setf (slot-value 'filter) nil) ; so eval works in full domain
  (setf (slot-value 'filter)
        (send self 
              (if (stringp filterForm)
                  :evaluate-string
                  :evaluate)
              filterForm)))



;;;;     PRINTING

(defmeth dataset-proto :PRINT-SUMMARY ()
  (format t "-----  Data Set  -----~% ~a ~%" (slot-value 'doc))
  (format t "Variables defined (lists): ~a ~%"
          (send self :variables))
  (format t "Number of cases          : ~a ~%"
          (send self :n-obs))
  )


(defmeth dataset-proto :PRINT-CONTENTS (&key (stream t))
  (format t "Printing contents to file... ~a~%" stream)
  (format stream "~s~%" (slot-value 'doc))
  (mapcar #'(lambda (v) (format stream "~a " v))
          (mapcar #'first (slot-value 'aList))  )
  (format stream "~%")
  (mapcar #'(lambda (obs)
              (mapcar #'(lambda (x) (format stream "~a " x))
                      obs)
              (format stream "~%") )
          (transpose
           (mapcar #'rest (slot-value 'aList)))
          )
  nil)
                

;;;;     MENU

(defmeth dataset-proto :ADD-TO-MENU ()
  (push self *datasets*)
  (send *dataset-menu* :append-items 
        (setf (slot-value 'menuItem)
              (send menu-item-proto :new (send self :name)
                    :action #'(lambda ()
                                (send self :open-window))
                    ))))


;;;;     WINDOW INTERACTION

(defmeth dataset-proto :OPEN-WINDOW ()
  (if (slot-value 'window)
      (send (slot-value 'window) :show-window)
      (let ((w    (send dataset-window-proto :new self))
            (vars (send self :variables))
            )
        (format t "Opening window on ... ~a ~%" vars)
        (setf (slot-value 'window) w)  ; needs for icon build
        (send w :title (format nil "Dataset: ~a" (slot-value 'name)))
        (mapcar #'(lambda (v loc)
                    (make-variable-icon v self loc))
                vars
                (transpose (list 
                            (repeat 0 (length vars))
                            (* 30 (iseq (length vars))))  ))
        (send self :build-features)
        )))


(defmeth dataset-proto :CLOSE-WINDOW ()
  (format t "Close window using window button...~%"))



;;;;     FEATURES

(defmeth dataset-proto :BUILD-FEATURES ()
  (let ((features (slot-value 'features))   )
    (mapcar #'(lambda (name pos funcs)
              (apply #'make-feature-icon self name pos funcs))
            (mapcar #'first features)
            (* 50 (iseq 1 (length features)))
            (mapcar #'cdr   features)
            )))
          

(defmeth dataset-proto :ADD-FEATURES (aList)
  ;  Association list of features for this dataset.
  (setf (slot-value 'features)
        (append (slot-value 'features) aList))
  )

   

;;;;     VALUES OF VARIABLES

(defmeth dataset-proto :BUILD-ALIST (var data)
  (format t "Setting number of obs to ~d.~%" (length (first (last data))))
  (setf (slot-value 'nObs) (length (first (last data)))  )
  (if (= (length var) (length data))
      (setf (slot-value 'aList)
            (mapcar #'(lambda (v x) (cons v x))
                    var data))
      (message-dialog "Number of variables not equal to columns.")
      ))


(defmeth dataset-proto :ADD-VARIABLE (sym value &key (addIcon? t) (forceEval? nil))
  ; If value is symbolic, then makes special variable icon with sub-name.
  (when forceEval? 
        (setf value (send self :evaluate value)))
  (let ((pair (assoc sym (slot-value 'aList)))  )
    (if pair  ; identifier found --- changing values
        (if (eq sym value)
            (format t "~a = ~a is self-referential; no changes made.~%")
            (progn
             (format t "Variable ~a was in dataset; updating.~%" sym)
             (setf (cdr pair) value)    ))  ; in place substitution
        (if (member value (send self :variables)) ; rename or add new one
            (let ((p (assoc value (slot-value 'aList))))
              (setf (select p 0) sym))
            (progn
             (push (cons sym value) (slot-value 'aList) )
             (when addIcon?
                   (make-variable-icon sym self
                                       (list 50 (slot-value 'nextPos)))
                   (setf (slot-value 'nextPos)
                         (+ (slot-value 'nextPos) 30))    ))
            ))))

(defmeth dataset-proto :FILTER-DATA (data)
  (if (slot-value 'filter)
      (select data (which (slot-value 'filter)))
      data))


(defmeth dataset-proto :SET-POINT-STATE (plot)
  (if plot
      (if (slot-value 'filter)
          (send plot :point-state
                (iseq (send self :n-obs))
                (mapcar #'(lambda (b) (if b 'normal 'invisible))
                        (slot-value 'filter)))
          )
      (format t "*** Attempt to alter point state in nil plot.~%")
      ))
  

(defmeth dataset-proto :LABEL-POINTS (plot)
  (if plot
      (let ((n (send self :n-obs)))
        (send plot :point-label (iseq n) (send self :case-labels))
        (send plot :showing-labels t)  )
      (format t "*** Attempt to label points in nil plot.~%")
      ))
      

(defmeth dataset-proto :VARIABLE-VALUE (var &key ignoreFilter?)
  (let ((val  (cdr (assoc var (slot-value 'aList))))  )
    (if val
        (progn
         (setf val
               (cond
                 ((numberp val)          val)
                 ((symbolp val) (send self :evaluate val))
                 ((numberp (first val))  val)
                 ((fboundp (first val))  (send self :evaluate val))
                 (    t                  val) ))
         (if ignoreFilter?
             val
             (send self :filter-data val)   ))
        (message-dialog (format nil "Error: Variable ~a not found." var))
        )))

(defmeth dataset-proto :VARIABLE? (var)
  (if (assoc var (slot-value 'aList))
      t
      nil))


;;     EVALUATION
;  Special processing for quote, setf, define macro
;                         ^dataset -> self
;                         ^cases   -> (send self :case-labels)
;                         ^filter  -> (send self :filter)
;        
(defmeth dataset-proto :EVALUATE (form &key ignoreFilter?)
  ;  (format t "*** Evaluating form:  ~a~%" form)
  ;  (if (atom form) (format t "Form is an atom...~%"))
  (if (atom form)
      (cond
        ((numberp      form) form)
        ((eq '^cases   form) (send self :case-labels))
        ((eq '^dataset form) self)
        ((eq '^filter  form) (send self :filter))
        ((send self :variable? form) (send self :variable-value form
                                           :ignoreFilter? ignoreFilter?))
        (t  form))
      (cond
        ((eq '=  (first form)) (send self :evaluate
                                    (cons 'match (rest form))))
        ((eq 'quote (first form)) (second form))  ; ??? rest
        ((eq 'setf  (first form)) (eval
                                   `(setf 
                                     ,(second form)
                                     ,(send self :evaluate (caddr form)))))
        ((eq 'def   (first form)) (send self :add-variable
                                        (second form)
                                        (send self :evaluate (caddr form))))
        (t (apply
            (first form)
            (mapcar #'(lambda (x) (send self :evaluate x 
                                        :ignoreFilter? ignoreFilter?))               
                    (rest form))
            )))))
   
(defmeth dataset-proto :EVALUATE-STRING (str &key ignoreFilter?)
  (with-input-from-string (s str)
                          (send self :evaluate (read s) 
                                :ignoreFilter? ignoreFilter?)
                          ))



;;;;     FILES

(defmeth dataset-proto :READ-FROM-FILE (fileName)
  (with-open-file
   (file (concatenate 'string fileName))
   (let ((data  (read file))
         (vars  nil)
         (nCols 0)  )
     (setf (slot-value 'doc) data)
     (read-line file) (setf data (read-line file))
     (setf vars (with-input-from-string
                 (s (concatenate 'string "(" data ")")) 
                 (read s))) 
     (setf nCols (length vars))
     (setf data                                   ; read it all at once
           (nthcdr (1+ nCols)
                   (read-data-file
                    (concatenate 'string  fileName))))
     (if (integerp (/ (length data) nCols))   ; check labels and lengths
         (progn
          (send self :build-aList
                vars (transpose (split-list data nCols)))
          (send self :print-summary)
          self)
         ; else send a message
         (progn
          (message-dialog
           (format nil "Confused by file.~%Docum str: ~a~%Var names: ~a"
                  (slot-value 'doc) vars))
          nil)
     ))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;           LAUNCH
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(install-data-menu)
