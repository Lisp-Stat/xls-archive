;;;;
;;;;     FUNCDICT.LSP  -- Function dictionary class.
;;;;
;;;;               26 May 92 ... Created as components of a simulator
;;;;               28 May 92 ... Build in labels.
;;;;               18 Sep 92 ... Built over dictionary and set collections.
;;;;               22 Sep 92 ... Stream additions.
;;;;

(provide "funcdict")

;;;;

(require "collect")
(require "streams")

;;;;


#||         Example

(setf estimator-list (list 
               (cons 'mean #'mean)
               (cons 'med  #'median)
               (cons 'sd   #'standard-deviation)))

(setf fd (make-function-dictionary
          estimator-list :name 'location-estimates 
          :domain? #'sequencep))

(send fd :is-in-domain? '(1 2 3 4 5) )

(send fd :apply '(1 2 3 4 5))
(send fd :apply-stream '(1 2 3 4 5))

||#


(defproto FUNCTION-DICTIONARY-PROTO
  '(
    haveRunGlobal? ; Boolean indicating if global has been run
    global         ; niladic function run prior to first application of factor  
    fList          ; list of the functions associated with dict
    prolog         ; function run before each application, passed argument
                   ;    and its output becomes the input to fList.
    isInDomain?    ; Boolean function that tests for domain of functions   
    rangeExample   ; canonical member of range of each function
    )
  ()
  dictionary-proto
  )

;;     _____  CREATOR  _____

(defun MAKE-FUNCTION-DICTIONARY (fList &key name global prolog domain? range)
  "The function list is a list of cons pairs of (label . func)."
  (send function-dictionary-proto :new name fList
        :global global :prolog prolog
        :domain? domain? :range range
        ))


(defmeth function-dictionary-proto :ISNEW (name fList
                                 &key global prolog epilog domain? range)
  (call-next-method name fList) ; set up the dictionary
  (setf (slot-value 'fList) (mapcar #'cdr fList))
  (if global  (setf (slot-value 'global ) global))
  (setf (slot-value 'haveRunGlobal?) (if global nil t))
  (if prolog  (setf (slot-value 'prolog ) prolog))
  (if domain? (setf (slot-value 'isInDomain?) domain?))
  (if range   (setf (slot-value 'rangeExample) range))
  )


;;     _____  ACCESSORS & DESCRIPTORS  _____

(defmeth function-dictionary-proto :FUNCTION-LIST ()
  (slot-value 'fList))

(defmeth function-dictionary-proto :LABEL-LIST ()
  (send self :key-list))

(defmeth function-dictionary-proto :N-FUNCTIONS ()
  (length (slot-value 'fList)))


;;     _____  METHODS: DOMAIN AND RANGE _____

(defmeth function-dictionary-proto :RANGE-EXAMPLE ()
  (slot-value 'rangeExample))


(defmeth function-dictionary-proto :IS-IN-DOMAIN? (arg)
  (if (slot-value 'isInDomain?)
      (funcall (slot-value 'isInDomain?) arg)
      (send self :print-message "Cannot check arg since lack range test.")
      ))

;;     _____  METHODS: EVALUATION _____

(defmeth function-dictionary-proto :APPLY (arg &key label)
  "Applies the function associated with label if given, otherwise all."
  (unless (slot-value 'haveRunGlobal?)
          (funcall (slot-value 'global))
          (setf (slot-value 'haveRunGlobal?) t))
  (if (slot-value 'prolog)
      (setf arg (funcall (slot-value 'prolog) arg)))
  (if label
      (funcall (cdr (assoc label (send self :list))) arg)
      (mapcar #'(lambda (f) (funcall f arg))
              (slot-value 'fList)))
  )

(defmeth function-dictionary-proto :APPLY-STREAM (arg)
  "Returns the stream formed by applying functions to the arg."
  (unless (slot-value 'haveRunGlobal?)
          (funcall (slot-value 'global))
          (setf (slot-value 'haveRunGlobal?) t))
  (if (slot-value 'prolog)
      (setf arg (funcall (slot-value 'prolog) arg)))
  (map-stream #'(lambda (f) (funcall f arg))
              (list-stream (slot-value 'fList)))
  )
     
 