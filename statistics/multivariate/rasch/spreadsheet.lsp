(require "make-proto")
(provide "spreadsheet")
(provide "multi-variable")

(defproto multi-variable-proto 
  '(data
    title 
    weights
    case-labels 
    variable-labels 
    backup)  () *object*
  "Prototype for a generic Multivariable") 

(is-new-method multi-variable-proto (data) 
               (title weights case-labels variable-labels backup))

(make-assessors multi-variable-proto (data) 
               (title weights case-labels variable-labels backup))                                                 
     
(defmeth multi-variable-proto :make-title ()
"Args: NONE
Sets the default title."
(send self :set-title "My Own Multivariable")
)

(defmeth multi-variable-proto :make-case-labels ()
  (mapcar #'(lambda (x) (format nil "~a" x))
          (iseq 0 (- (send self :nobs) 1)))
)

(defmeth multi-variable-proto :make-variable-labels ()
  (mapcar #'(lambda (x) (format nil "~a" x))
          (iseq 0 (- (send self :nvar) 1)))
)

(defmeth multi-variable-proto :make-weights ()
"Args: NONE
Sets the default weights (a vector of ones)."
  (repeat 1 (send self :nobs))
)

(defmeth multi-variable-proto :make-backup ()
"Args: NONE
Sets the default backup."
  (copy-tree (send self :set-data))
)

(defmeth multi-variable-proto :nobs ()
  (length (first (send self :set-data)))
)

(defmeth multi-variable-proto :nvar ()
   (length (send self :set-data))
)

(defmeth multi-variable-proto :edit 
  (&optional (ivar (iseq (send self :nvar))) (is-modal nil))
(let* (
       (n (send self :nobs))
       (m (length ivar))
       (kv 0)
       (dt (select (send self :set-data) ivar))
       (vt (select (send self :set-variable-labels) ivar))
       (cl (send self :set-case-labels))
       (wl (send self :set-weights))
       (pm (transpose (make-array (list 3 n) :initial-contents
                                  (list cl 
                                        (to-string-list wl)
                                        (to-string-list (first dt))))))
       (ll (send list-item-proto :new pm :columns 3))
       (dd (send ll :slot-value 'size))
       (l2 (make-array (list 1 3) :initial-contents
                       (list (list "Lables") (list "Weights") 
                             (list (first vt)))))
       (la (send list-item-proto :new l2 :columns 3))
       (ok (if is-modal
               (send modal-button-proto :new "Enough !" 
                   :location (list 10 (+ (second dd) 50)))
               (send button-item-proto :new "Enough !" 
                   :location (list 10 (+ (second dd) 50)))))
       (rv (send button-item-proto :new "Revert !" 
                   :location (list 150 (+ (second dd) 50))))
       (sb (send sequence-scroll-item-proto :new (iseq m) 
                 :size (list 150 16)
                 :location (list 310 (+ (second dd) 50))))
       (the-dialog (if is-modal
                       (send modal-dialog-proto :new 
                             (list
                              (list la)
                              (list ll) 
                              (list ok rv sb))
                             :default-button ok 
                             :size (list (+ (first dd) 20)
                                         (+ (second dd) 80)))
                       (send dialog-proto :new 
                             (list
                              (list la)
                              (list ll) 
                              (list ok rv sb))
                             :default-button ok 
                             :size (list (+ (first dd) 20)
                                         (+ (second dd) 80))
                             :title (send self :set-title))))
       )
  (send ll :slot-value 'action
        #'(lambda (x)
            (if x (send self :click-edit (elt ivar kv) 
                        (send ll :selection)
                        (send ll :replace-cell)))))
  (send la :slot-value 'action
        #'(lambda (x) (if x
                          (if (= (second (send la :selection)) 2)
                              (let (
                                    (lv (elt ivar kv))
                                    (fv (intern (string-upcase
                                                 (get-string-dialog 
                                                  "Enter a transformation:"))))
                                    )
                                (send self :transform fv lv)
                                (send ll :update (to-string-list
                                                  (elt (send self :set-data) lv)))
                                )))))
  (send  rv :slot-value 'action 
         #'(lambda ()
             (let (
                   (lv (elt ivar kv))
                   ) 
               (send self :revert lv)
               (send ll :update (to-string-list
                                 (elt (send self :set-data) lv)))
               )))
  (send sb :slot-value 'action
        #'(lambda (x)
            (setf kv x)
            (send ll :update (to-string-list (elt dt x)))
            (send la :set-text '(0 2) (elt vt x))))
  (if is-modal nil
      (send ok :slot-value 'action
            #'(lambda () (send the-dialog :close))))
  (if is-modal (send the-dialog :modal-dialog))
  )
)

(defmeth multi-variable-proto :click-edit (ivar ind new)
(let (
     (k1 (first ind))
     (k2 (second ind))
     (aa (elt (send self :set-data) ivar))
     )
(cond ((= k2 0) (setf (elt (send self :set-case-labels) k1) new))
      ((= k2 1) (setf (elt (send self :set-weights) k1) new))
      (t (setf (elt aa k1) new) 
))))

(defmeth list-item-proto :update (var)
(dotimes (i (length var))
         (send self :set-text (list i 2) (elt var i)))
)

(defmeth list-item-proto :replace-cell ()
(let* (
       (kk (send self :selection))
       (k2 (second kk))
       (aa  (if (= k2 0) 
                (get-string-dialog "Enter a label:")
                (first (get-value-dialog "Enter an expression:"))))
       )
(send self :set-text kk 
      (if (stringp aa) aa (num-to-string aa)))
aa))

(defmeth multi-variable-proto :revert (var)
(let (
     (ls (copy-list (elt (send self :set-backup) var)))
     )
(setf (elt (send self :set-data) var) ls)
))

(defmeth multi-variable-proto :transform (func var)
(let (
     (ls (funcall func (elt (send self :set-data) var)))
     )
(setf (elt (send self :set-data) var) ls)
))

(defun to-string-list (ls)
"Args: LIST
Converts LIST to list of strings."
(mapcar #'(lambda (x) (format nil "~a" x)) ls)
)


ÿ