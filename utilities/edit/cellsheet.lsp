;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; The previous spreadsheet data-editor I made had some disadvantages.
;;
;; (a) It used scrollable lists. One for the row-labels (an n x 1 list)
;; one for the column-labels (an 1 x m list), and one for the data 
;; (an n x m list). The problem was how to synchronize the scrolling of 
;; these three lists, and I haven't found a decent solution to this.
;; I used the hack of displaying only one column at the time, and the 
;; horizontal scrollbar determines what column.
;;
;; (b) Cells of the list were edited by opening a dialog window when
;; they were double clicked. The user could then enter a new value.
;; This stinks, from the interface point of view.
;;
;; (c) Only one cell can be selected.
;;
;; This is an attempt to get around these difficulties. The spreadsheet
;; is not a scrollable list of three columns, which the last one
;; the "current" variable, but it is a partitioned array of text-items
;; and edit-text-items. These seems to get around the problems
;; mentioned above, at the cost of more computation at the Lisp level.
;;
;; The current version uses a fixed size sheet, and you cannot scroll
;; past the boundaries of the data set. The actual matrix of edit-text-items
;; is only the size of the sheet, not the size of the data.
;;
;; Comments are invited. I think editors such as these are the natural
;; foundation for a statistics system, expecially for introductory
;; statistics. 
;;
;; Version 0.5 -- Jan de Leeuw -- 07-23-95
;; Version 0.6 ------------------ added save and revert
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


#+unix (def ht 35)
#-unix (def ht 20)

#+unix (def wd 15)
#-unix (def wd 10)




(defproto spreadsheet-proto
  '(data row-labels column-labels dims length decimal mini maxi minj maxj
         backup title) 
  () dialog-proto
  "Prototype for a spreadsheet")

(defmeth spreadsheet-proto :isnew 
  (the-data &key 
            (the-row-labels 
             (to-string-list (iseq (array-dimension the-data 0))))
            (the-column-labels 
             (to-string-list (iseq (array-dimension the-data 1))))
            (the-dims '(10 5)) (the-length 6) (the-decimal 3)
            (the-title "MyMultivariable")
            (the-backup (list (copy-array the-data)
                              (copy-list the-row-labels)
                              (copy-list the-column-labels))))
  (send self :set-data the-data)
  (send self :set-dims the-dims)
  (send self :set-length the-length)
  (send self :set-decimal the-decimal)
  (send self :set-title the-title)
  (send self :set-row-labels the-row-labels)
  (send self :set-column-labels the-column-labels)
  (send self :set-backup the-backup)
  (send self :set-mini 0)
  (send self :set-maxi (min (first (send self :set-dims))
                            (array-dimension (send self :set-data) 0)))
  (send self :set-minj 0)
  (send self :set-maxj (min (second (send self :set-dims))
                            (array-dimension (send self :set-data) 1)))
  (send self :make-the-sheet
        (send self :set-mini)
        (send self :set-maxi)
        (send self :set-minj)
        (send self :set-maxj))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; Assessor Methods
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro normal-accessor (key slot prototype)
`(defmeth ,prototype ,key (&optional (content nil set))
   (when set (setf (slot-value ',slot) content))
   (slot-value ',slot)))


(normal-accessor :set-data data spreadsheet-proto)
(normal-accessor :set-row-labels row-labels spreadsheet-proto)
(normal-accessor :set-column-labels column-labels spreadsheet-proto)
(normal-accessor :set-dims dims spreadsheet-proto)
(normal-accessor :set-length length spreadsheet-proto)
(normal-accessor :set-decimal decimal spreadsheet-proto)
(normal-accessor :set-title title spreadsheet-proto)
(normal-accessor :set-backup backup spreadsheet-proto)
(normal-accessor :set-mini mini spreadsheet-proto)
(normal-accessor :set-maxi maxi spreadsheet-proto)
(normal-accessor :set-minj minj spreadsheet-proto)
(normal-accessor :set-maxj maxj spreadsheet-proto)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; Initialization Methods
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmeth spreadsheet-proto :make-cells (the-data mini maxi minj maxj)
  (let ((ll (send self :set-length))
        (lk (send self :set-decimal))
        (cc (make-array (list (- maxi mini) (- maxj minj)))))
    (dotimes (i (- maxi mini))
             (dotimes (j (- maxj minj))
                      (setf (aref cc i j)
                            (send edit-text-item-proto 
                                  :new (format nil "~v,vf" 
                                               ll lk (aref the-data i j))
                                  :action #'(lambda () 
                                              (send self :edit-cells))
                                  :size (list (* wd ll) ht)))
                  )
         )    
    (array-to-list cc)
    )
  )

(defmeth spreadsheet-proto :make-row-labels (the-row-labels mini maxi)
  (let ((cc (make-array (list (- maxi mini) 2)))
        (ll (send self :set-length)))
    (dotimes (i (- maxi mini))
             (setf (aref cc i 0)
                   (send edit-text-item-proto 
                         :new (elt the-row-labels i)
                         :action #'(lambda () 
                                     (send self :edit-row-labels))
                         :size (list (* wd ll) ht)))
             (setf (aref cc i 1)
                   (send text-item-proto
                         :new "" :size (list (/ (* wd ll) 2) ht)))
             )
    (array-to-list cc)
    )
  )
                       
(defmeth spreadsheet-proto :make-column-labels 
  (the-column-labels minj maxj)
  (let ((cc (make-array (list 2 (+ 2 (- maxj minj)))))
        (ll (send self :set-length)))
    (dotimes (j (- maxj minj))
             (setf (aref cc 0 (+ 2 j)) 
                   (send edit-text-item-proto
                         :new (elt the-column-labels j)
                         :action #'(lambda ()
                                     (send self :edit-column-labels))
                         :size (list (* wd ll) ht)))
             (setf (aref cc 1 (+ 2 j)) 
                   (send text-item-proto :new "" :size (list (* wd ll) ht)))
             )
    (setf (aref cc 0 0) (send text-item-proto :new "" :size 
          (list (* wd ll) ht)))
    (setf (aref cc 1 0) (send text-item-proto :new "" :size 
          (list (* wd ll) ht)))
    (setf (aref cc 0 1) (send text-item-proto :new "" :size
          (list (/ (* wd ll) 2) ht)))
    (setf (aref cc 1 1) (send text-item-proto :new "" :size 
          (list (/ (* wd ll) 2) ht)))
    (array-to-list cc) 
    )
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; Draw Method
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmeth spreadsheet-proto :make-the-sheet 
  (the-mini the-maxi the-minj the-maxj)
  (let* ((the-data (send self :set-data))
         (ll (send self :set-length))
         (n (array-dimension the-data 0))
         (m (array-dimension the-data 1))
         (di (1+ (- n (first (send self :set-dims)))))
         (dj (1+ (- m (second (send self :set-dims)))))
         (clpt (send self :make-column-labels 
                     (send self :set-column-labels)
                     the-minj the-maxj))
         (rlpt (send self :make-row-labels 
                     (send self :set-row-labels)
                     the-mini the-maxi))
         (cspt (send self :make-cells the-data
                     the-mini the-maxi the-minj the-maxj))
         (txsh (send text-item-proto :new "As First Column"))
         (txsv (send text-item-proto :new "As First Row"))
         (txgt (send text-item-proto :new "As Upper Left Cell"))
         (txy0 (send text-item-proto :new (format nil "~a" 0)
                     :size (list (/ (* wd ll) 2) ht)))         
         (txym (send text-item-proto :new (format nil "~a" (1- dj))
                     :size (list (/ (* wd ll) 2) ht)))
         (txx0 (send text-item-proto :new (format nil "~a" 0)
                     :size (list (/ (* wd ll) 2) ht)))
         (txxn (send text-item-proto :new (format nil "~a" (1- di))
                     :size (list (/ (* wd ll) 2) ht)))
         (hitm (send text-item-proto :new (format nil "~a" the-mini)
                     :size (list (/ (* wd ll) 2) ht)))
         (vitm (send text-item-proto :new (format nil "~a" the-minj)
                     :size (list (/ (* wd ll) 2) ht)))
         (schz (send sequence-scroll-item-proto :new (iseq dj)
                     :text-item hitm :value the-mini 
                     :action #'(lambda (x) (send (slot-value 'dialog)
                                                 :scroll-horizontal x))))
         (scvr (send sequence-scroll-item-proto :new (iseq di)
                     :text-item vitm :value the-minj
                     :action #'(lambda (x) (send (slot-value 'dialog)
                                                 :scroll-vertical x))))
         (ok (send button-item-proto :new "Enough!"
                   :action #'(lambda () (send self :close))))
         (sv (send button-item-proto :new "Save"
                   :action #'(lambda () (send self :save))))
         (rv (send button-item-proto :new "Revert"
                   :action #'(lambda () (send self :revert))))
         )
    (call-method dialog-proto :isnew
     (list
      (list clpt)
      (list rlpt cspt)
      txsh
      (list txy0 schz txym hitm)
      txsv
      (list txx0 scvr txxn vitm)
      (list ok sv rv))
      :title (send self :set-title))
    )
)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; Update Methods
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmeth spreadsheet-proto :update-the-sheet-horizontally 
  (the-mini the-maxi the-minj the-maxj)
  (let* ((items (send self :items))
         (ll (send self :set-length))
         (lk (send self :set-decimal))
         (data (send self :set-data))
         (mat (first (first (first items))))
         (cls (second (second items)))
         (lbs (send self :set-column-labels)))
    (dotimes (j (- the-maxj the-minj))
             (send (elt mat (+ j 2))
                   :text (elt lbs (+ the-minj j)))
             (dotimes (i (- the-maxi the-mini))
                      (send (elt (elt cls i) j)
                            :text (format nil "~v,vf" ll lk
                                          (aref data (+ the-mini i) 
                                                (+ the-minj j))))
                      )                      
             )
    )
  )

(defmeth spreadsheet-proto :update-the-sheet-vertically 
  (the-mini the-maxi the-minj the-maxj)
  (let* ((items (send self :items))
         (ll (send self :set-length))
         (lk (send self :set-decimal))
         (data (send self :set-data))
         (mat (mapcar #'first (first (second items))))
         (cls (second (second items)))
         (lbs (send self :set-row-labels)))
    (dotimes (i (- the-maxi the-mini))
             (send (elt mat i)
                   :text (elt lbs (+ the-mini i)))
             (dotimes (j (- the-maxj the-minj))
                      (send (elt (elt cls i) j)
                            :text (format nil "~v,vf" ll lk
                                          (aref data (+ the-mini i) 
                                                (+ the-minj j))))
                      )
             )
    )
)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; Scroll and Move methods
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmeth spreadsheet-proto :scroll-horizontal (x)
  (let* ((data (send self :set-data))
         (m (array-dimension data 1))
         (items (send self :items))
         (mat (first (first (first items))))
         (cls (second (second items)))
         (lbs (send self :set-column-labels))
         (mini (send self :set-mini))
         (maxi (send self :set-maxi))
         (orig-minj (send self :set-minj))
         (orig-maxj (send self :set-maxj))
         (minj (send self :set-minj x))
         (maxj (send self :set-maxj
                     (min m (+ x (second (send self :set-dims)))))))
    (dotimes (j (- orig-maxj orig-minj))
      (dotimes (i (- maxi mini))
        (setf (aref data (+ mini i) (+ orig-minj j))
              (read-from-string (send (elt (elt cls i) j) :text)))))
    (send self :update-the-sheet-horizontally mini maxi minj maxj)
    )
  )

(defmeth spreadsheet-proto :scroll-vertical (x)
  (let* ((data (send self :set-data))
         (n (array-dimension data 0))
         (items (send self :items))
         (mat (first (first (first items))))
         (cls (second (second items)))
         (orig-mini (send self :set-mini))
         (orig-maxi (send self :set-maxi))
         (mini (send self :set-mini x))
         (maxi (send self :set-maxi 
                     (min n (+ x (first (send self :set-dims))))))

         (minj (send self :set-minj))
         (maxj (send self :set-maxj)))
    (dotimes (j (- maxj minj))
      (dotimes (i (- orig-maxi orig-mini))
        (setf (aref data (+ orig-mini i) (+ minj j))
              (read-from-string (send (elt (elt cls i) j) :text)))))
    (send self :update-the-sheet-vertically mini maxi minj maxj)
    )
  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; Edit methods
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmeth spreadsheet-proto :edit-column-labels ()
  (let* ((the-minj (send self :set-minj))
         (the-maxj (send self :set-maxj))
         (items (send self :items))
         (mat (first (first (first items))))
         (lbs (send self :set-column-labels)))
    (dotimes (j (- the-maxj the-minj))
             (setf (elt lbs (+ the-minj j))
                   (send (elt mat (+ 2 j)) :text))
             )
    )
  )

(defmeth spreadsheet-proto :edit-row-labels ()
  (let* ((the-mini (send self :set-mini))
         (the-maxi (send self :set-maxi))
         (items (send self :items))
         (mat (mapcar #'first (first (second items))))
         (lbs (send self :set-row-labels)))
    (dotimes (i (- the-maxi the-mini))
             (setf (elt lbs (+ the-mini i))
                   (send (elt mat i) :text))
             )
    )
  )

(defmeth spreadsheet-proto :edit-cells ()
  (let* ((the-mini (send self :set-mini))
         (the-maxi (send self :set-maxi))
         (the-minj (send self :set-minj))
         (the-maxj (send self :set-maxj))
         (data (send self :set-data))
         (items (send self :items))
         (mat (second (second items))))
    (dotimes (i (- the-maxi the-mini))
             (dotimes (j (- the-maxj the-minj))
                      (setf (aref data (+ the-mini i) (+ the-minj j))
                            (with-input-from-string 
                             (s (send (elt (elt mat i) j) :text)) 
                             (read s)))
                      )
             )
    )
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; Save and Revert methods
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmeth spreadsheet-proto :save ()
  (let ((head (send self :title)))
    (with-open-file (f (concatenate 'string head ".dat")
                       :direction :output)
                    (format f "(defvar ~a" 
                            (concatenate 'string head "-data"))
                    (print (send self :set-data) f)
                    (format f ")~%")
                    (format f "(defvar ~a (quote" 
                            (concatenate 'string head "-row-labels"))
                    (print (list (send self :set-row-labels)) f)
                    (format f "))~%")
                    (format f "(defvar ~a (quote" 
                            (concatenate 'string head "-column-labels"))
                    (print (list (send self :set-column-labels)) f)
                    (format f "))~%")
                    )
    )
  )

(defmeth spreadsheet-proto :revert ()
  (let ((back (send self :set-backup))
        (mini (send self :set-mini))
        (maxi (send self :set-maxi))
        (minj (send self :set-minj))
        (maxj (send self :set-maxj)))
    (send self :set-data (copy-array (first back)))
    (send self :set-row-labels (copy-list (second back)))
    (send self :set-column-labels (copy-list (third back)))
    (send self :update-the-sheet-horizontally mini maxi minj maxj)
    (send self :update-the-sheet-vertically mini maxi minj maxj)
    )
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; Utilities
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun array-to-list (a)
  (split-list (coerce (element-seq a) 'list) (array-dimension a 1))
  )

(defun to-string-list (ls)
"Args: LIST
Converts LIST to list of strings."
  (mapcar #'(lambda (x) (format nil "~a" x)) ls)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; Example
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setf data
      (outer-product (iseq 20) (/ (iseq 5) 10) #'+))

(defvar column-labels 
'("melon" "apple" "grape" "plum" "pear"))

(setf sheet (send spreadsheet-proto :new data 
                  :the-column-labels column-labels :the-dims '(7 2)))


