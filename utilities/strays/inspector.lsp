;;;;
;;;; inspect.lsp   version 1.3      Nigel Gilbert        December 2, 1996
;;;;
;;;; XLISP-STAT 2.1 Copyright (c) 1990, by Luke Tierney
;;;; Additions to Xlisp 2.1, Copyright (c) 1989 by David Michael Betz
;;;; You may give out copies of this software; for conditions see the file
;;;; COPYING included with this distribution.
;;;;
;;;; Based on XLISP-STAT Examples "inspect.lsp" with additional
;;;; capablity to inspect objects, closures, structures and hash tables,
;;;; a menu interface, and some bugs removed.

;;;  Usage:
;;;  -----
;;;
;;;--> using the Menu
;;;   Loading the file will add a new menu to the menu-bar, "Inspector",
;;;   with items "Inspect", "Update" and "Close all".
;;;   "Inspect" inspects whatever is selected in the front window (e.g.
;;;       to inspect the value of the variable "foo", either find the
;;;       word "foo" somewhere or type the word "foo" in a window,
;;;       select it and then type command-I or choose Inspect from
;;;       the Inspector menu).
;;;   "Update" redisplays the front Inspector window, to reflect any changes
;;;       in the value of the item being inspected (e.g. by programmatic or
;;;       command line edits 'behind the scenes')
;;;   "Close all" closes all inspector windows that are open.
;;;   The titles of all inspect windows are appended to this menu as they
;;;       are opened.
;;;   Selecting an inspector window title from the menu brings that window
;;;       to the top.
;;;
;;;--> using an Inspector window
;;;   Double clicking on any item shown in an Inspector window will
;;;       open another Inspector window to inspect that item, until
;;;       there is nothing more to inspect  (you've got down to a string
;;;       or atom)
;;;   There will often be a button marked "Edit" at the bottom of an
;;;       inspector window.  If there is, selecting an item in the window
;;;       and then pressing the Edit button allows you to change that item.
;;;       A dialogue box is displayed and you can enter the new value (which
;;;       is evaluated).
;;;
;;;--> using the Command window
;;;
;;;  (inspect data &optional (edit nil))
;;;   opens an inspect window on the value of data.  If EDIT is T, allows
;;;   editing of values of selected component items of data.
;;;   Returns the inspector object.
;;;   Double-clicking an item opens an inspect window on that item.
;;;
;;;  (close-all-inspectors)
;;;   Closes all inspector windows
;;;
;;;
;;;  Global options:
;;;  --------------
;;;    *inspect-edit*  Default editing state, T (allow edits) or NIL
;;;
;;;  Installation
;;;  ------------
;;;    1. Load this file
;;;    2. Compile it with (compile-inspect)
;;;    3. Move the file "inspect.fsl" to the Autoload directory/folder
;;;    4. Edit the file "autoload.lsp" to add the line:
;;;              (autoload inspect "inspect")
;;;    5. Then, whenever you want use the inspector, just evaluate
;;;         either  (inspect)  to load the menu
;;;         or      (inspect {data}) to inspect {data}
;;;    6. To see it working, load the inspector (see step 5) and then
;;;       evaluate
;;;                 (setq test-data (test-inspect))
;;;       then select the word "test-data" in the XLISP-STAT window
;;;       with the mouse and choose the "Inspect" item from the
;;;       Inspector menu.  Double-click on an item in the Inspect window
;;;       see its value, and so on...
;;;
;;;   Bugs
;;;   ----
;;;   Won't display the values of structure slots which have accessor
;;;   functions defined using the :con-name option in defstruct
;;;   (Because XLSIP-STAT doesn't make the names of these functions
;;;   available to the Inspector).  Works with ordinary accessors,
;;;   though.
;;;
;;;   Developed on a Macintosh with XLISP-STAT 2.1 Release 3.44 (Beta).
;;;    Not tested on other other machines
;;;
;;;   Please report other bugs, wish-lists etc. to:
;;;
;;;       Nigel Gilbert (n.gilbert@soc.surrey.ac.uk)
;;;       University of Surrey, UK
;;;
;;;   Acknowledgements:  To the memory of Interlisp-D.  R.I.P.
;;;

(provide "inspect")


(defconstant *inspect-edit* t "Default for edit mode of inspect windows")

(defvar *inspector-objects* nil "List of all inspector objects on display")
(defvar *inspector-menu*  nil "The menu bar Inspector menu")

(defun compile-inspect ()
  (compile-file ":inspect.lsp" :output-file ":inspect.fsl" :load t :print t))

;;;
;;;
;;; Inspect Dialog Prototype
;;;
;;;

(defproto inspect-dialog-proto '(data ; the value of the item being inspected
                                 expr ;  the expression whose value is
being inspected
                                 editable ; T if user can edit this item
                                 list-item
                                 menu-item ; the menu item for this window
                                           ; on the Inspector menu

                                 ) () dialog-proto)

(defmeth inspect-dialog-proto :isnew (d &key expr (title "Inspect") edit)
  (setf (slot-value 'data) d)
  (setf (slot-value 'expr) expr)
  (setf (slot-value 'editable) edit)
  (push self *inspector-objects*)
  (when *inspector-menu*

;; if you send a string including characters such as () # as the title
;;  of a menu item, it screws up.  Hence the work around below.

        (let ((item (send menu-item-proto :new "XXX"
                          :action #'(lambda ()
                                      (send self :show-window)))))
          (setf (slot-value 'menu-item) item)
          (send *inspector-menu* :append-items item)
          (send item :title title)
          (send item :enabled t)
          ))
  (let ((items (send self :make-items)))
    (when items
          (when (slot-value 'editable)
                (setq items (append items
                                    (list (send button-item-proto :new "Edit"
                                                :action
                                                #'(lambda ()
                                                    (send self
:edit-selection)))))))
          (call-next-method items :title title :type 'modeless :go-away t))))

(defmeth inspect-dialog-proto :make-items ()
  (let ((data (slot-value 'data)))
    (setf (slot-value 'editable) nil)
    (list (send text-item-proto :new (format nil "type:  ~s" (type-of data)))
          (send text-item-proto :new (maxstring (format nil "value: ~s"
data) 60)))))

(defmeth inspect-dialog-proto :update ()
  (inspect1 (slot-value 'data)
            :expr (slot-value 'expr)
            :title (slot-value 'title)
            :editable (slot-value 'editable))
  (send self :close))

(defmeth inspect-dialog-proto :close ()
  (send *inspector-menu* :delete-items (slot-value 'menu-item))
  (setq *inspector-objects* (remove self *inspector-objects*))
  (call-next-method))


(defmeth inspect-dialog-proto :edit-selection () (sysbeep))

;;;
;;;
;;; Inspect Symbol Dialog Proto
;;;
;;;

(defproto inspect-symbol-dialog-proto '() () inspect-dialog-proto)

(defmeth inspect-symbol-dialog-proto :isnew (d &key expr title edit)
  (if (not (symbolp d)) (error "not a symbol"))
  (when (or (eq d nil) (eq d t)) (setq edit nil))
  (call-next-method d :expr expr :title title :edit edit))

(defmeth inspect-symbol-dialog-proto :make-items ()
  (let* ((data (slot-value 'data))
         (strings (list (format nil "name:      ~s" (symbol-name data))
                        (format nil "value:      ~s"
                               (if (boundp data)
                                   (symbol-value data) '*unbound*))
                        (format nil "function: ~s"
                                (if (fboundp data)
                                    (symbol-function data) '*unbound*))
                        (format nil "plist:        ~s" (symbol-plist data)))))
    (setf (slot-value 'list-item)
          (send list-item-proto :new strings
                :action	(let ((d self))
                          #'(lambda (double)
                               (if double (send d :inspect-selection))))))
    (list (send text-item-proto :new (format nil "type:  ~s" (type-of data)))
          (slot-value 'list-item))))

(defmeth inspect-symbol-dialog-proto :inspect-selection ()
  (let ((data (slot-value 'data))
        (editable (slot-value 'editable)))
    (case (send (slot-value 'list-item) :selection)
          (0 (inspect2 (symbol-name data)))
          (1 (if (boundp data)
                 (inspect2 (symbol-value data) :editable editable)))
          (2 (if (fboundp data)
                 (inspect2 (symbol-function data) :editable editable)))
          (3 (if (symbol-plist data)
                 (inspect2 (symbol-plist data) :editable editable))))))

(defmeth inspect-symbol-dialog-proto :edit-selection ()
  (let ((data (slot-value 'data))
        (list-item (slot-value 'list-item)))
    (case (send list-item :selection)
          (1 (let ((v (get-value-dialog "New symbol-value"
                                        :initial (and (fboundp data)
(symbol-value data)))))
               (when v
                     (setf (symbol-value data) (car v))
                     (send list-item :set-text 1
                           (format nil "value:      ~s"
                                   (symbol-value data))))))
          (2 (let ((v (get-value-dialog "New symbol-function"
                                        :initial (and (fboundp data)
(symbol-function data)))))
               (when v
                     (setf (symbol-function data) (car v))
                     (send list-item :set-text 2
                           (format nil "function: ~s"
                                   (symbol-function data))))))
          (3 (let ((v (get-value-dialog "New symbol-plist"
                                        :initial (and (fboundp data)
(symbol-plist data)))))
               (when v
                     (setf (symbol-plist data) (car v))
                     (send list-item :set-text 3
                           (format nil "plist:        ~s"
                                   (symbol-plist data)))))))))

;;;
;;;
;;; Inspect Sequence Dialog proto
;;;
;;;

(defproto inspect-sequence-dialog-proto '() () inspect-dialog-proto)

(defmeth inspect-sequence-dialog-proto :isnew
  (d &key expr title edit)
  (if (not (or (consp d) (vectorp d))) (error "not a sequence"))
  (call-next-method d :expr expr :title title :edit edit))

(defmeth inspect-sequence-dialog-proto :make-items ()
  (let* ((data (slot-value 'data))
         (strings (map-elements #'(lambda (x) (format nil "~s" x)) data)))
    (setf (slot-value 'list-item)
          (send list-item-proto :new strings
                :action	(let ((d self))
                          #'(lambda (double)
                                    (if double
                                        (send d :inspect-selection))))))
    (list (send text-item-proto :new
                (format nil "type:    ~s" (type-of data)))
          (send text-item-proto :new
                (format nil "length:  ~s" (length data)))
          (slot-value 'list-item))))

(defmeth inspect-sequence-dialog-proto :inspect-selection ()
  (let* ((data (slot-value 'data))
        (editable (slot-value 'editable))
        (list-item (slot-value 'list-item))
        (item (elt data (send list-item :selection))))
    (inspect2 item  :editable editable)))

(defmeth inspect-sequence-dialog-proto :edit-selection ()
  (let* ((data (slot-value 'data))
         (list-item (slot-value 'list-item))
         (i (send list-item :selection))
         (v (get-value-dialog "New value for element" :initial (elt data i))))
    (when v
          (setf (elt data i) (car v))
          (send list-item :set-text i (format nil "~s" (elt data i))))))
;;;
;;;
;;; Inspect Hash Table Dialog proto
;;;
;;;

(defproto inspect-hash-dialog-proto '(keys) () inspect-dialog-proto)

(defmeth inspect-hash-dialog-proto :isnew
  (d &key expr title edit)
  (unless (hash-table-p d) (error "not a hash-table"))
  (call-next-method d :expr expr :title title :edit edit))

(defmeth inspect-hash-dialog-proto :make-items ()
  (let ((data (slot-value 'data))
         (keys '()))
    (maphash #'(lambda (k v) (push  k keys)) data)
    (setf (slot-value 'keys) keys)
    (setf (slot-value 'list-item)
          (send list-item-proto :new (mapcar #'(lambda (x) (format nil "~s"
x)) keys)
                :action	(let ((d self))
                          #'(lambda (double)
                              (if double
                                  (send d :inspect-selection))))))
    (list (send text-item-proto :new
                (format nil "type: ~s" (type-of data)))
          (slot-value 'list-item))))

(defmeth inspect-hash-dialog-proto :inspect-selection ()
  (let* ((data (slot-value 'data))
        (editable (slot-value 'editable))
        (list-item (slot-value 'list-item))
        (key (nth (send list-item :selection) (slot-value 'keys))))
    (inspect2 (gethash key data) :editable editable)))

(defmeth inspect-hash-dialog-proto :edit-selection ()
  (let* ((data (slot-value 'data))
         (list-item (slot-value 'list-item))
         (key (nth (send list-item :selection) (slot-value 'keys)))
         (v (get-value-dialog "New value" :initial (gethash key data))))
    (when v
          (setf (gethash key data) (car v)))))
;;;
;;;
;;; Inspect Record Dialog proto
;;;
;;;

(defproto inspect-record-dialog-proto '(rec-slots) () inspect-dialog-proto)

(defmeth inspect-record-dialog-proto :isnew
  (d &key expr title edit)
  (unless (typep d 'structure) (error "not a record structure"))
  (call-next-method d :expr expr :title title :edit edit))

(defmeth inspect-record-dialog-proto :make-items ()
  (let* ((data (slot-value 'data))
         (slots (mapcar #'car (get (type-of data) '*STRUCT-SLOTS*))))
    (setf (slot-value 'rec-slots) slots)
    (setf (slot-value 'list-item)
          (send list-item-proto :new (mapcar #'(lambda (x) (format nil "~s"
x)) slots)
                :action	(let ((d self))
                          #'(lambda (double)
                              (if double
                                  (send d :inspect-selection))))))
    (list (send text-item-proto :new
                (format nil "type: ~s" (type-of data)))
          (slot-value 'list-item))))

(defmeth inspect-record-dialog-proto :inspect-selection ()
  (let* ((data (slot-value 'data))
        (editable (slot-value 'editable))
        (list-item (slot-value 'list-item))
        (slot (nth (send list-item :selection) (slot-value 'rec-slots))))
    (inspect2 (apply (field-accessor data slot) (list data)) :editable
editable)))

(defmeth inspect-record-dialog-proto :edit-selection ()
  (let* ((data (slot-value 'data))
         (list-item (slot-value 'list-item))
         (slot (nth (send list-item :selection) (slot-value 'rec-slots)))
         (v (get-value-dialog "New value" :initial (apply (field-accessor
data slot) (list data)))))
    (when v
          (eval (field-setter data slot (car v))))))

(defmacro field-accessor (record field)
"returns a structure accessor function, created dynamically"
  `(intern (concatenate 'string (string (type-of ,record)) "-" (string
,field))))

(defun field-setter (record field value)
"returns a setf expression to set a structure's field"
    (list 'setf (list (field-accessor record field) record) value))

;;;
;;;
;;; Inspect Closure Dialog proto
;;;
;;;

(defproto inspect-closure-dialog-proto '() () inspect-dialog-proto)

(defmeth inspect-closure-dialog-proto :isnew
  (d &key expr title edit)
  (unless (typep d 'closure) (error "not a closure"))
  (call-next-method d :expr expr :title title :edit edit))

(defmeth inspect-closure-dialog-proto :make-items ()
  (let* ((data (slot-value 'data))
         (lambda (multiple-value-list (function-lambda-expression data)))
         (l-exp (first lambda))
         (f-name (third lambda))
         (loc (list (/ (- (car (screen-size)) 400) 2) (/ (- (cadr
(screen-size)) 200) 2)))
         (window (send edit-window-proto :new :title (string f-name)
:location loc :size '(400 200)))
         (ppstream (make-string-output-stream)))
    (pprint l-exp ppstream)
    (send window :paste-string (get-output-stream-string ppstream))
    (push window *inspector-objects*)
    nil))

;;;
;;;
;;; Inspect Matrix Dialog Proto
;;;
;;;

(defproto inspect-matrix-dialog-proto
  '(list-item columns) () inspect-dialog-proto)

(defmeth inspect-matrix-dialog-proto :isnew (d &key expr title edit)
  (if (not (matrixp d)) (error "not a matrix"))
  (setf (slot-value 'columns) (min 3 (array-dimension d 1)))
  (call-next-method d :expr expr :title title :edit edit))

(defmeth inspect-matrix-dialog-proto :make-items ()
  (let* ((data (slot-value 'data))
         (columns (slot-value 'columns))
         (strings (map-elements #'(lambda (x) (format nil "~s" x)) data)))
    (setf (slot-value 'list-item)
          (send list-item-proto :new strings :columns columns
                :action	#'(lambda (double)
                            (if double (send self :inspect-selection)))))
    (list (send text-item-proto :new
                (format nil "type:    ~s" (type-of data)))
          (send text-item-proto :new
                (format nil "dimensions:  ~s" (array-dimensions data)))
          (slot-value 'list-item))))

(defmeth inspect-matrix-dialog-proto :inspect-selection ()
  (let ((data (slot-value 'data))
        (columns (slot-value 'columns)))
    (inspect2 (apply #'aref data (send (slot-value 'list-item) :selection))
             :editable (slot-value 'editable))))

(defmeth inspect-matrix-dialog-proto :edit-selection ()
  (let* ((data (slot-value 'data))
         (list-item (slot-value 'list-item))
         (i (send list-item :selection))
         (v (get-value-dialog "New value for element" :initial (aref data
(car i) (cadr i)))))
    (when v
          (setf (aref data (car i) (cadr i)) (car v))
          (send list-item :set-text i
                (format nil "~s" (aref data (car i) (cadr i)))))))

;;;
;;;
;;; Inspect Object Dialog proto
;;;
;;;

(defproto inspect-object-dialog-proto '(sorted-slots) () inspect-dialog-proto)

(defmeth inspect-object-dialog-proto :isnew
  (d &key expr title edit)
  (if (not (objectp d)) (error "not an object"))
  (call-next-method d :expr expr :title title :edit edit))

(defmeth inspect-object-dialog-proto :make-items ()
  (let* ((data (slot-value 'data))
         (own-slots (send data :own-slots))
         (sorted-slot-names (and own-slots
                                 (sort (map-elements #'(lambda (x)
                                                         (format nil "~s" x ))
                                                     own-slots)
                                       #'string-lessp)))
         (strings (cons "[parents]" (cons "[methods]" sorted-slot-names))))
    (setf (slot-value 'sorted-slots) sorted-slot-names)
    (setf (slot-value 'list-item)
          (send list-item-proto :new strings
                :action	#'(lambda (double)
                              (if double
                                  (send self :inspect-selection)))))
    (list (send text-item-proto :new
                (format nil "Prototype:    ~s" (send data :slot-value
'proto-name)))
          (slot-value 'list-item))))

(defmeth inspect-object-dialog-proto :inspect-selection ()
  (let* ((data (slot-value 'data))
         (editable (slot-value 'editable))
         (sel (send (slot-value 'list-item) :selection))
         (parents (send data :parents)))
    (case sel
      (0 (if (= (length parents) 1)
             (inspect2 (car parents))
             (inspect2 parents)))
      (1 (inspect2 (send data :own-methods)))
      (otherwise
       (inspect2 (send data :slot-value (intern (nth (- sel 2) (slot-value
'sorted-slots)))) :edit editable)))))

(defmeth inspect-object-dialog-proto :edit-selection ()
  (let* ((data (slot-value 'data))
         (sel (send (slot-value 'list-item) :selection))
         slot v)
    (when (> sel 1)
            (setq slot (intern (nth (- sel 2) (slot-value 'sorted-slots))))
            (setq v (get-value-dialog "New value for slot" :initial (send
data :slot-value slot)))
            (when v (send data :slot-value slot (car v))))))
;;;
;;;
;;; Inspect Function
;;;
;;;

(defun maxstring (str &optional (maxlen 15))
"Returns str unless its length is greater than maxlen, in which case it
is cut short and ... appended"

  (if (> (length str) maxlen)
      (concatenate 'string (subseq str 0 maxlen) "...")
      str))

(defun inspect2 (x &key title title)
  (inspect1 x :expr x :title title))

(defun inspect1 (x &key expr title (edit *inspect-edit*))
  (when (typep x 'number) (setq edit nil))
  (apply #'send
         (cond
           ((symbolp x) inspect-symbol-dialog-proto)
           ((or (consp x) (and (vectorp x) (not (stringp x))))
                        inspect-sequence-dialog-proto)
           ((matrixp x) inspect-matrix-dialog-proto)
           ((objectp x) inspect-object-dialog-proto)
           ((hash-table-p x) inspect-hash-dialog-proto)
           ((typep x 'structure) inspect-record-dialog-proto)
           ((typep x 'closure) inspect-closure-dialog-proto)
           (t           inspect-dialog-proto))
         :new x (list :expr expr
                      :title (maxstring (format nil "~s" (or expr title) 15))
                      :edit edit)))


(defmacro inspect (&optional (x nil x-present) &rest args)

"Args: (x (edit *inspect-edit*))
   opens an inspector window on the value of x.  If edit is T, allows
   editing of values of selected component items of data.  Double-clicking
   an item opens an inspect window on that item."

  `(when ,x-present (inspect1 ,x :expr ',x ,@args)))

(defun inspector-menu ()
"Add an Inspector menu to the menu bar (if there isn't one there already)"
  (let ((menu (find-menu "Inspector")))
    (when menu (send menu :remove))
    (setq *inspector-menu* (send menu-proto :new "Inspector"))
    (send *inspector-menu* :append-items
           (send menu-item-proto :new "Inspect" :key #\I
                 :action #'inspect-selection)
           (send menu-item-proto :new "Update" :key #\U
                 :action #'inspector-update)
           (send menu-item-proto :new "Close all" :key #\W
                 :action #'close-all-inspectors)
           (send dash-item-proto :new))
     (send *inspector-menu* :install)))

(defun inspect-selection ()
"Inspect whatever is selected in the front window"
  (let ((w (front-window)))
    (cond
      ((kind-of-p w edit-window-proto)
       (let ((s (send w :selection-stream)))
         (do ((expr (read s nil '*eof*) (read s nil '*eof*)))
             ((eq expr '*eof*))
             (inspect1 expr :expr expr))))
       ((kind-of-p w inspect-dialog-proto)
        (send w :inspect-selection))
       (t (sysbeep)))))

(defun inspector-update ()
"Send an update message to the front inspector window"
  (let ((w (front-window)))
    (if (kind-of-p w inspect-dialog-proto)
        (send w :update)
        (sysbeep))))

(defun close-all-inspectors ()
"Close all open inspector windows"
  (dolist (obj *inspector-objects*)
          (and obj (send obj :close)))
  (setq *inspector-objects* nil))


(inspector-menu)


;;;  for testing...

(defstruct point x y)

(defun test-inspect ()
"Return a complicated data structure to test inspector with"
  (let ((seq '(A B C (X Y Z)))
        (mat (matrix '(2 2) '(1 2 3 4)))
        (hash (make-hash-table))
        (rec (make-point))
        (fun #'(lambda (x) (if (and x (> (length x) 1)) (cadr x) x)))
        (obj (send *object* :new)))
    (setf (gethash 'list hash) seq)
    (setf (gethash 'record hash) rec)
    (setf (point-x rec) fun)
    (setf (point-y rec) obj)
    (list seq mat hash)))
