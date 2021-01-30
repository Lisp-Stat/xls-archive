;;;;
;;;;    SMALLTALK COLLECTION CLASSES: collection, bag, set, dictionary
;;;;
;;;;              All collection decendants MUST override
;;;;                       :ADD, :REMOVE, :DO, :CONVERT
;;;;
;;;;   19 Feb 91   Created with lists.
;;;;   25 Feb 91   First-round of implementation completed.
;;;;   27 Feb 91   Second-round adopts Smalltalk implementation; start dicts.
;;;;    1 Mar 91   Dictionary imp continues; use of dotted list for more eff.
;;;;   11 Mar 91   Separate read/write (eg name & nameIs) & parents (as in SELF).
;;;;   25 Aug 92   Modify for working with the Simulator objects.

(provide "collect")


;;;;       ____  NAMED, MESSAGE-HANDLING OBJ  _______

(defproto named-message-object-proto
  '(name msgStream)     ; name is a symbol identifier, not nec a string
  ()                    ; < class   slots>
  *object*              ; <    parent    >
  "A named, message-handling object.")

(defmeth named-message-object-proto :ISNEW (name &key (msg-stream t))
  (setf (slot-value 'msgStream) msg-stream)
  (setf (slot-value 'name) name))

(defmeth named-message-object-proto :NAME (&key as-string?)
  (if as-string?
      (format nil "~a" (slot-value 'name))
      (slot-value 'name)))

(defmeth named-message-object-proto :NAME-IS (name)
  (setf (slot-value 'name) name))

(defmeth named-message-object-proto :MESSAGE-STREAM ()
  "Answers the stream used to print messages from the receiver."
  (slot-value 'msgStream))

(defmeth named-message-object-proto :MESSAGE-STREAM-IS (msgStream)
  "Set the message stream used in printing."
  (setf (slot-value 'msgStream) msgStream))

(defmeth named-message-object-proto :PRINT-MESSAGE (message)
  (format (send self :message-stream) 
          (strcat "<#" (send self :name :as-string t) ". " message ".>")))


;;;;      ______  STATE  OBJECTS  _______

(defproto list-state-proto
  '(list)      ; <instance slots>
  ()           ; < class   slots>
  *object*     ; <    parent    >
  "A data/state object holding a list.")


;;     DATA ACCESS

(defmeth list-state-proto :LIST ()
  "Answers the list representation of the data."
   (slot-value 'list))

(defmeth list-state-proto :TEXT-LIST()
  "Answers a formatted version of the internal data."
  (mapcar #'(lambda (i) (format nil "~a" i))
          (slot-value 'list)))

(defmeth list-state-proto :LIST-IS (list)
  "Sets the state data from a list."
   (setf (slot-value 'list) list))

(defmeth list-state-proto :ADD-TO-LIST (item)
  "Add items to the internal list."
  (push item (slot-value 'list)))
  


;;;;                   _____  COLLECTION  _____


(defproto collection-proto
  ()                            ; <behavior has NO instance slots>
  ()                            ; < class   slots>
  named-message-object-proto    ; <    parent    >
  "A collection is a group of related members.")


;;     IDENTIFIER PREDICATE

(defun COLLECTION? (x)
  (and (objectp x)
       (kind-of-p x collection-proto)))


;;     PRINTING

(defmeth collection-proto :PRINT (&optional (stream t))
  (format stream "<#Collection: ~a>~%" (send self :name))
  )

;;     ACCESS

(defmeth collection-proto :AT (location &key ((put element) nil set))
  "Add argument to receiver collection at chosen site if :put. In either
    case, answer the item at the chosen index."
  (send self :print-message "Method :at not implemented in collection class"))


;;     ADD

(defmeth collection-proto :ADD (newObject)
  "Include argument in receiver collection. Answer new object."
  (send self :print-message "Method :add not implemented in collection class"))

(defmeth collection-proto :ADD-ALL (aCollection)
  "Include all elements of argument in receiver collection.
    Answer argument collection."
  (let ((aColl (send self :convert aCollection)))
    (send aColl :do #'(lambda (each) (send self :add each)))
    aColl)
  )


;;     REMOVE

(defmeth collection-proto :REMOVE (oldObject &key((ifAbsent aFunc) nil set))
  "Remove argument from receiver. Answer argument unless not found. 
    If not found, signal error and return nil."
  (send self :print-message "Method :remove not implemented in collection class"))

(defmeth collection-proto :REMOVE-ALL (aCollection)
  "Remove each element of arg from receiver. If done, answer argument collection.
    If some not found, signal error and return nil."
  (let ((aColl (send self  :convert aCollection)))
    (send aColl :do #'(lambda (each) (send self :remove each)))
    aColl)
  )


;;     TESTING MEMBERSHIP

(defmeth collection-proto :EMPTY? ()
  (= 0 (send self :size)))
  
(defmeth collection-proto :INCLUDES? (anObject)
  (let ((found nil))
    (send self :do #'(lambda (x) (if (eql x anObject) (setf found t))))
     found))

(defmeth collection-proto :SIZE ()
  (let ((count 0))
     (send self :do #'(lambda (x) (1+ count)))
     count))

(defmeth collection-proto :OCCURENCES-OF (anObject)
  "Answer how many times the argument is in the receiver collection."
  (let ((count 0))
     (send self :do #'(lambda (x) (if (eql anObject x) (1+ count))))
     count))


;;     ENUMERATION

(defmeth collection-proto :DO (aFunc)
  "Evaluate monadic function arg at each element of the collection.
    Answer is nil; only effect is via side-effects of evaluation."
  (send self :print-message "Method :do not implemented in collection class"))

(defmeth collection-proto :COLLECT (aFunc)
  "Answer collection obtained by applying arg to each member
    of the receiving collection."
  (let ((newCollection  (send self :convert ())))
    (send self :do #'(lambda (each)
                       (send newCollection :add (funcall aFunc each))))
    newCollection))

(defmeth collection-proto :DETECT (pred &key((ifNone aFunc) nil set))
  "Answer first element in receiver for which pred is true. If
    none, note error and answer nil."
  (let  ( (found nil)  (item ())  )
    (send self :do #'(lambda (each)
                       (if (funcall pred each)
                           (progn
                            (setf found t) (setf item each)))))
    (if found
        item
        (send self :print-message "Item not found."))))

(defmeth collection-proto :INJECT (initial &key :into)
  "Accumulate function calls beginning with initial arg.  Dyadic
    function called with first set to prior result, beginning
    from the initial value. Answer final function value."
  (send self :do #'(lambda (each)
                     (setf initial (funcall :into initial each))))
  initial)

(defmeth collection-proto :REJECT (pred)
  "Answer new collection composed of those elements in the 
    receiver for which the predicate arg evaluates to false."
  (let ((newCollection (send self :convert ())))
    (send self :do #'(lambda (each)
                       (unless (funcall pred each)
                               (send newCollection :add each))))
    newCollection))

(defmeth collection-proto :SELECT (pred)
  "Answer new collection composed of those elements in the 
    receiver for which the predicate arg evaluates to true."
  (let ((newCollection (send self :convert ())))
    (send self :do #'(lambda (each)
                       (when (funcall pred each)
                             (send newCollection :add each))))
    newCollection))


;;     CONVERSION

(defmeth collection-proto :CONVERT (arg)
  "Convert the argument into a collection like the receiver."
  (send self :print-message "Method :convert not implemented in collection class."))

(defmeth collection-proto :TAGGED-LIST (&key (asConsPair t))
  "Answers the list of (name . item) pairs."
  (let ((name (send self :name)))
    (mapcar (if asConsPair
                #'(lambda (i) (cons name i))
                #'(lambda (i) (list name i)))
            (send self :list))))
         

(defmeth collection-proto :AS-BAG ()
  (send self :print-message "Method :asBag not implemented in collection class"))

(defmeth collection-proto :AS-SET ()
  (send self :print-message "Method :asSet not implemented in collection class"))

(defmeth collection-proto :AS-SORTED-COLLECTION (&key (ordering '< set))
  (send self :print-message "Method :asSortedCollection not implemented in collection"))
                      


;;;;                ________ BAGS ________


;;     INITIALIZATION

(defproto bag-proto
  ()                    ; <instance slots>
  ()                    ; < class   slots>
  (list collection-proto list-state-proto) ; <    parents   >
  "Bags are unordered collections permitting multiple occurrences.")


;;     IDENTIFIER PREDICATE

(defun BAG? (x)
  (and (objectp x)
       (kind-of-p x bag-proto)))


;;     CONSTRUCTOR

(defun MAKE-BAG (aList &key (name "a bag"))
  (send bag-proto :new name aList))

(defmeth bag-proto :ISNEW (name aList)
  (send self :name-is name)
  (if (listp aList)
      (send self :list-is aList)
      (format t "Argument ~a is not a list.~%" aList)  ))


;;     CONVERSION

(defmeth bag-proto :CONVERT (arg)
  "Convert the argument into a bag."
  (cond 
    ((listp arg)  (make-bag arg))
    ((bagp arg)   arg  )
    (t            (send self :print-message "convert: arg invalid")
                  (make-bag ()))
    ))


;;     PRINTING

(defmeth bag-proto :PRINT (&optional (stream t))
  (format stream "<#Bag ~a: ~a>~%" (send self :name) (send self :list)))


;;     ACCESS

(defmeth bag-proto :AT (location &key ((put element) nil set))
  (send self :print-message "Bags do not support :at"))


;;     ADD

(defmeth bag-proto :ADD (item &key(withOccurences 1 set))
 (send self :add-to-list
       (if set
           (repeat-item item :withOccurences)
           item)))


;;     REMOVE

(defmeth bag-proto :REMOVE (oldObject &key((ifAbsent aFunc) nil))
  "Remove argument from receiver. Answer argument unless not found. 
    If not found, signal error and return nil."
  (if (send self :includes? oldObject)
      (let ((found nil))
        (send self :list-is (remove-if
                   #'(lambda(x) 
                       (if found nil
                           (setf found (eql oldObject x)))) (send self :list)))
        oldObject)
      (send self :print-message 
            (format nil "remove: arg ~a not found" oldObject))
      ))


;;     TESTING MEMBERSHIP

(defmeth bag-proto :INCLUDES? (anObject)
  (find anObject (send self :list)))

(defmeth bag-proto :SIZE ()
  (length (send self :list)))

(defmeth bag-proto :OCCURENCES-OF (anObject)
  "Answer how many times the argument is in the receiver collection."
  (count-if #'(lambda (x) (eql x anObject)) (send self :list)))


;;     ENUMERATION

(defmeth bag-proto :DO (aFunc)
  "Evaluate monadic function arg at each element of the collection.
    Answer is nil; only effect is via side-effects of evaluation."
  (dolist (element (send self :list) ())
          (funcall aFunc element)))


(defmeth bag-proto :COLLECT (aFunc)
  "Bag formed by applying monadic function to each item in the bag."
  (send bag-proto :new (mapcar aFunc (send self :list)))
  )



;;;             ________ SETS ________


;;     INITIALIZATION

(defproto SET-PROTO 
  ()                    ; <instance slots>
  ()                    ; < class   slots>
  (list collection-proto list-state-proto)  ; <   parents    >
  "Sets are unordered collections with single occurences.")


;;     TYPE PREDICATE

(defun SET? (x)
  (and (objectp x)
       (kind-of-p x set-proto)))


;;     CONSTRUCTOR

(defun MAKE-SET (aList &key(name "a set"))
  (send set-proto :new name aList))

(defmeth set-proto :ISNEW (name aList)
  (send self :name-is name)
  (if (listp aList)
      (send self :list-is (remove-duplicates aList))
      (send self :print-message (format nil "Argument ~a is not a list.~%" aList)))
  )


;;     CONVERSION

(defmeth set-proto :CONVERT (arg)
  "Convert the argument into a set."
  (cond 
    ((listp arg)       (make-set arg))
    ((setp  arg)          arg        )
    ((collectionp arg) (make-set (send arg :list)))
    (t                 (send self :print-message "convert: arg invalid")
                       (make-set () ))))


;;     PRINTING

(defmeth set-proto :PRINT (&optional (stream t))
  (format stream "<#Set ~a: ~a>~%" (send self :name) (send self :list))
  )


;;     ACCESS

(defmeth set-proto :AT (location &key ((put element) nil set))
  (send self :print-message "at: Sets do not support at."))


;;     ADD

(defmeth set-proto :ADD (newObject)
  "If arg is not in receiver set, add it. Answer arg."
  (send self :list-is (adjoin newObject (send self :list)))
  newObject)


;;     REMOVE

(defmeth set-proto :REMOVE (oldObject)
  (if (send self :includes (newObject))
      (progn
       (send self :list-is (remove oldObject (send self :list)))
       oldObject)
      (send self :print-message "remove: element not found")
      ))


;;     TESTING MEMBERSHIP

(defmeth set-proto :INCLUDES? (anObject)
  (find anObject (send self :list)))

(defmeth set-proto :SIZE ()
  (length (send self :list)))

(defmeth set-proto :OCCURENCES-OF (anObject)
  (if (send self :includes anObject)
      1
      0))


;;     ENUMERATION

(defmeth set-proto :DO (aFunc)
  "Evaluate monadic function arg at each element of the collection.
    Answer is nil; only effect is via side-effects of evaluation."
  (dolist (element (send self :list) ())
          (funcall aFunc element)))

(defmeth set-proto :COLLECT (aFunc)
  "Evaluate monadic function arg at each element of the set,
     collecting into a list."
  (send set-proto :new (mapcar aFunc (send self :list)))
  )


;;;           __________  DICTIONARIES  ___________


;;     INITIALIZATION

(defproto dictionary-proto
  ()                    ; <instance slots>
  ()                    ; < class   slots>
  set-proto             ; <    parent   >
  "Dictionaries are sets of (key,value) associations.
    Note that the associations are implemented as (k . v) pairs.")
                    ; hence the use of car/cdr access functions

;;     TYPE PREDICATE

(defun DICTIONARY? (x)
  (and (objectp x)
       (kind-of-p x dictionary-proto)))


;;     CONSTRUCTOR

(defun MAKE-DICTIONARY (aList &key (name "a dict"))
  (send dictionary-proto :new name aList))

(defmeth dictionary-proto :ISNEW (name aList)
  (send self :name-is name)
  (send self :message-stream-is t)
  (if (listp aList)
      (send self :list-is (remove-duplicates aList))
      (format t "Argument ~a is not a list.~%" aList))
  )

(defmeth dictionary-proto :COPY ()
  (make-dictionary (send self :list)))


;;     CONVERSION

(defmeth dictionary-proto :CONVERT (arg)
  "Convert the argument into a dictionary."
  (cond 
    ((listp arg)       (make-dictionary arg))
    ((dictionaryp arg)    arg        )
    ((setp arg)        (make-dictionary (send arg :list)))
    ((collectionp arg) (make-dictionary (send arg :list)))
    (t                 (send self :printMessage "convert: arg invalid")
                       (make-set () ))))


;;     PRINTING

(defmeth dictionary-proto :PRINT (&optional (stream t))
  (format stream "<#Dict ~a: ~a>~%" (send self :name) (send self :list))
  )


;;     ACCESS

;;    Note the following timing results (3 Apr 91, Äx) - alist faster
;
;     (def alist '((a . 1) (b . 2) (c . 3) (d . 4) (e . 5)))
;     (time (dotimes (i 1000) (assoc 'e alist)))       ;  .50 s
;     (time (dotimes (i 1000) (cdr (assoc 'e alist)))) ;  .57 s
;
;     (def plist '(a 1 b 2 c 3 d 4 e 5))
;     (time (dotimes (i 1000) (getf plist 'e)))        ; 1.4  s
;
;     (def vec (make-array 5))
;     (setf (select vec (iseq 5)) (iseq 5))
;     (time (dotimes (i 1000) (elt  vec 3)))           ;  .13  s
;     (time (dotimes (i 1000) (aref vec 3)))           ;  .15  s
;
;     (defproto test '(value1 value2 value3 value4 value5))
;     (defmeth test :value1 ()
;       (slot-value 'value1))
;
;     (defproto test '(value1 value2 value3 value4 value5))
;     (defmeth test :value1 ()  (slot-value 'value1))
;     (defmeth test :value2 ()  (slot-value 'value2))
;     (defmeth test :value3 ()  (slot-value 'value3))
;     (time (dotimes (i 1000) (send test :value3)))     ;  .43 s



(defmeth dictionary-proto :AT (key &key(put nil set))
  ; (format t "key ~a    put ~a ~%" key put)
  (if set
      (let ((pair (cons key put)))
        (if (send self :includes-key? key)
            ;already includes this key
            (replace-if #'(lambda (x) (eql (car x) key)) ; in-place!!!
                    (send self :list)
                    pair)
            ;otherwise add the new pair
            (send self :list-is (cons pair (send self :list)))
            )
        pair)
      (cdr (send self :association-at key))
      ))

(defmeth dictionary-proto :ASSOCIATION-AT (key)
  (let
    ((value (assoc key (send self :list))))
    (if value
        value
        (progn
         (send self :print-message "association-at: key not found")
         nil)
    )))

(defmeth dictionary-proto :KEY-AT-VALUE (value)
  (car (find-if #'(lambda (x) (eql value (cdr x)))
                     (send self :list))))
    
(defmeth dictionary-proto :KEY-LIST ()
  "Answer the list of dictionary keys."
  (mapcar #'car (send self :list)))

(defmeth dictionary-proto :KEYS ()
  "Answer the set of keys in the receiver dictionary."
  (let ( (aSet (make-set ())) )
    (dolist (element (send self :list) aSet)
            (send aSet :add (car element)))
    ))

(defmeth dictionary-proto :TAGGED-LIST ()
  "List of (name.key) pairs."
  (let ((name (send self :name)))
    (mapcar #'(lambda (k) (cons name k))
            (mapcar #'car (send self :list)))))


(defmeth dictionary-proto :KEYS-AS-TEXT (&key withName?)
  "Answer the list of dict keys formated as text strings."
  (if withName?
      (let ((name (send self :name))  )
        (mapcar #'(lambda (s) (format nil "~a ~a" name s))
                (mapcar #'car (send self :list))))
      (mapcar #'(lambda (s) (format nil "~a" s))
              (mapcar #'car (send self :list)))
      ))
 
(defmeth dictionary-proto :VALUES ()
  "Answer the bag of values in the receiver dictionary."
  (let ( (aBag (make-bag ())) )
    (dolist (element (send self :list) aBag)
            (send aBag :add (cdr element)))
    ))
         

;;     ADD

(defmeth dictionary-proto :ADD (pair)
  "Add (key . value) pair receiver dictionary if key is not
    already in the dictionary."
  (if (send self :includesKey (first pair))
      (send self :print-message "add: key already in dict")
      (progn        ; call to set would check value...so do it here
       (send self :list-is (cons pair (send self :list)))
       pair)
      ))


;;     REMOVE

(defmeth dictionary-proto :REMOVE (oldValue)
  "Cannot remove a value from a dictionary."
  (send self :print-message "remove: not implemented in dictionary"))

(defmeth dictionary-proto :REMOVE-ASSOCIATION (oldAssoc)
  "Remove association from receiver. If found, answer association."
  (if (send self :includesAssociation oldAssoc)
      (progn
       (send self :list-is (remove-if 
                         #'(lambda (x) (equal oldAssoc x))
                         (send self :list)))
       oldAssoc)
      (send self :print-message "remove: association not found")
      ))

(defmeth dictionary-proto :REMOVE-KEY (oldKey)
  "Remove key from dict. If found, answer the key."
  (if (send self :includesKey oldKey)
      (progn
       (send self :list-is (remove-if 
                         #'(lambda (x) (eql oldKey (car x)))
                         (send self :list)))
       oldKey)
      (send self :print-message "remove: key not found")
      ))


;;     TESTING MEMBERSHIP

(defmeth dictionary-proto :INCLUDES? (value)
  (send self :keyAtValue value))

(defmeth dictionary-proto :INCLUDES-ASSOCIATION? (dotPair)
  (find dotPair (send self :list) :test #'equal))

(defmeth dictionary-proto :INCLUDES-KEY? (key)
  (assoc key (send self :list)))

(defmeth dictionary-proto :OCCURENCES-OF (anObject)
  (count-if #'(lambda(pair) (eql anObject (cdr pair)))
            (send self :list)))


;;     ENUMERATION

(defmeth dictionary-proto :DO (aFunc)
  "Evaluate monadic function arg at each value in the dictionary.
    Answer is nil; only effect is via side-effects of evaluation."
  (dolist (element (send self :list) ())
          (funcall aFunc (cdr element) )))

(defmeth dictionary-proto :KEYS-DO (aFunc)
  "Evaluate monadic function arg at each key in the dictionary.
    Answer is nil; only effect is via side-effects of evaluation."
  (dolist (element (send self :list) ())
          (funcall aFunc (car element) )))

(defmeth dictionary-proto :ASSOCIATIONS-DO (aFunc)
  "Evaluate monadic function arg at each association pair in receiver.
    Answer is nil; only effect is via side-effects of evaluation."
  (dolist (element (send self :list) ())
          (funcall aFunc element)))

(defmeth dictionary-proto :COLLECT (aFunc)
  "Answer dictionary obtained by applying arg to each value of receiver."
  (let ((newDict  (send self :convert ())))
    (send self :associations-do 
          #'(lambda (each) (send newDict :add 
                                 (cons (car each) (funcall aFunc (cdr each))))))
    newDict))


