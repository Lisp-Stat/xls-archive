;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Couple of macros to construct the :isnew method and all assessor
;; methods for the slots of a prototype. After defining the prototype
;; the slots are either argslots or keyslots. Argslots are mandatory
;; arguments for the :isnew method, keyslots are keyword arguments.
;; The macros create the :isnew method, and a :set-foo assessor
;; method for each slot (which returns the contents if called without
;; argement, and sets the content if called with an argument.
;;
;; Jan de Leeuw, 03-10-95
;;
;; modified to conform with make-proto.lsp 03-13-95
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro make-one-assessor (key slot prototype)
`(defmeth ,prototype ,key (&optional (content nil set))
   (if set (setf (slot-value ',slot) content))
   (slot-value ',slot)))

(defmacro make-proto (prototype slots parent)
`(defproto ,prototype ,slots () ,parent)
)

(defmacro make-is-new-method (prototype argslots keyslots)
`(defmeth ,prototype :isnew 
(,@(mapcar #'(lambda (x)
        (intern (concatenate 'string "THE-"
                                            (symbol-name x)))) argslots) 
&key 
,@(mapcar #'(lambda (x)
        (intern (concatenate 'string "THE-"
                                            (symbol-name x)))) keyslots))
,@(mapcar #'(lambda (x y) `(send self ,x ,y)) 
(mapcar #'(lambda (x)
        (intern (concatenate 'string "SET-"
                                            (symbol-name x)) 'keyword)) argslots)
(mapcar #'(lambda (y)
        (intern (concatenate 'string "THE-"
                                            (symbol-name y)))) argslots))
,@(mapcar #'(lambda (x y z) `(send self ,x (if ,y ,y (send self ,z)))) 
(mapcar #'(lambda (x)
        (intern (concatenate 'string "SET-"
                                            (symbol-name x)) 'keyword)) keyslots)
(mapcar #'(lambda (y)
        (intern (concatenate 'string "THE-"
                                            (symbol-name y)))) keyslots)
(mapcar #'(lambda (z)
        (intern (concatenate 'string "MAKE-"
                                            (symbol-name z)) 'keyword)) keyslots)))
)

(defmacro make-assessors (prototype argslots keyslots)
  `(progn 
    ,@(mapcar #'(lambda (x y) `(make-one-assessor ,x ,y ,prototype))
              (mapcar #'(lambda (x)
                          (intern (concatenate 'string "SET-"
                                                     (symbol-name x)) 'keyword)) (append argslots keyslots))
              (mapcar #'(lambda (x)
                          (intern (symbol-name x))) (append argslots keyslots))))
)

(provide "make-proto")