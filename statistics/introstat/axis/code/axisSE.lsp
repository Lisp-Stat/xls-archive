;;;;
;;;;     axisSE.lsp © Robert A. Stine, 1993
;;;;                 Correlation object.
;;;;
;;;;     20 Jul 93 ... Created to handle structural equation models.
;;;;
;;;; Additions to
;;;; XLISP-STAT 2.0 Copyright (c) 1988, by Luke Tierney
;;;; Xlisp 2.0 Copyright (c) 1985, 1987 by David Michael Betz
;;;;    All Rights Reserved
;;;;    Permission is granted for unrestricted non-commercial use

(provide "axisSE")

;;;;

(require "axisRegr")
(require "axisData")

;;;;

#||

(def ds  (make-dataset-from-values "se test data"
                                    '(y1 y2 x1 x2 x3)
                                    (list
                                     '(1 3 2 3 4 5)
                                     '(10 20 30 20 30 24)
                                     '(1 2 3 4 5 6)
                                     '(1 4 9 1 2 3)
                                     '(1 6 1 7 1 8) )))

(def sem (se-model '((y1 (x1  y2)) (y2 (x2 x3 y1)) )  ds))

(send sem :estimate)


||#

(defun SE-MODEL (eqns data)
  ; Eqns is a list of lists, each representing the symbols of an equation.
  ; Data is a list of same shape, with symbols evaluated to data.
  (if (and (listp (first eqns)) (< 1 (length eqns)) )
      (send se-model-proto :new eqns data)
      (format t "Argument error in SE-MODEL; eqns are: ~a~%" eqns)
      ))
      
  

;;;;;;;;;;;;;;;;


(defproto SE-MODEL-PROTO
  '(
    dataset         ; dataset for building equations
    endo            ; endogenous variable symbols
    exog            ; exogenous variable symbols
    equations       ; symbols that define equations
    recursive?      ; recursive model?
    iden            ; identification symbol for each eqn (over, not, exact)
    regressions     ; regression models for 2SLS on each equation
    )
  )


(defmeth se-model-proto :ISNEW (eqns dataset)
  (setf (slot-value 'dataset) dataset)
  (setf (slot-value 'endo) (mapcar #'first eqns))
  (setf (slot-value 'exog)
        (set-difference
         (remove-duplicates (combine (mapcar #'second eqns)))
         (slot-value 'endo))
        )
  (setf (slot-value 'equations) ; append list of included endogenous
        (mapcar #'(lambda (e)
                    (append e
                            (list (intersection 
                                   (second e) (slot-value 'endo))))  )
                eqns))
  (format t "Equations: ~a~%" (slot-value 'equations))
  (format t "SE-MODEL: Endogenous variables: ~a;~%          Exogenous  : ~a~%"
          (slot-value 'endo) (slot-value  'exog))
  (if (send self :recursive?)
      (format t "Recursive equations... OLS to be used.~%")
      (progn
       (format t "Non-recursive equations ... 2SLS to be used.~%")
       (send self :build-identification)
       (unless (send self :identified?)
               (format t "Equation(s) not identified: ~a~%"
                       (slot-value 'iden)))  ))
  (if (send self :identified?) (send self :estimate))
  )

;;;;;;;;;  IDENTIFICATION  ;;;;;;;;;;


(defmeth se-model-proto :RECURSIVE? ()
  ; Sets the recursive slot.
  (if (slot-value 'recursive?)
      (first (slot-value 'recursive?))
      (first 
       (setf (slot-value 'recursive?)
             (let* ((eqns  (sort ; sort on number included endogenous]
                                 (copy-list (slot-value 'equations))
                                 #'(lambda (a b)
                                     (< (length (select a 2))
                                        (length (select b 2))))  ))  )
               ;  (format t "Sorted equations ~a~%" eqns)
               (if (< 0 (length (select (first eqns) 2)))
                   (list nil)
                   (let ((endo ())
                         (rec  (list t))     )
                     (dolist (e eqns)
                             (if (= (length endo)
                                    (length (union endo (select e 2))))
                                 (push (first e) endo)
                                 (progn (setf rec (list nil)) (break))
                                 ))
                     rec)
                   ))))))
                    

(defmeth se-model-proto :IDENTIFIED? ()
  (if (send self :recursive?)
      t
      (if (member nil (slot-value 'iden))
          nil t)))

(defmeth se-model-proto :BUILD-IDENTIFICATION ()
  ; Order test - count number of included endogenous - excluded exogenous
  (setf (slot-value 'iden)
        (mapcar #'(lambda (p) 
                    ; (format t "pair is ~a~%" p)
                    (cond
                      ((apply #'< p) 'over)
                      ((apply #'= p) 'exact)
                      (    t         nil)))
                (mapcar #'(lambda (e) (send self :check-equation e))
                        (slot-value 'equations))
                )))


(defmeth se-model-proto :CHECK-EQUATION (eqn)
  ; returns a list with the #included endo, #excluded exog
  (let* ((iEndo (length (select eqn 2)))
         (eExog (- (length (slot-value 'exog))
                   (- (length (second eqn)) iEndo)))   )
    (list iEndo eExog)))


;;;;;;;;;;;;  ESTIMATION  ;;;;;;;;;;;;;;;;;;;;


; (def proj (send sem :build-projection))


(defmeth se-model-proto :BUILD-PROJECTION (x)
  ; Builds function that projects into space of variables in input matrix x
  ;  From Tierney, p172
  (let* ((svd (sv-decomp x))
         (u   (first svd))
         (s   (which (> (second svd) 0.0)))
         (b   (select (column-list u) s))
         (u1  (apply #'bind-columns b))  )
    (format t "Of ~d exogenous vars (with constant), using ~d in reduced.~%"
            (1+ (length (slot-value 'exog))) (length s))
    #'(lambda (y) (matmult u1 (matmult y u1)))
    ))


(defmeth se-model-proto :INCLUDED-ENDOGENOUS ()
  ; Returns list of the endogenous for which we need a reduced form.
  (remove-duplicates
   (combine
    (mapcar #'(lambda (e) (select e 2))
                    (slot-value 'equations)
            ))))


(defmeth se-model-proto :BUILD-REDUCED-FORM ()
  ; Constructs the reduced form variables and adds each to dataset.
  (let* ((ds (slot-value 'dataset))
         (x  (apply #'bind-columns
                    (repeat 1 (send ds :n-obs))
                    (send ds :evaluate (cons 'list (slot-value 'exog)))))
        (proj (send self :build-projection x))  )
     (mapcar #'(lambda (s) 
                (send ds :add-variable
                      (send self :append-caret s)
                      (funcall proj (send ds :evaluate s))   ))
            (send self :included-endogenous)
            )))
  
;      (send sem :estimate)


(defmeth se-model-proto :APPEND-CARET (name)
  ; Changes the input names to have a trailing caret.
  (if (listp name)
      (mapcar #'(lambda (n) (send self :append-caret n))
              names)
      (string-to-symbol (strcat (symbol-to-string name) "^"))
      ))            


(defmeth se-model-proto :MODIFY-EQUATION-NAMES ()
  ; Replaces the names of the endogenous on rhs with suffix of caret.
  (setf (slot-value 'equations)
        (mapcar #'(lambda (e)
                    (let ((endo (select e 2))   )
                      (list (first e)
                            (mapcar #'(lambda (s) 
                                        (if (member s endo)
                                            (send self :append-caret s)
                                            s))
                                    (second e))
                            endo)
                      ))
                (slot-value 'equations)
                )))


(defmeth se-model-proto :ESTIMATE ()
  ; If recursive, just estimate via ols the given equations.
  ; For nonrecursive, modify equations prior to feeding to regr.
  (unless (send self :recursive?)
          (format t "Building reduced form estimates~%")
          (send self :build-reduced-form)
          (send self :modify-equation-names)     )
  (setf (slot-value 'regressions)
        (mapcar #'(lambda (e)
                    (let ((xNames (symbol-to-string (second e)))
                          (yName  (symbol-to-string (first e)))   )
                      (send (slot-value 'dataset) :evaluate
                            `(regress (list ,@(second e)) ,(first e)
                                      :response-name  ,yName
                                      :predictor-names  ',xNames))
                      ))
                (slot-value 'equations)))
  )

;;;;
;;;;     BOOTSTRAP
;;;;

;  (def f (send *model* :bootstrap-function))
;  (funcall f (iseq 45))    ; reproduce original...

(defmeth se-model-proto :BOOTSTRAP-FUNCTION ()
  ; Return function that works given a list of input indices.
   (if (send self :recursive?) 
      #'(lambda (i)
          (combine
           (mapcar
            #'(lambda (r)
                (let* ((y   (select (send r :y) i))
                       (all (iseq (array-dimension (send r :x) 1)))
                       (x   (select (send r :x) i all))
                       (m (make-sweep-matrix x y))
                       (p (array-dimension x 1))  )
                  (coerce (compound-data-seq
                           (select (first (sweep-operator m (iseq 1 p)))
                                   (1+ p) (iseq 0 p))) 'list)
                  ))
            (slot-value 'regressions))))
       ; else handle non-recursive
       (format t "Does not do bootstrap for non-recursive (yet).~%")
       ))
;(let* ((exog (apply #'bind-columns
;                   (repeat 1 (send ds :n-obs))
;                   (send ds :evaluate (cons 'list (slot-value 'exog)))))
 ;       (cols (iseq (array-dimension x 1)))
       

(defmeth se-model-proto :BS-LABELS ()
  (mapcar #'string-to-symbol
          (combine (mapcar #'(lambda (e)
                               (let ((y  (strcat (symbol-to-string  (first e)) "-"))
                                     (xx (symbol-to-string  (cons 'Const (second e))))
                                     )
                                 (mapcar #'(lambda (x) (strcat y x))
                                         xx)
                                 ))
                           (slot-value 'equations)))
          ))
 

(defmeth se-model-proto :BOOTSTRAP (b)
  (let* ((n  (send (slot-value 'dataset) :n-obs))  )
    (bootstrap "Recursive SE"
               (list (send self :bootstrap-function))
               `(resample (iseq ,n))
               (slot-value 'dataset)
               b 
               :labels (send self :bs-labels)    )
    ))
  
