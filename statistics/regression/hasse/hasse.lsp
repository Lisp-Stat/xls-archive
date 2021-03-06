;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                           hasse.lsp
;;;
;;; This program determines F-test and expected mean squares for
;;; balanced complete experimental designs.  It is based on rules
;;; presented in Taylor & Hilton (1981) American Statistician, 85-93.
;;; The program accepts as input a plain text file containing
;;; information about the factors in the experiment, for example:
;;;
;;; (A 5 fixed nil)
;;; (C 3 random (A))
;;; (B 4 fixed nil)
;;; (E 1 random (A C B))
;;;
;;; Each line represents one factor in the design and contains a 
;;; list of four elements: the factor name (or letter designation), number
;;; of levels, type (either fixed or random) and a list of all factors in
;;; which the given factor is nested.  An empty list, nil, in the fourth
;;; position indicates that the given factor is not nested in any 
;;; other factors.  There should always be one factor, E, which is
;;; nested in all the other factors.  It should be random and usually
;;; has one level.
;;;    On UNIX systems, the input file filenames MUST be in upper case,
;;; or the get-filename function will not find them.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; MAKE-ACCESS defines accessor/settor methods for the
;;             given object name and slot name.

(defun make-access (object method slot)
  "Args: (object method slot)
Defines an accessor method for slot in object."

  (cond ((not (objectp (eval object)))
         (error "in MAKE-ACCESS: not an object"))

        ((not (send (eval object) :has-slot slot :own t))
         (error "in MAKE-ACCESS: object does not have the given slot"))

        (t
         (eval `(defmeth ,object ,method (&optional (,slot nil set))
                  (if set (setf (slot-value ',slot) ,slot))
                  (slot-value ',slot))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; FACTOR-PROTO

(defproto factor-proto
  '(name range type nested-in parents x y depth))

(make-access 'factor-proto ':name 'name)
(make-access 'factor-proto ':range 'range)
(make-access 'factor-proto ':type 'type)
(make-access 'factor-proto ':nested-in 'nested-in)
(make-access 'factor-proto ':parents 'parents)
(make-access 'factor-proto ':x 'x)
(make-access 'factor-proto ':y 'y)
(make-access 'factor-proto ':depth 'depth)

(defmeth factor-proto :print (&optional (stream t))
  (format stream "#<factor ~a>" (send self :name)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; BALANCED-ANOVA-PROTO

(defproto baproto
  '(factors effects menu factor-names structure-diagram))

(make-access 'baproto ':factors 'factors)
(make-access 'baproto ':effects 'effects)
(make-access 'baproto ':menu 'menu)
(make-access 'baproto ':factor-names 'factor-names)
(make-access 'baproto ':structure-diagram 'structure-diagram)


              ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
              ;;; Methods associated with the menu ;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; LOAD-DESIGN
;;; 1. Prompt user to select a design file.
;;; 2. Read that file into FACTOR-LIST
;;; 3. For each nested-in list, replace factor names with factor objects.
;;; 4. Compute all allowable effects.
;;; 5. Remove any previous structure diagram.

(defmeth baproto :load-design ()
  "Args: ().  Reads in GLOBAL list of factor information."

  (let ((factor-list (load-design-file nil)) )
    (send self :factors factor-list)
    (send self :factor-names (mapcar #'(lambda (x) (send x :name))
                           factor-list))
    (dolist (factor factor-list)
            (send factor :nested-in
                  (send self :replace-name-with-object
                        (send factor :nested-in))) )
    (send self :effects (send self :allowable-effects factor-list))
    (if (send self :structure-diagram)
        (send (send self :structure-diagram) :close) )
    (send self :structure-diagram nil)
    (format t "Design file has been loaded.~%") ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; DO-LINEAR-MODEL

(defmeth baproto :do-linear-model ()
  "Args: ().  Calls linear-model."

  (cond
    ((send self :factors)
         (format t "~%Model: Y = ")
         (send self :linear-model) )
    (t (format t "NO FACTORS ARE LOADED.~%")) ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; DO-ANOVA-TABLE

(defmeth baproto :do-anova-table ()
  "Args: ().  Shows the ANOVA table if *factor-list* is non-NIL."

  (if (send self :factors)
      (send self :anova-table)
      (format t "NO FACTORS ARE LOADED.~%")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; DO-EMS-LIST

(defmeth baproto :do-ems-list ()
  "Args: ().
Shows the list of Expected Mean Squares if *factor-list* is non-NIL."

  (if (send self :factors)
      (send self :ems-list (send self :effects))
      (format t "NO FACTORS ARE LOADED.~%") ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; DO-STRUCTURE-DIAGRAM

(defmeth baproto :do-structure-diagram ()
  "Args: ().  Draw the structure (Hasse) diagram."

  (if (send self :factors) (send self :draw-structure)
    (format t "NO FACTORS ARE LOADED.~%") ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; DO-FACTOR-SETS

(defmeth baproto :do-factor-sets ()
  "Args: ().  Show the important factor sets for a selected effect."

  (if (send self :factors) (send self :factor-sets)
    (format t "NO FACTORS ARE LOADED.~%") ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; FACTOR-SETS
;;; Applies only to non-NIL effects.

(defmeth baproto :factor-sets ()
  "Args: ().  Show the important factor sets for a selected effect."

  (labels
    ((eff-strings (eff-list)
                  (mapcar #'(lambda (x) (send self :effect-string x))
                          eff-list) )
     (factor-list (factors) (send self :factor-list factors ", "))
     (show-sets (effect)
         (format t "~%Factor sets for effect ~a:~%"
		 (send self :effect-string effect))
         ;;(format t "selected effect: ~s~%" effect)
	 (format t "----------------------~%")
         (format t "     Symbolic Factors: ~a~%"
                 (factor-list (send self :symbolic-factors effect)) )
         (format t "         Live Factors: ~a~%" (factor-list effect))
         (format t "         Dead Factors: ~a~%"
                 (factor-list (send self :dead-factors effect)) )
         (format t "   Complement Factors: ~a~%"
                 (factor-list (send self :complement-factors effect)) )
         (format t "    Random Complement: ~a~%"
                 (factor-list (send self :random-CF effect)) )
         (format t "    Simple Rand. Comp: ~a~%"
                 (factor-list (send self :simple-RCF effect)) )) )

    (let* ((effects (subset (send self :effects) #'(lambda (x) x)))
           (effect-names (eff-strings effects))
           (subset
	    (first (choose-subset-dialog "Select an effect:" effect-names)) )
           (efflist (select effects subset)) )
      (mapcar #'show-sets efflist) )))

                 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                 ;;; Other Methods (Alphabetically) ;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ALLOWABLE-EFFECTS

(defmeth baproto :allowable-effects (factor-list)
  "Args: (factor-list)
Computes the allowable effects of factor-list."

  (labels
        ((link-p (X1 X2)
                 (or (member X2 (nested-in X1))
                     (member X1 (nested-in X2)) ))
         (extended-link (first rest)
                        (eval `(or ,@(mapcar #'(lambda (x)
                                                 (if (link-p first x) T nil))
                                             rest ))))
         (effect-allowed (X)
                         (cond
                          ((null X) nil)
                          (t (not (extended-link (first X) (rest X)))) ))
         (check (X)
                (subset X #'effect-allowed))
         (allow2 (LST)
                   (cond ((null (rest LST))          ; no CDR.
                          (cons nil (list LST)) )
                         (t                                      ; otherwise.
                          (let ((result (allow2 (rest LST))) )
                            (append result
                                    (check
                                     (addtoeach (first LST) result) ))))))
         ) ; end of local function definitions.

        (if (null factor-list)
            nil                    ; if-part.
          (allow2 factor-list)) )) ; else-part.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ANOVA-TABLE
;;; Assumes that the first item in the effects slot is NIL
;;;    (the grand mean) and should not be processed.
;;; Also, skip any effects that have zero DF.

(defmeth baproto :anova-table ()
  "Args: ().  Displays the ANOVA table (recursive)."

  (format t "~%    ANOVA TABLE:~%~%")
  (format t "SOURCE            d.f.  Mean Sqr  F-test~%")
  (format t "-------------     ----  --------  -----------~%")

  (let ((total-df 0)
        (DF 0)
        (effects (rest (send self :effects))) )
    (dolist (this-effect effects)
            (setf DF (send self :symbolic-product this-effect))
	    ;;(cond
	     ;;((> DF 0)
	      (format t "~18a"
		      (send self :effect-string this-effect :extended t) )
	      (format t "~4d  " DF)                   ; degrees of freedom.
	      (format t "~11a"
		      (send self :MS-string this-effect 11)) ; mean square.
	      (send self :F-test this-effect)                ; F-test.
	      (format t "~%")                                ; new line.
	      (setf total-df (+ total-df DF))
	      ;;))
	    )

    (format t "-------------     ----~%")
    (format t "Total (corr.)     ~4d~%" total-df) ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; COMPLEMENT-PROD

(defmeth baproto :complement-prod (effect)
  "Args: (effect).  Computes the complement product of effect."

  (let ((complement-F (send self :complement-factors effect)))
    (if (null complement-F)
        1
      (reduce #'* (mapcar #'num-levels complement-F))) ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; COMPLEMENT-FACTORS

(defmeth baproto :complement-factors (effect)
  "Args: (effect).  Returns a list of the complement factors of effect."

  (complement (send self :symbolic-factors effect)
              (send self :factors) ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; DEAD-FACTORS

(defmeth baproto :dead-factors (effect)
  "Args: (effect).  Returns the list of dead factors for effect."

  (flet ((dead-list-dup (factors)
                       (if (null factors)
                           (error "in dead-list-dup: null argument.")
                         (do*
                          ((list factors (rest list))
                           (head (first list) (first list))
                           (result (nested-in head)
                                   (append (nested-in head) result)))
                          ((null (rest list)) result) ))))

        (remdups (dead-list-dup effect)) ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; EFFECT-STRING

(defmeth baproto :effect-string (effect &key (extended nil))
  "Args: (effect &key (extended nil))
Takes an effect, as a list of factors, and returns a string of characters
representing the usual way of writing an effect.  Normal mode leaves off
the nesting factors; extended mode puts them in parentheses."

  (flet
    ((nesting-factors (effect)
                      (if (null effect) nil
                        (reduce #'union (mapcar #'nested-in effect)) )))
    (cond
      ((null effect) "u")                     ; overall mean.
      ((not extended)                         ; normal mode.
       (send self :factor-list effect))
      (t                                      ; extended mode.
       (let ((Result1 (send self :factor-list effect))
             (Nesting (send self :factor-list
                            (nesting-factors effect) ",")))
         (if (= 0 (length Nesting))
             Result1                                         ; if-part.
             (concatenate 'string Result1 "(" Nesting ")" )) ; else-part.
         )))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; EMS-LIST

(defmeth baproto :ems-list (effects)
  "Args: (effects)
Displays the Expected Mean Square for each effect in EFFECTS."

  (format t "~%Expected Mean Squares:~%~%")
  (let*
    ((effects (send self :effects))
     (effects (if (null (first effects)) (rest effects) effects)) )

    (dolist (this-effect effects)
            (format t "~a = " (send self :MS-string this-effect :expected t))
            (send self :write-ems
                  (send self :gfi
                        (send self :allowable-effects
                              (send self :random-CF this-effect) )
                        this-effect)
                  this-effect)
            (format t "~%") ))

  (format t "~%Note: (.) is a variance component.~%")
  (format t   "      [.] is a mean squared deviation from a ~
                     treatment mean.~%") )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; F-TEST
;;;
;;; Called functions:
;;;    allowable-effects, simple-RCF, random-CF, complement,
;;;    even-NSRCF, odd-NSRCF, gfi, write-ftest.

(defmeth baproto :F-test (this-effect)
  "Args: (this-effect)
Displays the formula for the F-test for this-effect."

  (labels
    ((even-NSRCF (Nway-SRCF)
                 (subset Nway-SRCF #'(lambda (X) (evenp (length X)))) )
     (odd-NSRCF  (Nway-SRCF)
                 (subset Nway-SRCF #'(lambda (X) (oddp (length X)))) )
     (sym-prod (x) (send self :symbolic-product x))
     (zero-df (F-list)
	      (equal 0 (reduce #'+ (mapcar #'sym-prod F-list))) )
     (write-ftest (F-list)
		  ;;(format t "~%F-list: ~s~%" F-list)
                  (cond
                    ((or (null F-list) (zero-df F-list))
                     "**" )
		    (t
		     (let ((result (send self :MS-string (first F-list))))
		       (dolist (F-item (rest F-list) result)
			       (setf result
				     (format nil "~a + ~a" result
					     (send self :MS-string F-item) )))
				     ))))
     (char-repeat (char times)
		  (apply #'concatenate 'string (repeat char times)) ))

    (let*
      ((all-factors (send self :factors))
       (Nway-SRCF (send self :allowable-effects
                        (send self :simple-RCF this-effect) ))
       (numerator   (send self :gfi (even-NSRCF Nway-SRCF) this-effect))
       (denominator (send self :gfi (odd-NSRCF  Nway-SRCF) this-effect))
       (numer (write-ftest numerator))
       (denom (write-ftest denominator)) )
      (format t "~a" numer)
      (if (null (rest numerator))
	  (format t " / ")
	(format t "~%~35a~a~%~35a"
		" " (char-repeat "-" (max (length numer) (length denom))) " "))
      (format t "~a." denom) )))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; FACTOR-LIST

(defmeth baproto :factor-list (factors &optional (separator "*"))
  "Args: (factors &optional (separator "*")).
Returns a string containing a list of factors separated by separator."

  (setf factors (send self :sort-factors factors))
  (if (null factors) ""
      (do*
       ((dolist factors (rest dolist))
        (name (factor-name (first dolist))
              (factor-name (first dolist)))
        (result name (concatenate 'string result separator name)) )
       ((null (rest dolist)) result) )))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; GFI - Generalized Formal Interaction
;;; Note: g-remove is a global function.

(defmeth baproto :gfi (effect-list effect)
  "Args: (effect-list effect)
Computes the formal interaction of effect with each item in effect-list
and returns a list of the results."

  (labels
    ((g-nested-in (factor-list)
                  (if (null factor-list)
                      nil
                      (reduce #'append
                              (mapcar #'nested-in factor-list) )))
     (fi (effect factor-list)
         (let ((total (remdups (append effect factor-list))) )
           (g-remove (g-nested-in total) total) )) )

    (if (null effect-list)
        nil
        (mapcar #'(lambda (X) (fi X effect)) effect-list) )))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; LINEAR-MODEL

(defmeth baproto :linear-model ()
  "Args ()
Writes the linear model using the slot, effects."

  (let ((effects (send self :effects)))
    (format t "~a" (send self :effect-string (first effects) :extended t))
    (dolist (effect (rest effects))
            (format t " + ~a" (send self :effect-string effect :extended t)))
    (format t "~%") ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; MS-STRING

(defmeth baproto :MS-string (effect &key (expected nil) (maxlen nil))
  "Args: (effect &key (expected nil) (maxlen nil))
Print effect as a mean square or an expected mean square.  Maxlen controls
the field width if supplied."

  (let* ((prefix (if expected "E" ""))
         (result (concatenate 'string prefix "MS-"
                              (send self :effect-string effect))))
    (if maxlen
        (format nil "~va" maxlen
                (subseq result 0 (min (length result) maxlen)))
        result)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; RANDOM-CF

(defmeth baproto :random-CF (effect)
  "Args: (effect)
Returns a list of the random complement factors of effect."

  (subset (send self :complement-factors effect)
          #'(lambda (X) (eq 'RANDOM (factor-type X))) ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; SIMPLE-RCF

(defmeth baproto :simple-RCF (effect)
  "Args: (effect)
Computes the simple random complement factors for effect."

  (let ((RC-factors (send self :random-CF effect)))
    (subset RC-factors
            #'(lambda (X) (null (intersection (nested-in X) RC-factors))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; SYMBOLIC-FACTORS

(defmeth baproto :symbolic-factors (effect)
  "Args: (effect)
Returns a list of the symbolic factors of an effect."

  (append effect (send self :dead-factors effect)) )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; SYMBOLIC-PRODUCT

(defmeth baproto :symbolic-product (effect)
  "Args: (effect)
Returns the symbolic product of an effect."

  (let ((dead-F (send self :dead-factors effect)))
    (flet ((symbolic-range (factor)
                           (cond
                             ((member factor dead-F)
                              (num-levels factor))
                             (t
                              (- (num-levels factor) 1) ))))
      (reduce #'*
              (mapcar #'symbolic-range
                      (send self :symbolic-factors effect) )))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; WRITE-EMS
;;; An effect is random if any one of its factors is random.

(defmeth baproto :write-ems (EMS-effects this-effect)
  "Agrs: (EMS-effects this-effect).
Writes the expected mean square of this-effect.  EMS-effects is the list of
effects that contribute to the expected mean square."

  (labels
   ((random-p (effect)
	      (eval `(or ,@(mapcar
			    #'(lambda (x) (eq 'RANDOM (factor-type x)))
			    effect ))))
    (left-p (symbol) (eq 'L symbol))
    (paren (left-right effect)
	   (if (random-p effect)
	       (if (left-p left-right) "(" ")")  ; random effect.
	     (if (left-p left-right) "[" "]")  ; fixed  effect.
	     ))
    
    (write-ems-ext (effect)
		   (format nil "~d*~a~a~a"
			   (send self :complement-prod effect)
			   (paren 'L effect)
			   (send self :effect-string effect)
			   (paren 'R effect))))
   (cond
    ((null EMS-effects)
     (format t "~a" (write-ems-ext this-effect)))
    (t
     (let* ((first-eff (write-ems-ext (first EMS-effects)))
	    (col (+ 7 (length (send self :effect-string this-effect))
		    (length first-eff)))
	    (next-eff nil) )
       (format t "~a" first-eff)
       (dolist (EMS-term (rest EMS-effects))
	       (setf next-eff (write-ems-ext EMS-term))
	       (when (> (+ col (length next-eff)) 75)  ; Does nicer formatting.
		     (format t "~%        ")
		     (setf col 8) )
	       (setf col (+ col 3 (length next-eff)))
	       (format t " + ~a" next-eff) ))))))
       
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; BALANCED-ANOVA-MODEL
;; Removes any previous menu named "Bal-ANOVA".

(defun balanced-anova-model ()
  "Args: ().
Creates a new balanced-anova object and prompts for an input file."

  (let*
    ((menu-list (send menu-proto :slot-value 'menu-list))
     (title-list (mapcar #'(lambda (x) (send x :title)) menu-list))
     (position (position "Bal-ANOVA" title-list :test #'equal)) )
    (if position (send (nth position menu-list) :remove)) )

  (let ((ba1 (send baproto :new)))
    (send ba1 :load-design)
    (send baproto :load-menu ba1)
    ba1 ))


                    ;;;;;;;;;;;;;;;;;;;;;;;;;;;
                    ;;; Auxiliary Functions ;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; GET-FILENAME
;; WARNING: The function SYMBOL-NAME returns only uppercase strings.
;;          Thus, the *.DSN file names must be all uppercase.

#+macintosh(defun get-filename () (open-file-dialog))
#+msdos(defun get-filename () (open-file-dialog))
#+unix(defun get-filename ()
  "Args: ().  Prompts user to select from a list of design, *.dsn, files."

  (system "ls *.DSN > ls.out")

  (let*
      ((lsfile (open "ls.out"))
       (list-of-choices      ; do* form gets a list of filenames as strings.

        (do* ((next-name 'bof (read lsfile nil '*eof*))
              (name-list nil (cons next-name name-list)))
             ((eq '*eof* next-name)
                 (reverse
                  (mapcar #'symbol-name (rest name-list)) ))))

       (index (if list-of-choices
                  (choose-item-dialog "Select a file." list-of-choices) ; if.
                (error "You must first create a design file, ~
                        and name it ___.dsn.")) ; else.
              )) ; end init section of let*.

    (if index (nth index list-of-choices)     ; if.
              (error "No file was selected.") ; else.  User selected CANCEL.
              )))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; REPLACE-NAME-WITH-OBJECT

(defmeth baproto :replace-name-with-object (name)
  "Args: (name)
Returns the factor-object having the given name.  Vectorized."

  (flet ((convert (x)
                  (nth (position x (send self :factor-names))
                                 (send self :factors))))
    (cond
      ((null name) nil)
      ((listp name)
       (mapcar #'convert (mapcar #'symbol-name name)) )
      (t
       (convert (symbol-name name)) ))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; LOAD-DESIGN-FILE
;;;
;;; This file is expected to have a name ending in '.dsn'
;;; and be a text file with one list per line.  Each list must
;;; have 4 elements in this order: <factor-name>, <number-of-levels>,
;;; <type>, <nesting-list>.  <type> must be either 'FIXED or 'RANDOM.
;;; <nesting-list> is a list of factor names in which <factor-name> is
;;; nested.

(defun load-design-file (&optional design-file-in)
  "Args: (&optional design-file-in)
Reads an input file for Hasse.lsp.  The file name is expected to end with .dsn"

  (let* ((design-file (if design-file-in
                          design-file-in        ; if-part.
                        (get-filename)))      ; else-partn.
         (infile (open design-file)))
    (format t "design-file: ~a~%" design-file)
    (do* ((factor-input 'bof (read infile nil '*eof*))
          (list-of-factors nil (append-factor factor-input list-of-factors)))
         ((eq '*eof* factor-input) list-of-factors) )))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; APPEND-FACTOR

(defun append-factor (factor-input factor-list)
  (if (eq factor-input '*eof*)
      factor-list
    (let ((next-factor (send factor-proto :new)))
      (send next-factor :name      (symbol-name (first factor-input)))
      (send next-factor :range     (second factor-input))
      (send next-factor :type      (third factor-input))
      (send next-factor :nested-in (fourth factor-input))
      (cons next-factor factor-list))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; FACTOR-NAME

(defun factor-name (factor)
  "Args: (factor).  Returns the name of factor."
  (send factor :name) )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; FACTOR-TYPE

(defun factor-type (factor)
  "Args: (factor).  Returns the type of factor (fixed or random)."
  (send factor :type) )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; NESTED-IN

(defun nested-in (factor)
  "Args: (factor).  Returns a list of factors in which factor is nested."
  (send factor :nested-in) )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; NUM-LEVELS

(defun num-levels (factor)
  "Args: (factor).  Returns the number of levels of factor."
  (send factor :range) )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; SORT-FACTORS
;;; Note: the slot FACTORS contains a reverse-order list of the
;;;       factors that have been input.  Thus, ">" means "less-than".

(defmeth baproto :sort-factors (factors)
  "Args: (factors).
Sorts factors according to their order in the slot factors."

  (let ((all-factors (send self :factors))
	(flist (copy-list factors)) )
    (flet ((less-than (x1 x2)
		      (> (position x1 all-factors)
			 (position x2 all-factors) )))
	  (sort flist #'less-than) )))


                     ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                     ;;; Hasse Diagram Routines ;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; STRUCTURE-DIAGRAM-PROTO

(defproto structure-diagram-proto '(root nodes) nil graph-proto)

(make-access structure-diagram-proto ':root 'root)
(make-access structure-diagram-proto ':nodes 'nodes)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Initialize a special factor: the grand mean (root node)

(send structure-diagram-proto :root
      (send factor-proto :new
            :name "u"
            :range 1
            :type 'FIXED
            :nested-in NIL
            :depth 0))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; COMPUTE-PARENTS

(defmeth structure-diagram-proto :compute-parents (factor)
  "Args: (factor)
Returns a list of the factors that are direct parents of FACTOR.
vectorized."

  (flet
    ((parents (factor)
              (let* ((ancestors (send factor :nested-in))
                     (result ancestors))
                (cond
                  ((null ancestors) (list (send self :root)))
                  (t (dolist (ancestor ancestors result)
                             (setf result (set-difference
                                           result
                                           (send ancestor :nested-in) ))
                                           ))))))
    (cond
      ((listp factor) (mapcar #'parents factor))
      (t (parents factor)) )))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; COMPUTE-DEPTH
;;; Note: flatten is defined in Hutils.lsp.

(defmeth structure-diagram-proto :compute-depth (factor)
  "Args: (factor)
Returns the depth of FACTOR in the hasse diagram."

  (labels
    ((eq-root (x) (equal x (send self :root)))
     (all-equal-root (y) (eval `(and ,@(mapcar #'eq-root y)))) )

    (if (eq-root factor) 0
        (let ((depth 0)
              (current-factors factor)
              (parents nil))

          (loop
           (setf parents
                 (flatten (send self :compute-parents current-factors)))
           (setf depth (+ 1 depth))
           (if (all-equal-root parents)
               (return depth) )
           (setf current-factors (flatten parents)) )))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; DO-CLICK
;;; This method will allow user to rearrange nodes in hasse diagram.
;;; Nodes can _only_ be moved laterally (x-direction).
;;; NODE-HERE returns T if the mouse click is near the given node.
;;; NODE-FOUND returns the found node, or NIL if no node is NEAR-HERE.

(defmeth structure-diagram-proto :do-click (x y m1 m2)
  "Args: (x y m1 m2).  Drag a node to a new location."

  (labels
    ((node-here (x1 y1 node)
                (and (< (abs (- x1 (send node :x))) 8)
                     (< (abs (- y1 (send node :y))) 12) ))
     (node-found (x2 y2)
                 (subset (cons (send self :root) (send self :nodes))
                         #'(lambda (z) (node-here x2 y2 z)) )))

    (if (node-found x y)
        (let ((node (first (node-found x y)))
              (xy (send self :drag-grey-rect x y 10 10 5 5)) )
          (send node :x (+ 5 (first xy)))
          (send self :redraw) ))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; DRAW-STRUCTURE
;;; 1. If the diagram does not exist create one.
;;; 2. Call the :redraw method of the structure-diagram.

(defmeth baproto :draw-structure ()
  "Args: ().  Draws the structure diagram."

  (if (not (send self :structure-diagram))
      (send self :structure-diagram
            (send structure-diagram-proto :new (send self :factors)
                  :title "Hasse Diagram") )
      (send (send self :structure-diagram) :show-window) ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ISNEW
;;; FIRST: call-next-method, to ensure REDRAWing occurs properly.
;;; 1. add factors to slot, nodes, reversing their order.
;;; 2. compute parents of each node in nodes.
;;; 3. compute depth of each node in nodes.
;;; 4. Compute the (x,y) location of each node.
;;; 5. Change the :x slot of the error term to x-center.

(defmeth structure-diagram-proto :isnew (factors &rest args)
  "Args: (factors &rest args).
Initializes the parents and depth of each factor in the list FACTORS."

  (apply #'call-next-method 1 args)
  (send self :nodes (append (send self :nodes) (reverse factors)))
  (let ((nodes (send self :nodes)))
    (dolist (node nodes)
            (send node :parents (send self :compute-parents node)))
    (dolist (node nodes)
            (send node :depth (send self :compute-depth node)))

    (flet
      ((have-depth (depth) #'(lambda (x) (eq depth (send x :depth)))))

      (let ((x-center 75)
            (root (send self :root))
            (max-depth (max (mapcar #'(lambda (x) (send x :depth)) nodes)))
            (cur-nodes nil)
            (cur-y 20)
            (first-x 30)
            (delta-y 75)
            (delta-x 50) )

        (send root :x first-x)
        (send root :y cur-y)
        (dolist (cur-depth (iseq 0 max-depth))
                (let ((cur-x first-x))
                  (dolist (cur-node (subset nodes (have-depth cur-depth)))
                          (send cur-node :x cur-x)
                          (send cur-node :y cur-y)
                          (setf cur-x (+ cur-x delta-x)) ))
                (setf cur-y (+ cur-y delta-y)) )))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; REDRAW
;;; 1. Draw the root node.
;;; 2. Draw the other nodes, one level at a time, adding upward links
;;;    to each node's parent.

(defmeth structure-diagram-proto :redraw ()
  "Args: ().  Draws the structure diagram."

  (let* ((nodes (send self :nodes))
         (max-depth (max (mapcar #'(lambda (x) (send x :depth)) nodes)))
         (offset (+ 2 (send self :text-ascent)))
         (cur-nodes nil) )

    (labels
      ((draw-node (node)
                  (let ((text (format nil "~a(~d)"
                                      (factor-name node) (num-levels node)))
                        (x (send node :x))
                        (y (send node :y)) )
                    (send self :draw-text text x y 1 0)
                    (if (eq 'FIXED (factor-type node))
                      (let ((half-w
                             (round (/ (send self :text-width text) 2))))
                        (send self :draw-line
                              (- x half-w) (+ y 3)
                              (+ x half-w) (+ y 3) )))))
       (draw-link (node parent)
                  (send self :draw-line
                        (send node :x) (- (send node :y) offset)
                        (send parent :x) (+ 3 (send parent :y)) ))
       (draw-links (node)
                   (mapcar #'(lambda (x) (draw-link node x))
                           (send node :parents) ))
       )
      (send self :erase-window)
      (draw-node (send self :root))
      (dolist (cur-node nodes)
              (draw-node cur-node)
              (draw-links cur-node) ))))


                     ;;;;;;;;;;;;;;;;;;;;;
                     ;;; Menu Routines ;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; LOAD-MENU

(defmeth baproto :load-menu (obj)
  (if (send obj :menu)
      (send (send obj :menu) :remove) )
  (send self :install-menu obj) )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; INSTALL-MENU

(defmeth baproto :install-menu (obj)
  (setf load-design-item
        (send menu-item-proto :new "Load a Design ..."
              :action #'(lambda () (send obj :load-design)) ))

  (setf structure-item
        (send menu-item-proto :new "Structure Diagram"
              :action #'(lambda () (send obj :do-structure-diagram)) ))

  (setf linear-model-item
        (send menu-item-proto :new "Linear Model"
              :action #'(lambda () (send obj :do-linear-model)) ))

  (setf anova-table-item
        (send menu-item-proto :new "Show ANOVA Table"
              :action #'(lambda () (send obj :do-anova-table)) ))

  (setf ems-list-item
        (send menu-item-proto :new "Expected Mean Squares"
              :action #'(lambda () (send obj :do-ems-list)) ))

  (setf factor-sets-item
        (send menu-item-proto :new "Factor Sets ..."
              :action #'(lambda () (send obj :do-factor-sets)) ))

  (setf dash-item (send dash-item-proto :new ))

  (setf exit-item
        (send menu-item-proto :new "Exit LISP-STAT"
              :action #'exit))

  (let
    ((menu (send obj :menu (send menu-proto :new "Bal-ANOVA"))))
    (send menu :append-items
          dash-item
          load-design-item
          structure-item
          linear-model-item
          anova-table-item
          ems-list-item
	  factor-sets-item
          dash-item
          exit-item)
    (send menu :install) )

  ) ; install-menu


                     ;;;;;;;;;;;;;;;;;
                     ;;; Utilities ;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ADDTOEACH
;;; 2/22/92: When I tried changing (append (list item) (first x))
;;; to (append (first x) (list item)), I got 51 allowable effects
;;; for the LLL design instead of the 30 that I should get!!?

(defun addtoeach (item X)
  "Args: (item X).  Adds item to each top-level list in X"

  (cond
      ((null X) nil)
      (t (cons (append (list item) (first X))
               (addtoeach item (rest X)) ))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; COMPLEMENT
;;; Set-difference happens to
;;; reverse the original order.

(defun complement (set1 universe)
  "Args: (set1 universe)
Computes the complement of set1 with respect to universe."

  (reverse (set-difference universe set1)) )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; EQUAL-SETS-P

(defun equal-sets-p (s1 s2)
  "Args: (s1 s2).  Returns T if the sets s1 and s2 are the same."

  (and (null (set-difference s1 s2))
       (null (set-difference s2 s1)) ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; FLATTEN

(defun flatten (L)
  "Args: (L), flattens L, reducing sub-lists to their individual elements."
  (cond
    ((null L) L)
    ((atom L) (list L))
    (t (append (flatten (car L))
               (flatten (cdr L)) ))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; G-REMOVE

(defun g-remove (item-list master-list)
  "Args: (item-list master-list)
Removes each item in item-list from master-list."

  (if (null item-list)
      master-list           ; if-part.
    (reverse                ; else-part.
     (reduce #'intersection
             (mapcar #'(lambda (x) (remove x master-list :test #'equal))
                     item-list )))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; MORE

(defun more (N)
  "Args (N).  Stop scrolling if N = 0"

  (cond
      ((= N 0)
          (format t "---Press RETURN for more---")
          (read-char)
          ': )))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; REMDUPS

(defun remdups (X)
  "Args: (X).  Removes duplicate top-level elements in X"

  (cond
      ((null X) nil)
      ((member (first X) (rest X) :test #'equal)  ; CAR is a member of CDR.
          (remdups (rest X)))
      (t  (cons (first X) (remdups (rest X)))) ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; SUBSET

(defun subset (set test)
  "Args: (set test).  Returns the items from set for which test is true."

  (if (null set)
      nil
    (select set (which (mapcar test set))) ))


;;;; END OF FILE.

(format t "~%To get started: (def ba (balanced-anova-model))~%")

