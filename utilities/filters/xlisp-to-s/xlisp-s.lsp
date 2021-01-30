;;;; CL-TO-S: A package to call the statistical package S
;;;
;;;  R.W. Oldford, January 1989 rwoldford@water.waterloo.edu  
;;;  Martin B. Maechler, June 1990, maechler@stat.washington.edu
;;;  Xlisp Lisp-Stat adaptation:
;;;  Steve McKinney, October 1991, kilroy@biostat.washington.edu
;;;
;;; The package is not terribly sophisticated but gets the job done. 
;;; If you make *improvements* please add your name, the date, and
;;; your e-mail address to the bottom of the above list.
;;; Then forward your changes to all previous authors.... rwo.
;;; 
;;;
;;; The functions make use of the files (as defaults)
;;; /tmp/username/S.to.CL and /tmp/username/CL.to.S as scratch files.
;;; This could be changed by making appropriate global changes to this
;;; file.
;;;
;;;
;;; Call this package CL-TO-S
;;;
;;; (in-package 'CL-TO-S :nicknames '("S"))
;;;
;;;; Xlisp-S: Modified CL-TO-S package using Xlisp instead of
;;;;          Common Lisp.
;;;
;;; Because the Xlisp language does not include all Common Lisp
;;; functions, portions of the original Common Lisp code have
;;; been commented out.  Xlisp alternatives have been substituted.
;;; The original Common Lisp code has been left in, in comment
;;; form, to convey the original intent of the functions.
;;;
;;; Since Xlisp, Lisp-Stat do not have Common Lisp routines 
;;; "user-homedir-pathname" and "directory-namestring", the 
;;; user must set up a file named "userhome.lsp" containing 
;;; the Xlisp command
;;;
;;; (defparameter *HOME* "/pathway to user's home directory/")
;;;
;;; example:
;;; (defparameter *HOME* "/belgica-2g/kilroy/")
;;;

(load "userhome") ; sets up the *HOME* global variable

;;; Typically users will interact through the single function "eval-in-S"
;;; A second function "names" is provided to extract the component names
;;; (if any) of an S-like list (actually extracts the car of a list of pairs).

;; (export '(eval-in-S eval-in-S-running stop-S-running names))

;;;
;;; The following relies on the Kyoto Common Lisp escape to Unix(tm).
;;; If you're using another Common Lisp, changes will be necessary wherever
;;; the KCL function "system" is found.
;;; For example, Franz Extended CL uses 'shell' where KCL uses 'system', so
;;; one could just uncomment the following for Franz:
;;; 
;;;>>>>>>>>>>>>>>>>> For Allegro CL  &  Franz Ext. CL : <<<<<<<<<<<<<<<<< 
;;;
;;; (defmacro system (command) `(shell , command))
;;;           ~~~~~~             ~~~~~
;;;

(def S-program-name "S")	;-- default --
(def S-program-name "Splus")	;  on belgica.stat.wa..

;;;--- This is the old function from  Wayne Oldford  which starts and shuts down
;;;--- its own S.  Has also other flaws: e.g. File-names make it dangerous 
;;;--- for 2 persons using this on the same machine.

(defun eval-in-S (S-fun-name-string arglist
		  &key (file-name "/tmp/CL.to.S")  (if-exists ':supersede)
		       (S-temp-file "/tmp/S.to.CL")
		       (S-transcript-file NIL))
  (rm-file  S-temp-file)
 
  ;;-- Call S and write the result to S-temp-file.
  (flet ((S-eval (S-fun-name arglist stream S-temp-file)
	   ;; Load the S to CL code into S
	   ;;
	   ;;-MM- next line was: >> 	(princ "library('CL+S')" stream)
	   (S-write-cmd "attach.if.necessary" '(".Data/.CL.data") stream)
	   (terpri stream)
	   
	   ;; Have S write out the value of our S-function call
	   ;;
	   (princ "CL.write.val(" stream)
	   (S-write-cmd S-fun-name arglist stream)
	   (princ ", " stream)
	   (princ "file=" stream)
	   (S-write-string S-temp-file stream)
	   (princ ", " stream)
	   (princ "append=TRUE" stream)
	   (princ ")" stream)
	   (terpri stream)
	   ))
    (with-open-file
	(S-cmd-file file-name :direction :output :if-exists if-exists)
      (S-eval S-fun-name-string arglist S-cmd-file S-temp-file))
  )
  (if S-transcript-file
      (system (concatenate 'string
		S-program-name " < " file-name " > " S-transcript-file))
    (system (concatenate 'string S-program-name " < " file-name )))
  
  ;;-- Now just read to return the result.
  ;;-- read will just return the an unevaluated list that will
  ;;-- construct the right CL object when eval'ed (evaluation is necessary).

  (let (ans)
    (with-open-file (CL-fun-as-list S-temp-file :direction :input)
      (setf ans (eval (read CL-fun-as-list))))
    ans)
)

;;; XLISP does not have a probe-file predicate so make one up.

(defun probe-file (file-name)
  (open file-name))

(defun rm-file (file-name)
  (if (probe-file file-name)
      (system (concatenate 'string "rm " file-name))))

(defun names (S-struct)
  (if (listp S-struct)
      (map 'list
	#'(lambda (x) (if (consp x) (car x) ()))
	S-struct))
  )
	

(defun S-eval (S-fun-name arglist stream S-out-file S-ready-fname 
	       &key (new-S-process (null *S-process-running*)))
  "Call an  S - function on Common LISP objects.  Returns its results."
;  (declare (special  *S-process-running* *HOME*))

  ;; Load the S to CL code into S
  ;;
  (when  new-S-process
    (S-write-attach-Data (concatenate 'String *HOME* "xlisp.S/.Data") stream)
    (S-write-attach-Data (concatenate 'String *CL.Data*) stream :pos 1)
    ;;-- If S is started with 
    ;;-- > (S-CL-initialize)
    ;;-- then *CL.Data* will be <your home directory>/.Data/.CL.data
    ;;
    ;;-- If S is started with (S-CL-initialize <data directory path>)
    ;;-- e.g. (S-CL-initialize "/belgica-2g/kilroy/xlispstat/.Data")
    ;;-- then *CL.Data* will be <data directory path>
  )
  (princ "CL.write.val(" stream)
  (S-write-cmd S-fun-name arglist stream)
  (princ ", file=" stream)  (S-write-string  S-out-file  stream)
  (princ ", append=TRUE" stream)
  (princ ", ready.file=" stream)  (S-write-string  S-ready-fname  stream)
  (princ ", ack.ready=TRUE" stream)
  (princ ")" stream)
  (terpri stream)
)

;;;-- this function is not yet called by any other !!! ----
(defun S-assign (S-symbol-name  S-fun-name  arglist
		 stream  S-out-file  S-ready-fname 
		 &key (new-S-process (null *S-process-running*)))
  "Assign both in 'S' and 'Common LISP' the result of 'S'-function (arglist).
   Use   S-write-assign   for assignment in S only (w/o feedback to CLISP)"
;  (declare (special  *S-process-running*))

  ;; Load the S to CL code into S
  ;;
  (when  new-S-process
    (S-write-attach-Data (concatenate 'String *HOME* "xlisp.S/.Data") stream)
(terpri)
(princ "defun S-assign - S-write-attach-Data   xlisp.S/.Data")
(terpri)

  )

  ;; Have S write out the value of our S-function call
  ;; ---- Assign both in 'S' and 'Common LISP'
  ;;
  (princ S-symbol-name  stream)   (princ " <- "  stream)
  (S-write-cmd S-fun-name arglist stream)
  (terpri stream)
  (princ "CL.write.assign(" stream)
  ;;      ~~~~~~~~~~~~~~~	   ---- Assign into 'COMMON LISP'
  (princ S-symbol-name  stream)
  (princ ", file=" stream)  (S-write-string  S-out-file  stream)
  (princ ", append=TRUE" stream)
  (princ ", ready.file=" stream)  (S-write-string  S-ready-fname  stream)
  (princ ", ack.ready=TRUE" stream)
  (princ ")" stream)
  (terpri stream)
)

(defun S-write-assign (S-name S-fun-name-string arglist stream)
  (princ S-name stream)
  (princ " <- " stream)
  (S-write-cmd S-fun-name-string arglist stream)
  (terpri stream)
)

(defun S-write-attach-Data (.Data-name-string stream  &key pos)
;;-- works for   default : pos = NIL  <==> S: empty pos == S-default --
  (S-write-cmd "attach" (list .Data-name-string  pos) stream) 
  (terpri stream))

(defun S-write-activate-assignments (stream)
  (S-write-cmd "synchronize" nil  stream)
  (terpri stream))

;; what for ?? --
;(defun S-test  (test-fun test-arg
;		&key (file-name "CL.to.S") 
;		     (if-exists ':supersede))
;  (with-open-file
;      (S-cmd-file file-name :direction :output :if-exists if-exists)
;    (apply test-fun (list test-arg S-cmd-file))))

;; cover function for 'S-write-cmd',  what for ?? --
;(defun S-write  (S-fun-name-string arglist
;		 &key (file-name "CL.to.S")
;		      (if-exists ':supersede))
;  (with-open-file
;      (S-cmd-file file-name :direction :output :if-exists if-exists)
;    (S-write-cmd S-fun-name-string arglist S-cmd-file)))


(defun S-write-cmd (S-fun-name-string arglist stream)
  "Called by low - & high level functions "
  (princ S-fun-name-string stream)
  (princ "(" stream)
  (let ((nargs (length arglist)))
    (cond
     ((zerop nargs)	(princ  ")" stream))
     (T
      (dotimes (i (- nargs 1))
	(S-write-val (elt arglist i)  stream)
	(princ "," stream))
      (S-write-val (elt arglist (- nargs 1)) stream)
      (princ ")" stream))
)))

;(defun S-write-val  (CL-value stream)
;  "Dispatcher function"
;  (typecase CL-value
;    (symbol	(S-write-sym		CL-value stream))
;    (string	(S-write-string		CL-value stream))
;    (integer	(S-write-num		CL-value stream))
;    (rational	(S-write-rat		CL-value stream))
;    (complex	(S-write-complex 	CL-value stream))
;    (float	(S-write-num		CL-value stream))
;    (array	(S-write-array		CL-value stream))
;    (vector	(S-write-vector		CL-value stream))
;    (cons	(S-write-named-arg	CL-value stream)) ;; <<-- e.g. 'pos = 2'
;  )
;)
;;;
;;; No "typecase" function in Xlisp, so re-write the dispatcher.
;;;

(defun S-write-val  (CL-value stream)
  "Dispatcher function"
  (cond ((symbolp CL-value)    (S-write-sym       CL-value stream))
        ((stringp CL-value)    (S-write-string    CL-value stream))
        ((integerp CL-value)   (S-write-num       CL-value stream))
        ((realp CL-value)      (S-write-num       CL-value stream))
        ((vectorp CL-value)    (S-write-vector    CL-value stream))
        ((arrayp CL-value)     (S-write-array     CL-value stream))
        ((consp  CL-value)     (S-write-named-arg CL-value stream)) ; <<-- e.g. 'pos = 2'
        (t (princ "error in defun S-write-val - type not matched."))))


(defun S-write-sym (symbol stream)
  (if symbol ;-- NOT == 'nil
      (princ symbol stream)))

(defun S-write-named-arg (name-arg-pair stream)
  "name-arg-pair == (cons <string>  <atom>);
  If 'name' starts by 'S-' then 'arg' (then also string) is meant as S-object."
;; E.G. (S-write-named-arg '("S-list" .  "ls(pos=2)") t)   --> list=ls(pos=2)

  (let ((name (car name-arg-pair))
	(arg  (cdr name-arg-pair)))
    (cond ( (equal (subseq name 0 2) "S-") ;;-- then give S-object as argument !
	  ;  (check-type  arg  String)
;
; Xlisp warning in place of check-type loop
;
	    (if (not (stringp arg))
		(princ "error in S-write-named-arg - (cdr name-arg-pair) not a string."))
	    (setf name (subseq name 2 (length name)))
	    (when (not (equal name ""))
	      (princ  name  stream)
	      (princ "=" stream))
	    ;;--  use no ".."  for name of the S-object: princ instead of write
	    (princ arg  stream))
	  (t (princ  name  stream)
	     (princ "=" stream)
	     (S-write-val  arg  stream))
    )
))


;(defun S-write-string (string stream)
;  (princ string stream))
(defun S-write-string (string stream)
  (print string stream))

(defun S-write-num (num stream)
  (princ num stream))


(defun S-write-rat (num stream)
  (S-write-val (coerce num 'float) stream))


(defun S-write-complex (num stream)
  (S-write-cmd	"complex"
		(list	(cons "real" (realpart num))
			(cons "imaginary" (imagpart num)))
		stream))


(defun S-write-vector (vec stream)
  (S-write-cmd "c" vec stream))


(defun S-write-array (array stream)
  (case (array-rank array)
    (1		(S-write-vector array stream))
    (2		(S-write-matrix array stream))
    (otherwise	(S-write-any-array array stream))))


(defun S-write-matrix (array stream)
  (princ "matrix" stream)
  (princ "(" stream)
  (princ "c" stream)
  (princ "(" stream)

  (let ( (nrows-1 (1- (array-dimension array 0)))
	 (ncols-1 (1- (array-dimension array 1)))
       )
    
    (do ((j 0 (+ j 1))) ((> j ncols-1))	; all cols.
      
      (do ((i 0 (+ i 1))) ((> i nrows-1)) ; all rows.
	
	(princ (aref array i j)  stream) ; write element.
	(unless	(and (= j ncols-1) (= i nrows-1))
	  (princ "," stream))
      )
    )

    (princ ")" stream)
    (princ "," stream)
    (princ (1+ nrows-1) stream)
    (princ "," stream)
    (princ (1+ ncols-1) stream)
    
  )
  (princ ")" stream)
)


(defun S-write-any-array (array stream)

  ;; Note that S's array
  ;; function likes to fill the array up by travelling fastest
  ;; over the first index, then the second, ...
  ;; And Common-Lisp does it the other way around, travelling fastest
  ;; over the last index, then the second last ... sigh.
  ;; Hence the reverse on the array-dimensions.
  
  (princ "aperm" stream)
  (princ "(" stream)
  (princ "array" stream)
  (princ "(" stream)

  (S-write-vector			; refer to elements
   (make-array (array-total-size array)	; as a single vector.
	       :displaced-to array) stream)
  (princ "," stream)
  
  (S-write-vector
   (reverse (array-dimensions array))
   stream)
  (princ ")" stream)
  (princ "," stream)
  
  (princ (length (array-dimensions array)) stream)
  (princ ":1" stream)
  (princ ")" stream)
)
        
;;;;========== Entirely new stuff from Martin Maechler ==============
;;;;========== Xlisp Lisp-Stat modifications from      ==============
;;;;========== Steve McKinney                          ==============

;;### There is one big problem, if you are using this extensively:
;;### If several (eval-in-S-running ...) are executed in a row,
;;### they all use the same local file-names. (problem ??)
;;### A plot may be started before the device is really ready !!!
;;### (this happens !! if you start X11, and do plot and lines...)

;;===> Solution (?): Use  (setf <sym> (eval-....)) 
;;=== 		     such that LISP waits until completion 
;;=== 		     before doing the next one. Seems to work.

(defun system-touch (file-name)
    (system (concatenate 'String "touch " file-name)))

(defvar *S-process-running*)
;(defvar *HOME*)
(defvar *tmp-dir*)
(defvar *CL.Data*)

;;-- to test do beforehand:    cd; 'rm' -r tmp; 'rm' -r /tmp/maechler
;;--- then a second time (now /tmp/maechler should already exist)
;;-- and then later:           cd; mkdir tmp

;;;==========================================================================
;;; This function initializes the whole process
;;; Examples:
;;; (S-CL-initialize)
;;;         uses the default data directory 
;;;         <your home directory>/.Data/.CL.data
;;; (S-CL-initialize "/belgica-2g/kilroy/xlispstat/.Data")
;;;         uses the specified data directory 
;;;         "/belgica-2g/kilroy/xlispstat/.Data"

(defun S-CL-initialize (&optional S-Data-directory) 
;  (declare (special *S-process-running*  *HOME*  *tmp-dir*  *CL.Data*))
  (setf *S-process-running* nil)
;  (setf *HOME* (directory-namestring (user-homedir-pathname)))
  (setf *tmp-dir* (concatenate 'String *HOME* "tmp/"))

  (if (not (probe-file *tmp-dir*)) ;;-- use the public one, make own sub-dir.
      (let* ( (le (1- (length *HOME*)))
	      (home (subseq (subseq *HOME* 0 le) 1 le)) ;;- w/o first & last '/'
	      (login-name (subseq  home (1- (- le (position #\/
		    (reverse (coerce home 'list))))) (1- le)))
	      tmp-dir)
	(setf tmp-dir (concatenate 'String "/tmp/" login-name))
	(setf *tmp-dir* (concatenate 'String  tmp-dir "/"))
	(if (not (probe-file *tmp-dir*))
;	    (assert (zerop (system (concatenate 'String "mkdir " tmp-dir))))
	    (system (concatenate 'String "mkdir " tmp-dir))
	)))
  
  (if (not (probe-file *tmp-dir*))
      (error " Error: couldn't create or find a 'tmp' directory ~%~
     *HOME* = ~A     *tmp-dir* = ~A ~%" *HOME* *tmp-dir*)
    ;; ELSE --> 
    (format t "
 >>>>>> Using directory  ~A  for temporary files ~%" *tmp-dir*))

  (if  S-Data-directory
      (setf *CL.Data* S-Data-directory)
    (progn 
      (setf *CL.Data* (concatenate 'String *HOME* ".Data/.CL.data"))
      (if (not (probe-file *CL.Data*))
	  (system (concatenate 'String "mkdir "  *CL.Data*)))))
  
  (if (not (probe-file *CL.Data*))
      (error " Error: couldn't find  S-Data  directory ~S ~%~%" *CL.Data*)
    (format t "
 >>>>>> Using directory  ~A  for S - data objects ~%" *CL.Data*))
  
)


(defun S-process-check (new-S-process
			&key (S-process-sleep-seconds 1)
			     cmd-file-name   (if-exists-cmd ':supersede)
			     input-file-name
			     new-file-name
			     stop-file-name
			     S-out-file-name
			     S-ready-file-name
			     transcript-file-name)
  "Check or Open  a new S-process running in background"
;  (declare (special *S-process-running*))
    
  (when (not new-S-process)
    ;;-- Make sure that there is good indication of an S-process running
    ;;   and waiting for lisp
    (if (or 
	 (not (probe-file cmd-file-name))
	 (not (= 0 (system "test 0 -ne `ps -x | grep cmd/S | grep -v grep | wc -l`"))))
	(format t "ATTENTION: There might not be a proper S-process running~
           ~%============================================================~%~%"))
  )
  
  (rm-file  S-out-file-name)
  (rm-file  new-file-name)
  (rm-file  stop-file-name)
  (rm-file  S-ready-file-name)
  
  (when new-S-process
    
    ;;-- The cmd-file: will start the "background S"
    ;;
    (terpri)
    (format t "Starting a background S - process. . . . Be patient")
    (terpri)
    (with-open-file
	(S-cmd-file cmd-file-name :direction :output 
	 :if-exists if-exists-cmd)
      (S-eval "start.bg" (list (cons "sleep.seconds" S-process-sleep-seconds)
			       (cons "stop.file"  stop-file-name)
			       (cons "new.file"   new-file-name)
			       (cons "input.file" input-file-name))
	      S-cmd-file  S-out-file-name  S-ready-file-name
	      :new-S-process T))
    
    ;;-- Start the "background S" with a loop 'waiting' for input : ---
    ;;
    (if transcript-file-name
	(system (concatenate 'string
		  S-program-name "<" cmd-file-name 
		  ">" transcript-file-name "&"))
      (system (concatenate 'string
		S-program-name "<" cmd-file-name  "&")))
    ;;-- "&"  VERY IMPORTANT: MUST run in background !!--
    
    (setf *S-process-running* T)
))


(defun assign-in-S-running (S-symbol-function-arglist-list
			    &key (.Data-path *CL.DATA*)
				 (new-S-process (null *S-process-running*))
				 (S-process-sleep-seconds 1)
				 (tmp-directory-name *tmp-dir*)
				 (cmd-fname "CL.to.S") 
				 (if-exists-cmd ':supersede)
				 (input-fname "S.bg-input")
				 (if-exists-input ':supersede)
				 (new-fname "S.bg-new")
				 (stop-fname "S.bg-stop")
				 (S-out-fname "S.to.CL")
				 (S-ready-fname "S.ready")
				 (transcript-fname "S.to.CL.transcript"))
;  (declare (special  *S-process-running*))
  (let ((cmd-file-name (concatenate 'String tmp-directory-name  cmd-fname))
	(input-file-name (concatenate 'String tmp-directory-name input-fname))
	(new-file-name (concatenate 'String tmp-directory-name new-fname))
	(stop-file-name (concatenate 'String tmp-directory-name stop-fname))
	(S-out-file-name (concatenate 'String tmp-directory-name S-out-fname))
	(S-ready-file-name 
	 (concatenate 'String tmp-directory-name S-ready-fname))
	(transcript-file-name
	 (concatenate 'String tmp-directory-name transcript-fname))
	(symbol-list nil)
       )
    
    (S-process-check  new-S-process
		      :S-process-sleep-seconds  S-process-sleep-seconds
		      :cmd-file-name  cmd-file-name   
		      :if-exists-cmd  if-exists-cmd
		      :input-file-name  input-file-name 
		      :new-file-name  new-file-name
		      :stop-file-name  stop-file-name
		      :S-out-file-name  S-out-file-name
		      :S-ready-file-name  S-ready-file-name
		      :transcript-file-name  transcript-file-name)
        
    ;;-- The input-file: contains the real S-commands. Will be read by S.
    ;;
    (with-open-file
	(S-input-file input-file-name :direction :output 
	 :if-exists if-exists-input)
      ;(S-write-attach-Data  .Data-path  S-input-file  :pos 1)

      (let (symbol fun-name arglist)
	(dolist (symbol-function-arglist S-symbol-function-arglist-list)
	  (setf symbol   (first  symbol-function-arglist))
	  (setf fun-name (second symbol-function-arglist))
	  (setf arglist  (third  symbol-function-arglist))
	  
	  (check-arglist arglist)
	  (S-write-assign symbol fun-name arglist S-input-file)
	  (setf symbol-list (cons symbol symbol-list))
	  ;;-- NO waiting for feedback 
	) )
      (S-write-activate-assignments S-input-file)
    )
    ;;-- tell the background 'S' that there is new input: ---
    ;;
    (system-touch new-file-name)
    
    ;;-- Now check if the variables are assigned.
    ;;-- This could be done after waiting for a while here,
    ;;-- but it seems better to do it later:
    ;;-- E.G.		(setf var-list (assign-in-s-running ......))
    ;;-- To check somewhat later (using my extended 'ls' function):
    ;;-- 		(eval-in-s-running "ls" (list var-list))
	
    (apply #'Vector symbol-list)
))

;;;
;;; Modify "assign-in-S-running" function to wait for completion
;;; of the background S job before proceeding.
;;; When several assignments in S are required with successive
;;; assignments depending on their predecessors, "assign-in-S-running"
;;; can produce bad results because later requests can be started before
;;; earlier assignments in the background S are completed.
;;;
;;; "assign-in-S-running-and-wait" first removes the S symbol name,
;;; then checks if the symbol has been reassigned its new value,
;;; then exits.  Steve McKinney Feb 1992
;;;

(defun assign-in-S-running-and-wait (S-symbol-function-arglist-list
			    &key (.Data-path *CL.DATA*)
				 (new-S-process (null *S-process-running*))
				 (S-process-sleep-seconds 1)
				 (tmp-directory-name *tmp-dir*)
				 (cmd-fname "CL.to.S") 
				 (if-exists-cmd ':supersede)
				 (input-fname "S.bg-input")
				 (if-exists-input ':supersede)
				 (new-fname "S.bg-new")
				 (stop-fname "S.bg-stop")
				 (S-out-fname "S.to.CL")
				 (S-ready-fname "S.ready")
				 (transcript-fname "S.to.CL.transcript"))
;  (declare (special  *S-process-running*))
  (let ((cmd-file-name (concatenate 'String tmp-directory-name  cmd-fname))
	(input-file-name (concatenate 'String tmp-directory-name input-fname))
	(new-file-name (concatenate 'String tmp-directory-name new-fname))
	(stop-file-name (concatenate 'String tmp-directory-name stop-fname))
	(S-out-file-name (concatenate 'String tmp-directory-name S-out-fname))
	(S-ready-file-name 
	 (concatenate 'String tmp-directory-name S-ready-fname))
	(transcript-file-name
	 (concatenate 'String tmp-directory-name transcript-fname))
	(symbol-list nil)
       )
    
    (S-process-check  new-S-process
		      :S-process-sleep-seconds  S-process-sleep-seconds
		      :cmd-file-name  cmd-file-name   
		      :if-exists-cmd  if-exists-cmd
		      :input-file-name  input-file-name 
		      :new-file-name  new-file-name
		      :stop-file-name  stop-file-name
		      :S-out-file-name  S-out-file-name
		      :S-ready-file-name  S-ready-file-name
		      :transcript-file-name  transcript-file-name)
        
    ;;-- The input-file: contains the real S-commands. Will be read by S.
    ;;
    (with-open-file
	(S-input-file input-file-name :direction :output 
	 :if-exists if-exists-input)
      ;(S-write-attach-Data  .Data-path  S-input-file  :pos 1)

      (let (symbol fun-name arglist)
	(dolist (symbol-function-arglist S-symbol-function-arglist-list)
	  (setf symbol   (first  symbol-function-arglist))
	  (setf fun-name (second symbol-function-arglist))
	  (setf arglist  (third  symbol-function-arglist))
	  
	  (check-arglist arglist)
	  ;; - now remove the symbol name from S directory if it already exists
	  ;;
	  (let ((S-symbol-name (concatenate 'string *CL.Data* "/" symbol)))
	    (if (probe-file S-symbol-name)
		(system (concatenate 'string "rm " S-symbol-name))))
	  (S-write-assign symbol fun-name arglist S-input-file)
	  (setf symbol-list (cons symbol symbol-list))
	  ;;-- NO waiting for feedback 
	) )
      (S-write-activate-assignments S-input-file)
    )
    ;;-- tell the background 'S' that there is new input: ---
    ;;
    (system-touch new-file-name)
    
    ;;-- Now check if the variables are assigned.
    ;;
    (dolist (symbol symbol-list)
      (loop
      (if (probe-file (concatenate 'string *CL.Data* "/" symbol)) (return))
	(sleep 1)))
    ;;-- The assignment is done, so exit
    ;;
    (apply #'Vector symbol-list)
))

(defun eval-in-S-running  (fun-name-string  &optional arglist 
			   &key (new-S-process (null *S-process-running*))
				(S-process-sleep-seconds 1)
				(tmp-directory-name *tmp-dir*)
				(cmd-fname "CL.to.S") 
				(if-exists-cmd ':supersede)
				(input-fname "S.bg-input")
				(if-exists-input ':supersede)
				(new-fname "S.bg-new")
				(stop-fname "S.bg-stop")
				(S-out-fname "S.to.CL")
				(S-ready-fname "S.ready")
				(transcript-fname "S.to.CL.transcript"))
;  (declare (special  *S-process-running*))
  (let ((cmd-file-name (concatenate 'String tmp-directory-name  cmd-fname))
	(input-file-name (concatenate 'String tmp-directory-name input-fname))
	(new-file-name (concatenate 'String tmp-directory-name new-fname))
	(stop-file-name (concatenate 'String tmp-directory-name stop-fname))
	(S-out-file-name (concatenate 'String tmp-directory-name S-out-fname))
	(S-ready-file-name 
	 (concatenate 'String tmp-directory-name S-ready-fname))
	(transcript-file-name
	 (concatenate 'String tmp-directory-name transcript-fname))
       )
    
    (S-process-check  new-S-process
		      :S-process-sleep-seconds  S-process-sleep-seconds
		      :cmd-file-name  cmd-file-name   
		      :if-exists-cmd  if-exists-cmd
		      :input-file-name  input-file-name 
		      :new-file-name  new-file-name
		      :stop-file-name  stop-file-name
		      :S-out-file-name  S-out-file-name
		      :S-ready-file-name  S-ready-file-name
		      :transcript-file-name  transcript-file-name)
    
    ;;-- The input-file: contains the real S-commands. Will be read by S.
    ;;
    (check-arglist arglist)
    (with-open-file
	(S-input-file input-file-name :direction :output 
	 :if-exists if-exists-input)
      (S-eval fun-name-string arglist S-input-file S-out-file-name 
	      S-ready-file-name :new-S-process NIL))
    
    ;;-- tell the background 'S' that there is new input: ---
    ;;
    (system-touch new-file-name)
    
    ;;-- Now "just" read to get the result.
    ;;-- read will just return  an unevaluated list that will
    ;;-- construct the right CL object when eval'ed (evaluation is necessary).
    
    ;;-- You might have to wait a while !!
    (format t " LISP waiting for S result file:")
    (terpri)
    (do ((i 1 (+ i 1)))  ((and (probe-file S-ready-file-name) 
			       (probe-file S-out-file-name)))
      (sleep 1)  (format t ".")
      (if (= 0 (mod i 50))  (format t " : ~d ~%" i))
    ) ;-- end-do
    (sleep 1) 
    ;;--- if there is no waiting, the file may not yet be completely written.
    (let (ans)
      (with-open-file (CL-fun-as-list S-out-file-name :direction :input)
	(setf ans (eval (read CL-fun-as-list))))
      ans)
))

(defun sleep (num)
  (let ((bignum (* num 1000))
        (junkspace 0.0))
    (dotimes (i bignum)
      (setq junkspace (+ junkspace 1)))))

(defun get-S-value (S-expression-string)		
  (eval-in-S-running "" (list (cons "S-" S-expression-string))))


(defun stop-S-running  ( &optional (stop-file "S.bg-stop")
				   (tmp-directory-name *tmp-dir*))
;  (declare (special  *S-process-running*))
  (let ((stop-file-name (concatenate 'String tmp-directory-name  stop-file)))
    (system-touch stop-file-name)
    (system (concatenate 'string "rm -r " tmp-directory-name "*CL*"))
    (setf *S-process-running* nil)))


(defun check-arglist (arglist)
  "Check if 'arglist' has proper structure"
;  (assert (listp arglist) (arglist) 
;    "'arglist' must be a list, but is ~A~%" (type-of arglist))
  (if (not (listp arglist))
    (format nil "'arglist' must be a list, but is ~A~%" (type-of arglist)))
;  (dolist (arg arglist)
;    (assert 
;	(or (atom arg) 
;	    (and (consp arg)
;		 (atom (car arg))
;		 (subtypep (type-of (car arg)) 'String)
;		 (not (listp (cdr arg))) ;--- not NIL, nor a proper list 
;	    )
;	)
;        ()
;	" S-arg must be either <atom> or  '(cons <string> <atom>)'. Is ~A~%" 
;      arg)))
  (dolist (arg arglist)
    (if (not 
	(or (atom arg) 
          (and (consp arg)
		 (atom (car arg))
;		 (subtypep (type-of (car arg)) 'String)
		 (stringp (car arg))
		 (not (listp (cdr arg))) ;--- not NIL, nor a proper list 
	    )
	))
	(format nil " S-arg must be either <atom> or  '(cons <string> <atom>)'. Is ~A~%" 
      arg))))

;;;------ tested examples for use:  --> file xlispstat-s-script.lisp 
