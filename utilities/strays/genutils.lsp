( provide "genutils" )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; MACROS: MAKE-SETTER/GETTER
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 
( defmacro make-setter/getter ( proto slot )
 "Args: prototype slot 
  makes a standard setter/getter method  :slot  for 'slot of prototype."
    `( defmeth ,proto ,( keyword slot ) ( &optional ( value nil set ))
       ( when set ( setf ( slot-value ',slot ) value ))
       ( slot-value ',slot )))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; OBJECT UTILS and EXTENSIONS:  :APROPOS, :APROPOS-LIST, SENDTOALL 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 
( defmeth *object* :apropos ( str &key help )
  ( dolist 
    ( name ( send self :doc-topics ))
    ( when (string-search  str (symbol-name name)) 
           ( if help 
                ( progn ( send self :help name ) (terpri))
                (format t "~s~%" name ))))
  ( format t "~a : ~a SLOTS:~%" ( slot-value 'proto-name ) self )
  ( dolist 
    ( name ( send self :slot-names ))
    ( when (string-search str (symbol-name name))
           ( format t "~s : ~a~%" name ( slot-value name )))))
           
( defmeth *object* :apropos-list ( str ) 
  (remove 
   Nil
   ( mapcar
     #'(lambda (x) (if (string-search str (symbol-name x)) x )) 
     ( send self :doc-topics ))))


( defmeth *object* :call-setters ( pairlis )
 "Arg: pairlis   \; an assoc list of ( :keyword . value ) pairs 
  where :keyword is a setter method." 
	( dolist ( pair pairlis )
	   ( let (( meth ( keyword ( string-upcase ( car pair )))))
	   	( if ( send self :has-method meth )
	   			( send self meth ( cdr pair ))))))

 
 ( defun sendtoall ( sequ msg &rest args )
  "Args: sequence message &rest args 
   sends :message args ... to each object in sequence." 
   ( when ( not ( null sequ ))
  	( map-elements #'( lambda (x) ( apply #'send x msg args )) sequ )))
         


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; SYMBOLS & STRINGS:  KEYWORD, EXTERNAL-SYMBOLS, split-string
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


( defun keyword ( &rest names )
 "Args: &rest names 
   strings to keyword symbols."
  ( intern ( apply #'concatenate 'string ( mapcar #'string names )) 'keyword ))

( defun external-symbols ( package )
  "Arg: package
    return all of the external symbols in a package."
  ( let (( symbols () ))
    ( do-external-symbols ( name package symbols )
                          ( push name symbols ))))


( defun split-string ( str &optional ( chr  #\Space ))
 "Args: string &optional ( char #\Space )
 Split string into a list of strings at separator char."
  ( let (( n ( position chr str )))
    ( if n 
         ( cons ( subseq str 0 n ) ( split-string ( subseq str (1+ n)) chr ))
         ( list str ))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  LIST FUNCTIONS:  INTERSECT, UNION, LIST-PRODUCT, shuffle
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
           
( defun intersect ( &rest args )
	"Args: (&rest lists)
	 intersection of an arbitrary number of lists -
	 [does not preserve any list order]."  
	( let (( result ( car args )))
		( dolist ( x ( cdr args ))
			( setf result ( intersection result x )))
		result ))

( defun unions ( &rest args )
   "Union of arbitrary number of lists. [Does not preserve any list order]"
        ( let (( result ( car args )))
          ( dolist ( x ( cdr args ))
                   ( setf result ( nunion result x )))
          result ))

( defun list-product ( alist blist )
	"Args: ( alist blist )
 	Returns all possible pair-lists of respective elements of alist and blist."
  ( apply #'append 
  		( mapcar #'(lambda (a) (mapcar #'(lambda (b) (list a b))  blist )) 
  			      alist )))


( defmacro shuffle (s)
  "Destructively randomly reorder sequence s"
  `( setq ,s (sample  ( reverse ,s ) (length ,s))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; MATH:  RECIP, EXP-1, GAUSS, SQUARE, MEAN-DIFFERENCE 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 

( defun recip ( x )
 " ( RECIP number ) 
 ( RECIP number-sequence )
    1/X guarded for divide by zero (which yields POSITIVE-INFINITY)"
  ( if (sequencep x) ( map 'vector #'recip x )  
  ( if ( zerop x ) positive-infinity ( / x ))))

( defun exp-1 ( x ) (recip (exp x)))


( defun gauss (x &optional (sigma 1) (mean 0))
  "Args: x &optional  (sigma 1) (mean 0) 
     returns gaussian probability function. 
     without the optional mean and sigma args, this is the same as NORMAL-DENS"
  ( / ( exp ( * -0.5 ( square ( / ( - x mean ) sigma )))) 
      ( * (sqrt (* 2 pi)) sigma )))

;; Terminology note:       
;;  ... and CAUCHY-DENS  is the same as LORENTZIAN distribution 

( defun square (x) (* x x))

( defun mean-difference (s) 
  ( mean ( difference s)))


( defun wmean ( samples &optional stddev )
  "Args: ( samples stddev )  Or:  ( list-of-sample-stddev-pairs ) 
    returns weighted means of sample. "
  ( when ( and ( null stddev ) ( equal 2 ( length (elt samples 0))))
         ( setf stddev ( map 'vector #'(lambda (x) (elt x 1)) samples ))
         ( setf samples ( map 'vector #'(lambda (x) (elt x 0)) samples )))  
    ( let (( var ( recip ( * stddev stddev )))) 
      ( / ( sum (* var samples)) ( sum var ))))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; FILE INPUT FUNCTIONS... 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; READ-DATA-FILE, READ-ROWS-FROM-FILE,  FIND-FILE, READ-HEADER-LINES 

;;; version of read-data-file that reads "#" as a comment line indicator char.
;;; gnuplot, python and lots of other unix programs ( including many of mine )
;;; have followed the unix shell convention of using '#' as a comment line
;;; indicator. This version will read those files in properly, but will NOT
;;; read in lisp objects like vectors and chars that use "#" in their representation.
;;; ( EMSA/MSA File Format data files also use the convention of 
;;; 	"#Keyword: value"  header lines before the numerical data. 
;;;   #'read-header-lines (below) will read those lines into a alist 
;;;  ( a list of ( "keyword" . "value" ) dotted pairs. ))

;;;  Comma is read as whitespace to allow comma delimited spreadsheet files to be read.
;;;  ( Which also means that macro's won't read. ) 

;;; So far, none of these mods have produced any problems for me, as read-data-file
;;; is typically only used to read DATA files, not lisp sources. ( I haven't run into any
;;; data files with macro definitions or object literals. vectors and char codes are more 
;;; likely. )

;;; everything except the readtable modifications was copied from the 
;;; standard xlispstat read-data-file function. It follows that implementation
;;; too closely: if the function didn't mess with the *readtable*, then it could
;;; be modified from the "outside" -  readtable could be modified before calling
;;; the function. 


(defun read-data-file (&optional (file (open-file-dialog)))
"Args:  (&optional file)
Returns a list of all lisp objects in FILE. FILE can be a string 
or a symbol,in which case the symbol'f print name is used. (If no 
file arg is given, 'open-file-dialog' is called to get a filename.)
This function differs from standard read-data-file in that #\# is a 
comment character. This function can be used to read lists of numbers
from various unix programs that use the # comment convention.
It will, of course, NOT read lisp vectors, characters or other objects
that use the # char, and is intended only for reading DATA files."
  (if file
      (let ((oldtable *readtable*)
            (oldbreak *breakenable*)
            (eof (gensym)))
        (setq *readtable* ( copy-seq *xlisptable*))
        (setq *breakenable* nil)
        ;; copy the readtable entry for lisp-comment char ';'  to '#'
        (setf (elt *readtable* ( char-code #\# ))  
              (elt *readtable* ( char-code #\;
                                           ))) ; These parends NEED to be on next line!
        ;; There is a bug in the xlisp reader that causes it to ignore the rest of the
        ;; line after the semicolon even when it's used as a char constant as above.
        (setf (elt *readtable* (char-code #\, )) :WHITE-SPACE )
                          
        (with-open-file (f file)
          (if f
              (unwind-protect
               (do* ((r (read f nil eof) (read f nil eof))
                     (x (list nil))
                     (tail x (cdr tail)))
                    ((eq r eof) (cdr x))
                    (setf (cdr tail) (list r)))
               (setq *breakenable* oldbreak)
               (setq *readtable* oldtable)))))))




( defun read-rows-from-file (file) 
  "( read-rows-from-file file-name ) reads in a file turning each line into
   a lisp list, and returns the list of lists. "
( with-open-file ( ffile file )
    ( do  (( line ( read-line ffile ) ( read-line ffile Nil))
           ( data ()))
          ( ( null line ) ( reverse data ) )
          ( if ( not ( zerop ( length ( string-trim " \t" line ))))
               ( setf data 
                      ( cons  
                               ( with-input-from-string ( str line )
                                  ( let (( error-token ( gensym "eof" )))
                                    ( do (( x (read str Nil error-token)
                                              (read str Nil error-token))
                                          ( l () ))
                                         ( ( eq x error-token ) (reverse l) )
                                         ( setf l ( cons x l )))))
                        data ))))))


( defun read-rows-from-file-special (file)
 "Modified version of read-rows-from-file, set to read colon's as whitespace/separators.
  You probably don't need this, but it's included until I replace it with more general code." 
    ( let (( oldtable *readtable* ) ( result nil ))
      (setq *readtable* ( copy-seq *xlisptable*))
      (setf (elt *readtable* ( char-code #\' ))  
               :CONSTITUENT )
      (setf (elt *readtable* ( char-code #\: ))  
               :WHITE-SPACE )
      ( unwind-protect 
        (setf result ( read-rows-from-file file )) 
        ( setf *readtable* oldtable ))
      result ))


( defun find-file ( name  &key verbose  directory )
 "Args: filename &key verbose  directory 
  Searches  directories, *load-pathname*, current-working-directory 
  and *load-pathname-defaults*  for filename. Returns truname or Nil."
  ( dolist ( pname (append  
  				( if ( or ( null directory ) ( consp directory )) directory ( list directory ))
                    		(list		*load-pathname*           (get-working-directory))
                      		*load-pathname-defaults* ))
           ( let (( path ( merge-pathnames name pname )))
             ( when verbose ( format T  "Trying:  ~s ...~%"  path ))
             ( if ( probe-file path ) ( return (truename path))))))





(  defun  read-header-lines ( &optional ( filename (open-file-dialog))
							    ( c-char  #\# ) ( split-char #\: ))
"Args: &optional ( filename (open-file-dialog)) ( c-char #\# ) ( split-char #\:  )
	Reads filename and builds as Assoc list of ( keyword . value ) pairs out
	of all lines starting with c-char, where the keyword, value strings are split
	by split-char. The defaults are set for lines of the form: \'#keyword  :  value\'
	Values part of alist is evaluated as a number if it's numeric, Nil if the empty 
	string, otherwise the raw unevaluated string." 
	(when filename 						   
		( with-open-file  (  file filename )
			( when file 
				( do (( line ( read-line file  nil ) ( read-line file  nil ))
					 ( hdrs nil ))
					 (( null line ) hdrs )
					( when 
						( and 
							( not ( zerop ( length line )))
							( or ( null c-char ) ( char= ( elt line 0 ) c-char )))
						( let (( pair ( split-string  line  split-char )))
							( setf hdrs ( pairlis 
										( list ( string-trim  " \t"  ( subseq ( car pair ) 1 )))
										( list  ( let*  	(( stmp 	( string-trim  " \t"  
											 					 ( apply #'concatenate 'string ( cdr pair ))))
											 	    			 ( ntmp ( read-from-string stmp Nil Nil )))
															( if ( and ( numberp ntmp )
																	( every 
																		#'(lambda (c) (find c "0123456789.+-Ee" ))
																			stmp ))
																	ntmp 
											 	  				  ( if ( null ntmp ) ntmp  stmp ))))
										 hdrs )))))))))





;;; Window Placement methods 

( require "winplace" )


;;; shortcuts ...

( setf ( symbol-function 'uniq ) #'remove-duplicates )
( setf ( symbol-function 'i ) #'identity )


( defun cwd ( &optional dir )
  ( when dir 
         ( set-working-directory dir ))
         ( get-working-directory ))


#+macintosh
( defun updir ( ) ( cwd "::" ))
#-macintosh
( defun updir () ( cwd  ".." ))

( defun dir () ( format T "; ~A : ~%" (cwd)) ( directory "*" :all T ))
( defun files () ( format T "; ~A : ~%" (cwd)) ( directory "*" ))
( defun folders ( &optional ( path "*" )) 
	( let (( all ( directory path :all T ))
			 ( files (directory path )))
			 ( dolist ( file files )
			 	( setf all ( remove file all :test #'string= )))
			 all ))
			 


