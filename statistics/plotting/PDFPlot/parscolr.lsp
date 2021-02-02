;; X11 xlispstat has a PARSE-COLOR function to return the rgb values of a 
;; color symbol. This is an implementation for Mac (should also work on
;; MS-Windows) which requires an X11 style rgb.txt file, with each line
;; containing:  r g b  colorname. 
;;
;;
;;  The short instructions:
;;     (load "parscolr")  ;; load this file
;;     (load-colortable)  ;; you need rgb.txt 
;;	   (unintern 'load-colortable)		;; optional -- you no longer need this
;;	   (unintern 'parse-rgb.txt)		;; optional -- you no longer need this
;;     (save-workspace "filename")		;; XLS exits on saving workspace
;; 
;;  *colortable*, parse-color, new make-color, etc. are now available 
;;   in new workspace.
;;
;;
;;  (steve majewski) <sdm7g@Virginia.EDU>
;;
;; 
;; Changes from COLORS.LSP:
;;
;; The earlier version of this module contained some other experimental
;; color management routines. These have been removed, and except for
;; ADD-COLOR, there are all non-unix support routines. 
;; 
;; No longer uses it's own package -- that was more trouble than it
;; was worth, and the problems with redefining USER:MAKE-COLOR is
;; solved my un-interning "XLISP:MAKE-COLOR.
;;
;; Only the hash-table is assigned (to *colortable*, which previously
;;  was a list, hash-table was rgb-table )
;; 
;; The file handling has been changed to make it easier to save a workspace
;;  with the *colortable* assigned, thus it no longer writes out a rgb.lsp
;;  file, and there is no automatic loading of rgb.{txt|lsp} on loading
;;  this file -- 
;;
;;
;;  Steven D. Majewski <sdm7g@Virginia.EDU> 
;; 


(defun canonicalize-name (name)
 "Coerce name to a symbol -- if it's a string, spaces and tabs are
  removed and it is converted to all upper case before interning symbol."
; ( because rgb.txt colors are often duplicated as, for example:
;   "SkyBlue" and "sky blue" -- we coerce both to symbol 'SKYBLUE )
	(if (stringp name)
		(intern 
			(nstring-upcase (remove #\Space ( remove #\Tab name )))
			"USER" )
		name))

(defun list2table (list)
  (let ((table (make-hash-table)))
    (dolist (x list)
             (setf (gethash (car x) table) (cdr x)))
    table))

(defun table2list (tab)
	(let ((list ()))
		(maphash #'(lambda (k v) (push (cons k v) list)) tab)
		list ))
		

( defun parse-rgb.txt (&optional (path "rgb.txt"))
  (setf path (probe-file path))  
  (when path
  	(with-open-file (strm path)
         (do ((line (read-line strm nil) (read-line strm nil))
              (retlst () ))
             ((null line) (remove-duplicates retlst :test #'equal))
	(when line 
     	(push 
      		(with-input-from-string ( stream line )
        		(let ((red 		(/ (read stream) 255.0))
              		  (green  	(/ (read stream) 255.0))
              		  (blue 	(/ (read stream) 255.0)))
          			(list 
          				(canonicalize-name (read-line stream))
           					red green blue )))
			retlst))))))


(defun parse-color (color)
	(gethash (if (stringp color) (canonicalize-name color) color)
		 *colortable*))


;; Since PARSE-COLOR seems to be the only way to get a color's RGB values, 
;; we need to redefine MAKE-COLOR to add new colors to the rgb hash table:

( unexport 'make-color "XLISP" )

( defun MAKE-COLOR ( name r g b )
  (let ((name (canonicalize-name name)))
  	(xlisp::make-color  name r g b )
  	(setf (gethash name *colortable*) (list r g b))
  name))

;; NOTE: the functions that use the redefined MAKE-COLOR need  
;; to be defined AFTER the unintern and redefinition.

( defun ADD-COLOR (color)
	(let*	((color (canonicalize-name color))
		 	 (rgb (parse-color color)))
    	(apply #'make-color color rgb)))


( defun load-colortable ( &optional (path "rgb.txt"))
	(setf path (probe-file path))
	(when (null path)
   	   (message-dialog "Where is rgb.txt?" )
   	   (setf path ( open-file-dialog ))
   	   (if (null path) (return) (setf path (probe-file path))))
	(when path
		(setf *colortable* 
			(list2table (parse-rgb.txt path)))
		#+macintosh ( progn 
;; macintosh built-in 'GREEN isn't a pure green
;; we can't redefine standard colors, so we patch the value
;; in the table, so we get the correct Mac value from parse-color,
;; and create a real GREEN green ( named GREENGREEN ) 
  			(setf (gethash 'GREEN *colortable*) (LIST 0.0 0.6 0.0))
  			(make-color 'GREENGREEN  0.0  1.0  0.0))
  	*colortable* ))


( provide "parscolr" )	
