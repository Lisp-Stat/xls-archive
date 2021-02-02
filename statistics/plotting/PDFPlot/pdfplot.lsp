
( require "basicplt" )	;; builds on BASIC-PLOT-PROTO
( provide "pdfplot" )


;; PDF-PLOT redefines the graphics primitives called by a :REDRAW message
;; to produce a PDF output stream to a file. 
;; With a plot based on BASIC-PLOT-PROTO :
;;		( write-pdf <basic-plot-proto-object> "filename.PDF" ) 
;; and try opening and printing with Adobe Reader.
;;
;;   Steven D. Majewski/University of Virginia	<sdm7g@Virginia.EDU>
;; 
;; 
;; As far as I can tell, this program produces a properly formatted PDF file
;; according to the 1.2 Spec. including a proper xref table, stream length,
;; and other offsets. I have changed the stream length into a object 
;; reference, so that the length is not needed before the page description
;; is generated, however, it still generates the commands into a 
;; string-stream, so it is limited in the size and complexity of page
;; description it can generate. This will be changed in the next version. 
;; 

;; NOTE: TO FIX:
;; There may still be some inconsistant use of :SIZE, :CLIP-RECT, :FRAME-RECT, etc.
;;  to get the plot region dimensions.


#-unix ( require "parscolr" )   	;; or at least PARSE-COLOR 
;;   parse-color is built-in on X11 but not on Mac 
;;   ( I don't know about MS-Windows ) 
;;  It's needed for setting writing out the RGB color values in PDF, 
;;  even if you're only writing black and white. If those are the
;;  only colors you use, you could use this as a minimal 
;;  Black&White (&one shade of gray) replacement:

#-unix
  (when (not (fboundp 'parse-color))
   ( defun PARSE-COLOR ( symb )
   	( case symb  
   		( BLACK  ( list 0 0 0 ))
   		( WHITE  ( list 1 1 1 ))
		( T      ( list 0.5 0.5 0.5 )))))



( defproto pdf-plot '( output ) () basic-plot-proto )


;; NOTE about coordinate space
;; The PDF coordinate space vertical axis is opposite to the 
;;  xlisp canvas coordinates. 
;; This first implementation changes the PDF coordinate space -- 
;;  which sadly turns out to be the wrong way to do it, as it 
;;  also inverts text strings, so the text handling has to invert it
;;  again and change the y coordinate. ( This was a result of saving
;;  text for last! <GROAN> )  If this gets a second version, I'll
;;  probably change that. 


	
( defmeth pdf-plot :line-width (&optional (val nil set))
	( when set 
		( format (slot-value 'output) " ~d w " val)
		( call-next-method val ))
	(call-next-method))


( defmeth pdf-plot :line-type (&optional (val nil set))
	(when set
		(if (eq val 'DASHED)
			(format (slot-value 'output) " [ 3 5 ] 0 d " ) 	; 'DASHED
			(format (slot-value 'output) " [ ] 0 d " )))	; else 'SOLID 
	(call-next-method))
	
( defmeth pdf-plot :draw-color (&optional (val nil set))
	( when (and set val)
		(call-next-method val)
		( format (slot-value 'output) "~{ ~f ~} RG ~{ ~f ~} rg "
			( parse-color val ) ( parse-color val )))
	( call-next-method ))

( defmeth pdf-plot :draw-line (x0 y0 x1 y1)
	( call-next-method x0 y0 x1 y1 )
	( format (slot-value 'output) 
		"~d ~d m ~d ~d l S ~%" x0 y0 x1 y1 ))

;; for now, all symbols are a small square... 
 ( defmeth pdf-plot :draw-symbol (symb hi x y)  
	( call-next-method symb hi x y )
	( format (slot-value 'output)
		" ~d ~d m ~d ~d 3 3 re ~A ~%" x y x y ( if hi "f" "S" )))


( defmeth pdf-plot :draw-string ( str x y )
	(call-next-method str x y )
	(format (slot-value 'output) "BT 1 0 0 -1 0 0 Tm ~d ~d Td (~A) Tj ET~%" 
			 x (- y) str ))

( defmeth pdf-plot :draw-string-up ( str x y )
	(call-next-method str x y)
	(format (slot-value 'output)
	 "BT 0 -1 -1 0 0 0 Tm ~d ~d Td (~A) Tj ET~%" 
	 (- y) (- x) str ))


( defmeth pdf-plot :draw-text ( str x y h v )
	(if ( = h 1) 
		(setf x (- x (floor ( / ( send self :text-width str) 2))))
		(if ( = h 2)
			(setf x (- x (send self :text-width str)))))
	(if ( = v 1) 
		(setf y (+ y  (send self :text-ascent))))
	(send self :draw-string str x y ))

( defmeth pdf-plot :draw-text-up ( str x y h v )
	(if (= h 1)
		(setf y (+ y (floor 
			(/ (send self :text-width str) 2 )))))
	( setf x ( + x  (send self :text-ascent)))
	(send self :draw-string-up str x y ))


( defmeth pdf-plot :paint-rect ( x0 y0 x1 y1 )
	(call-next-method x0 y0 x1 y1)
	(format (slot-value 'output)
		"\n ~d ~d ~d ~d re f\n" x0 y0 x1 y1 ))
		
( defmeth pdf-plot :make-poly ( points  &optional (from-origin t))
	( let (( output (slot-value 'output)))
			(format output " n ~d ~d m " (first (car points)) (second (car points)))
		( dolist ( pt points )
			(format output " ~d ~d l " (first pt) (second pt)))))
	
( defmeth pdf-plot :frame-poly ( points &optional (from-origin t))
	(call-next-method points from-origin) 
	(send self :make-poly points from-origin)
	(format (slot-value 'output) " S ~%" ))

( defmeth pdf-plot :paint-poly ( points &optional (from-origin t))
	(call-next-method points from-origin)
	(send self :make-poly points from-origin)
	(format (slot-value 'output) " f ~%" ))	

;; the following are just to add some tracing and debugging
;; info to the PDF output file: 

( defmeth pdf-plot :draw-data-lines ( a b c d )
	( format (slot-value 'output) "\n%% :DRAW-DATA-LINES\n" )
	(call-next-method a b c d))

( defmeth pdf-plot :draw-data-points ( a b c d )
	( format (slot-value 'output) "\n%% :DRAW-DATA-POINTS\n" )
	(call-next-method a b c d))
	
( defmeth pdf-plot :redraw-background ()
	( format (slot-value 'output) "\n%% :REDRAW-BACKGROUND\n" )
	(call-next-method))

( defmeth pdf-plot :redraw-axis ()
	( format (slot-value 'output) "\n%% :REDRAW-AXIS\n" )
	(call-next-method))
	
( defmeth pdf-plot :redraw-content ()
	( format (slot-value 'output) "\n%% :REDRAW-CONTENT\n" )
	(call-next-method))
	



;; Format template for PDF prelude containing header and 
;; initial object definitions that precede the page description.
;; Values for MediaBox need to be supplied:
;;   /MediaBox [ ~d ~d ~d ~d ]  
;; 

( setf pdf-header  
"%PDF-1.0
% Graphics output produced by xlispstat & pdfplot.lsp
" )

( setf pdf-objects 
  (list
"1 0 obj
<<
/Type /Catalog
/Pages 3 0 R
/Outlines 2 0 R
>>
endobj
"

"2 0 obj
<<
/Type /Outlines
/Count 0
>>
endobj
"

"3 0 obj
<<
/Type /Pages
/Count 1
/Kids [ 7 0 R ]
>>
endobj
"

"4 0 obj
[
/PDF /Text
]
endobj
"

"5 0 obj
<< 
/Type /Font 
/Subtype /Type1
/Name /F1
/BaseFont /Helvetica
/Encoding /MacRomanEncoding
>>
endobj
"

"6 0 obj
<< 
/Type /Font 
/Subtype /Type1
/Name /F2
/BaseFont /ZapfDingbats
/Encoding /MacRomanEncoding
>>
endobj
"

"7 0 obj
<<
/Type /Page
/Parent 3 0 R
/Resources << /Font << /F1 5 0 R   /F2 6 0 R  >>   /ProcSet 4 0 R >>
/MediaBox [ ~d ~d ~d ~d ]
/Contents 8 0 R
>>
endobj
" ))

;; PDF page description template 
;; page description needs to be preceeded by /Length of stream.
;; [Changed to an object reference so length can follow stream.]

( setf pdf-stream ( list 
 
"8 0 obj
<< /Length 9 0 R >>
stream
%
BT
/F1 10 Tf
ET
%
% Begin Lisp graphics output
%
~A
%
% End Lisp graphics output 
%
endstream
endobj
"


"9 0 obj
 ~d 
endobj
"
 ))


;; The trailing part, including the cross reference and the PDF trailer
;; is split in two as we need to get the byte position of the start of
;; the xref table to insert near the end of the trailer. (startxref).
;; The length of all of the preceeing output is supplied for that
;; second trailer template.

;; NOTE: Currently, if you add more objects to the file, you need to 
;; change this table. Eventually, all of this will be computed dynamically. 


( setf pdf-trailer-template
"xref
0 10
0000000000 65535 f
~10,48d 00000 n
~10,48d 00000 n
~10,48d 00000 n
~10,48d 00000 n
~10,48d 00000 n
~10,48d 00000 n
~10,48d 00000 n
~10,48d 00000 n
~10,48d 00000 n
trailer
<<
/Size 11
/Root 1 0 R
>>
startxref
~d
%%EOF
" )


(defun write-pdf (graph filename)
	(with-open-file (file filename :direction :output)
		(let ((xrefs ())
			  (size (cddr (send graph :view-rect))))
			(format file "~A" pdf-header)
			(push (file-length file) xrefs)
			(dolist (obj (butlast pdf-objects))
				(format file "~A" obj)
				(push (file-length file) xrefs))
			(format file (car (last pdf-objects)) 0 0 (first size) (second size))
			(push (file-length file) xrefs)
			(let ((contents (make-pdf-string graph)))
				(format file (car pdf-stream) contents)
				(push (file-length file) xrefs)
				(format file (cadr pdf-stream) (length contents))
				(push (file-length file) xrefs))
			(apply #'format file pdf-trailer-template
				(reverse xrefs)))))

;; NOTE: I had thought that I could :RETYPE the graphics object to include
;; the pdf plot methods, however there is a problem with retyping anything
;; with HARDWARE-OBJECTs -- they will be released and reallocated. 
;; Instead, the pdf plot methods are temporarily added to the object. 
;; This assumes that the instance object does not itself own any methods 
;; of the same name -- they should all be owned by the prototype, so that
;; adding methods which do a (CALL-NEXT-METHOD) will work and that removing
;; the temporarily installed methods will revert back to original behavior. 


;; Although the following method has been modified to take a stream arg for
;; output sink, rather than returning a string, the function that calls it
;; above still embeds a string with a format statement, so plots are limited
;; to output that will fit into memory. This will be changed. 


( defun make-pdf-string ( plot &optional (output nil output-assigned))
	( dolist ( meth ( send pdf-plot :own-methods ))
		( send plot :add-method meth ( send pdf-plot :get-method meth )))
	(if (null output-assigned) (setf output (make-string-output-stream)))
	( send plot :add-slot 'output output )
	( format output "1 0 0 -1 0 ~d cm ~%" ( second ( send plot :size )))
	( send plot :redraw )
	( dolist ( meth ( send pdf-plot :own-methods ))
		( send plot :delete-method meth ))
	( format output "%\n%\n BT 1 0 0 -1 0 ~d Tm 10 ~d Td (~A ~A) Tj ET ~%" 
		( second ( send plot :size ))
		( - ( second (send plot :size)) 10 )
			(send plot :title)
			"produced by XLS pdfplot.lsp" )
	(if (null output-assigned) (get-output-stream-string output)
								output ))
			