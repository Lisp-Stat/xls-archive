

I'm using the code below ( on Macintosh Xlisp 3.47 ) to add common
directories to the file menu. It works more or less as it should
 -- it adds directories to the file menu, and it sets them as
the current working directory. If I try to load a filename from
that directory, it works as it should. However, when I do an
(open-file-dialog), it doesn't  start up in the current working
directory. Any idea why it doesn't or  how it can be forced ?
The whole reason for adding these shortcuts was to avoid having
to climb in and out of directories with the open-file dialog!!


---|  Steven D. Majewski   (804-982-0831)  <sdm7g@Virginia.EDU>  |---
---|  Computer Systems Engineer          University of Virginia  |---
---|  Department of Molecular Physiology and Biological Physics  |---
---|  Box 449 Health Science Center    Charlottesville,VA 22908  |---
 [ "The grass is always greener, except at t=0" - Stan Kelly-Bootle ]

;; Adds common default directories to File menu
;;

( defproto dir-menu-item-proto () '( dir-items ) menu-item-proto )

( defmeth dir-menu-item-proto :mark ( &optional ( value nil set ))
	( when set
		( call-next-method value )
		( if value
			( mapcar #'(lambda (x) (send x :mark nil))
				( remove self ( slot-value 'dir-items)))))
	(call-next-method))

#+macintosh ( defconstant dir-sep ":" )
#-macintosh ( defconstant dir-sep "/" )

( defun short-dir-name ( path )
	( let (( dname ( pathname-directory ( pathname path ))))
		( if ( > ( length dname ) 3 )
			( concatenate 'string
					"..."
					dir-sep
					( elt dname (- (length dname) 2))
					dir-sep
					( elt dname (1- (length dname))))
			(make-pathname :directory dname))))

( defun dir-item ( dname )
	( send *FILE-MENU* :find-item ( short-dir-name dname )))


( defmeth dir-menu-item-proto :isnew ( dirname )
	( if ( dir-item dirname )
		( error "Directory: ~S already installed~%" dirname ))
	( let (( name ( short-name dirname )))
		( call-next-method name
		:action
			#'(lambda ( &rest args )
				( set-working-directory dirname )
				( send
					( dir-item dirname )
					:mark T )
				(format *standard-output*
				 "SET WORKING DIRECTORY:\n ~S ~\n\n"
					(get-working-directory)))
		:style 'italic
		)
	( send dir-menu-item-proto :slot-value 'dir-items
			( cons self (slot-value 'dir-items)))
	( send (find-menu "File") :append-items  self )
	( send self :mark Nil )))




( defun add-dir ( &optional dirname )
	( if ( null dirname )
		( setf dirname ( get-working-directory )))
	( send dir-menu-item-proto :new dirname ))


;
; WARNING -- This is hard coded to keep the original 10
; menu items -- it is mostly needed for testing and reloading
; these definitions. You can remove or replace.

( defun reset-file-menu ()
	(apply #'send *file-menu* :delete-items
		( subseq ( send *file-menu* :items ) 11 )))

( reset-file-menu )


( send *FILE-MENU* :append-items ( send dash-item-proto :new ))

( send *FILE-MENU* :append-items
		( send menu-item-proto
			:new "add directory"
			:action #'(lambda ()
				( let (( file ( set-file-dialog
						"Choose directory" )))
					( when file (add-dir))))))

( send *FILE-MENU* :append-items ( send dash-item-proto :new ))



