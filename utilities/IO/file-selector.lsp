;; This file contains some helpful functions for loading files in the
;; X11 version of XLispStat. 
;;
;;     !!               ONLY USEFUL UNDER UNIX              !!
;;
;; It is written for XLISPSTAT 2.1 Release 3.44. Maybe it won't run under
;; older versions. 
;;
;; (file-selector-dialog ( &optional (mask "*") (path ".") 
;;                                   (xless t)  (cwd nil) )
;;
;;         Opens a modal dialog-window, where path and filename can be
;;         selected via mouseclicks. Since the file-list is constructed
;;         by using 'ls -lLd mask' for extracting files, mask can be
;;         any combination of wildcards that can be treated by 'ls -lLd'.
;;         After popping up the dialog, mask and path can also be edited
;;         interactively (Buttons 'Mask' and 'Path').
;;
;;         Clicking into the left list (the directory-list), changes
;;         the directory immediatly.
;;
;;         Clicking into the right list (the file-list) selects a file.
;;         When parameter 'cwd' is T, the left button is 'Select/cwd'.
;;         Clicking here changes the working-directory to selected
;;         one (useful for load Lisp-files that load others) and returns
;;         selected filename with whole absolute path.
;;         When parameter 'cwd' is NIL, then the left button is 'Select'.
;;         Clicking here leaves the working-directory unchanged (this is
;;         default, makes sense when e.g. loading data) and returns 
;;         filename with whole absolute path.
;;
;;         When pressing 'Cancel' the working directory is unchanged and the
;;         function returns NIL. 
;;
;;         When X11R5 program xless (ftp.x.org:/contrib/applications/xless-...)
;;         is available on your machine and 'xless' is set to T then a Button 
;;         'Preview' is available. Clicking on this button calls xless with
;;         the selected file. If you don't have xless, you may change the
;;         default setting of 'xless' to NIL.
;;
;; (load-lsp ( &optional (cwd t) )
;;
;;         Calls file-selector-dialog with mask "*.[lf]*" (so you'll find
;;         *.lsp, *.lisp, *.fsl), changes working-directory if cwd is T and
;;         loads the selected file.
;;
;; (open-file-dialog)
;;
;;         Original function from XLispStat overwritten, so that instead of a
;;         (get-string-dialog) the (file-selector-dialog) is called. Result is
;;         NIL (when cancelling) or the filename with whole absolute path.
;;         You'll have to uncomment this function definition, if you want to
;;         use this.
;;         
;; (basename path-file)  
;;
;;         extracts the pure filename from a string that contains path and
;;         filename. It is written in Lisp, doesn't use the shellcommand
;;         'basename' and doesn't check if file exists. It just cut's the 
;;         string until the last '/' is deleted ;;
;;
;;
;; Author:         Bernhard Walter 
;; First version:  21.2.1995
;; modified:       22.2.1995:  added   parameter 'xless' 
;;                             added   redirection of popen's standard 
;;                                     error output
;;                 23.2.1995:  added   parameter cwd
;;                             changed preview call, so that xless runs
;;                                     in background. (suggestion from
;;                                     F. Udina) 
;;                             added   function (load-lsp)
;;                             added   _optional_ overwriting of
;;                                     (open-file-dialog) 
;;                  2.3.1995   added   possibility to select file
;;                                     in file-list by double-click
;;
;;
;; for any ideas, bugs, etc, please contact
;;
;; walter@pollux.edv.agrar.tu-muenchen.de
;;


(defun load-lsp( &optional (cwd t) )
"Arguments: ( &optional (cwd t) )
Pops up a file-selector-dialog with file-mask *.[fl]*
When cwd is T, working-directory is changed before loading selected file,
else file is loaded without change of current working-directory."
  (let ((file (file-selector-dialog "*.[fl]*" "." t cwd)))
    (when file (load file))))

;; If you want the original (open-file-dialog) substituted by
;; (file-selector-dialog "*" "."), remove leading ';' in the next 5 lines
;
; (defun open-file-dialog()
; "Not the original one: Calls (file-selector-dialog \"*\" \".\") instead. 
; Try (help 'file-selector-dialog)" 
;   (file-selector-dialog "*" "."))

(defun basename(path-file) 
"Argument: (path-file)
Gets a file with its path and returns just the filename.
(Lisp-coded, no piping through shell-command 'basename')."
  (do ( (i (1- (length path-file))  (1- i)) 
      )
      ( (or (< i 0) (char= (char path-file i) #\/))
	(if (> i 0) 
	    (subseq path-file (1+ i))
	    path-file)
      )))

(defun file-selector-dialog( &optional (mask "*") (path ".") 
                                       (xless t) (cwd nil))
"Arguments: (&optional (mask \"*\") (path \".\") (xless t) (cwd nil))
Opens a file-selector-box under X11.
  mask  can be any wildcard combination that can be treated by 'ls -l',
        (in fact, 'ls -lLd mask' gets piped through a shell to
        get dir- and file-list.)
  path  can be any absolute or relative path.
  xless when T, Shows 'Preview'-Button. Preview uses the X11R5 program xless.
  cwd   when T, changes working-directory, else leaves it unchanged (default).
The result is the chosen filename with its whole absolute path. 
If you just need the filename, use function 'basename'."

  (let* ((true-path (truename path))
	 (true-mask mask)
	 (true-path (if (string= "." path)
		        (subseq true-path 0 (- (length true-path) 2))
		        (truename path)))
	 (dir-file  (get-file-and-dir-list mask true-path))
	)
    (if (first dir-file) ; we were allowed to change to new directory
	(let* ((dirs        (first  dir-file))
	       (files       (if (second dir-file) (second dir-file) '("")))
	       (dir-list    (send list-item-proto :new dirs
				  :location '(20 90)))
	       (d-d         (send dir-list  :slot-value 'size))
	       (file-list   (send list-item-proto :new files
				  :location '(200 90)))
	       (d-f         (send file-list :slot-value 'size))
	       (len         240)
	       (path-button (send button-item-proto :new "Path:"
				  :location '(20 10)))
	       (path-text   (send text-item-proto :new "" :text-length 25))
	       (mask-button (send button-item-proto :new "Mask:"
				  :location '(20 40)))
	       (mask-text   (send text-item-proto :new "" :text-length 20))
	       (ok          (send modal-button-proto :new 
				  (if cwd "Select/cwd" "Select") 
				  :location (list 20 (+ len 120))))
	       (preview     (send button-item-proto :new "Preview" 
				  :location (list 140 (+ len 120))))
	       (cancel      (send modal-button-proto :new "Cancel" 
				  :location (list 260 (+ len 120))))
	       (fs-box      (send modal-dialog-proto :new 
				  (list
       				   (list path-button path-text)
				   (list mask-button mask-text)
				   (list dir-list file-list) 
				   (if xless
				       (list ok preview cancel)
				       (list ok cancel))
				  )
				  :default-button ok 
				  :size (list (+ (first d-d) (first d-f) 80)
					      (+ len 160))
				  ))
	       (result-file   nil)
	       (result-dir    nil))
	  

	  (send path-text :text true-path)
	  (send mask-text :text true-mask)

	  (send path-button :slot-value 'action
		#'(lambda()
		    (let ((answer (get-string-dialog "Get new absolute Path" 
					:initial true-path))
			  (old-pwd (get-working-directory)))
		      (when answer
			    (if (set-working-directory answer)
				(block nil
				       (set-working-directory old-pwd)
				       (setf true-path "/")
				       (setf result-file nil)
				       (setf result-dir  (subseq answer 1))
				       (send fs-box :modal-dialog-return 1))
			      (message-dialog "Sorry, wrong directory"))))))

	  (send mask-button :slot-value 'action
		#'(lambda()
		    (let ((answer (get-string-dialog "Get new file mask" 
					:initial mask)))
		      (when answer
			    (setf result-file nil)
			    (setf result-dir (subseq true-path 1))
			    (setf true-path "/")
			    (setf true-mask answer)
			    (send fs-box :modal-dialog-return 1)))))


	  (send preview :slot-value 'action
		#'(lambda()
		    (when result-file
			  ;; Use this if you want to browse during xless runs:
			  (system (format nil "xless ~a/~a &" 
					  true-path result-file)))))
	                  ;; Use this if you want to wait until xless ended:
	                  ;; (let ((xless (popen (format nil "xless ~a/~a" 
	                  ;;	 		      true-path result-file))))
	                  ;;   (pclose xless)))))
	  
	  (send cancel :slot-value 'action
		#'(lambda()
		    (setf result-file nil)
		    (setf result-dir  nil)))

	  (send file-list :slot-value 'action
		#'(lambda (x)
		    (setf result-file 
			  (format nil "~a" 
				  (elt files (send file-list :selection))))
		    (setf result-dir nil)
		    (send dir-list :selection nil)
		    (when x (send fs-box :modal-dialog-return 1))
		    ))
	  
	  (send dir-list :slot-value 'action
		#'(lambda (x)
		    (setf result-dir 
			  (format nil "~a" 
				  (elt dirs (send dir-list :selection))))
		    (setf result-file nil)
		    (send file-list :selection nil)
		    (send fs-box :modal-dialog-return 1)
		    ))

	  (send  fs-box :modal-dialog)


	  (if result-file 
	      (if (string= "" result-file)
		  nil
		  (block nil
			 (when cwd (set-working-directory true-path))
			 (concatenate 'string true-path
				      (if (string= true-path "/" ) "" "/")
				      result-file)))
	    (if result-dir 
		(if (string= ".." result-dir)
		    (file-selector-dialog  
		        true-mask (dir-up true-path) xless cwd)

  		    (file-selector-dialog  
		         true-mask (concatenate 'string true-path
						(if (string= true-path "/" ) 
						    "" "/")
						result-dir)
			 xless cwd))
	      nil))
	  )
      (block nil
	     (message-dialog (format nil "Error:~%~a" (second dir-file)))
	     ;; try it again, one directory higher, at least '/' must be
	     ;; accessible ! 
	     (file-selector-dialog true-mask (dir-up true-path) xless cwd))) 
))

(defun extract-last-word(file)
    (do ( (i (1- (length file))  (1- i)) 
        )
	( (char= (char file i) #\Space) (subseq file (1+ i))
        )))

(defun dir-up(path) 
  (do ( (i (1- (length path))  (1- i)) 
      )
      ( (char= (char path i) #\/) (if (> i 0) (subseq path 0 i) "/")
      ) ))

(defun get-file-and-dir-list( &optional (mask "*") (path "."))
  (let ((file-list nil)
	(dir-list  nil) 
	(old-pwd   (get-working-directory)))
    
    (if (set-working-directory path) ; change directory
	(block nil

	   ;; get all files in 'path' according to 'mask'
           (let ((s (popen (format nil 
				   "/bin/sh -c \"'ls' -lLd ~a 2>/dev/null\" "
				   mask))))
				; list all files, dereference links, don't 
				; list directory contents
	     (loop
	      (let ( (file (read-line s nil)))
		(if file
		    (when (char= (char file 0) #\- )
			  (setf file-list 
				(cons (extract-last-word file) file-list)))
		  (return))))
	     (pclose s))

	   ;; get all directories in 'path'
	   (let ((s (popen (format nil "'ls' -lL"))))
					; list all files, dereference links
	     (loop
	      (let ( (file (read-line s nil)))
		(if file
		    (when (char= (char file 0) #\d )
			  (setf dir-list 
				(cons (extract-last-word file) dir-list)))
		  (return))))
	     (pclose s))
	   
	   (set-working-directory old-pwd) ; go back from where you started
	   
    ;; if there were results, sort them
	   (when dir-list  (setf dir-list  (sort-data dir-list )))
	   (when file-list (setf file-list (sort-data file-list)))

	   (list (cons ".." dir-list) file-list))
      (list nil (format nil "Cannot change to directory ~a" path)))))
	    

