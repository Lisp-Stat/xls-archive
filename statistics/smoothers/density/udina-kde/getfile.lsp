;; This file contains two helpful functions for the X11 version of XLispStat:
;;
;;
;; (file-selector-dialog ( &optional (mask "*") (path ".") (xless t))
;;
;;         Opens a modal dialog-window, where path and filename can be
;;         selected via mouseclicks. Since the file-list is constructed
;;         by using 'ls -lLd mask' for extracting files, mask can be
;;         any combination of wildcards, that can be treated by 'ls -lLd'.
;;         After popping up the dialog, mask and path can also be edited
;;         interactively (Buttons 'Mask' and 'Path').
;;         Clicking in the directory-list, changes directly the directory
;;
;;         When X11R5 program xless (ftp.x.org:/contrib/applications/xless-...)
;;         is available on your machine and :xless is set to T then a Button 
;;         'Preview' is available. Clicking on this button calls xless with
;;         the selected file. If you don't have xless, you may change the
;;         default setting of :xless to NIL.
;;
;;         When pressing 'Cancel' the function returns NIL, Clicking 'Select'
;;         it returns the selected filename with the whole absolute path.
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
;; modified:       22.2.1995:  added :xless key
;;                             added redirection of popen's standard error output
;;
;;
;; for any ideas, bugs, etc, please contact
;;
;; walter@pollux.edv.agrar.tu-muenchen.de
;;

(unless (member :unix *features*)
	(error "getfile.lsp doesn't work outside unix world"))

(defun basename(path-file) 
"Argument: (path-file)
Gets a file with a pathname, returns just the filename.
(Lisp-coded, not piping through shell-command 'basename')."
  (do ( (i (1- (length path-file))  (1- i)) 
      )
      ( (or (< i 0) (char= (char path-file i) #\/))
	(if (> i 0) 
	    (subseq path-file (1+ i))
	    path-file)
      )))

(defun file-selector-dialog( &optional (mask "*") (path ".") (xless t))
"Arguments: (&optional (mask \"*\") (path \".\") )
Opens a file-selector-box under X11.
  mask  can be any wildcard combination that can be treated by 'ls -lLd ',
        (in fact, mask gets 'ls -lLd mask' gets piped through a shell to
        get dir- and file-list.)
  path  can be any absolut or relative path.
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
				  :location '(200 90) ))
	       (d-f         (send file-list :slot-value 'size))
	       (len         240)
	       (path-button (send button-item-proto :new "Path:"
				  :location '(20 10)))
	       (path-text   (send text-item-proto :new "" :text-length 25))
	       (mask-button (send button-item-proto :new "Mask:"
				  :location '(20 40)))
	       (mask-text   (send text-item-proto :new "" :text-length 20))
	       (ok          (send modal-button-proto :new "Select" 
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
			  (let ((xless (popen (format nil "xless ~a/~a" 
						      true-path result-file))))
			    (pclose xless)))))
	 
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
		    ))
	  
	  (send dir-list :slot-value 'action
		#'(lambda (x)
		    (setf result-dir (format nil "~a" 
					     (elt dirs (send dir-list :selection))))
		    (setf result-file nil)
		    (send file-list :selection nil)
		    (send fs-box :modal-dialog-return 1)
		    ))

	  (send  fs-box :modal-dialog)


	  (if result-file 
	      (if (string= "" result-file)
		  nil
		(concatenate 'string true-path
			     (if (string= true-path "/" ) "" "/")
			     result-file))
	    (if result-dir 
		(if (string= ".." result-dir)
		    (file-selector-dialog  
		        true-mask (dir-up true-path) xless)

  		    (file-selector-dialog  
		         true-mask (concatenate 'string true-path
						(if (string= true-path "/" ) "" "/")
						result-dir)
			 xless))
	      nil))
	  )
      (block nil
	     (message-dialog (format nil "Error:~%~a" (second dir-file)))
	     ;; try it again, one directory higher, at least '/' must be
	     ;; accessible ! 
	     (file-selector-dialog true-mask (dir-up true-path) xless))) 
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
	    
(provide "getfile")
