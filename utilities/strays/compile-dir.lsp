;;; this is file "compile-dir.lsp"
;;; functions to automate file compilation
;;; f. udina mar 1995


;;; compile-dir must be run in an environment understanding 
;;; things like that
;;; (system "test kde.fsl -ot kde.lsp") or  "test -e kde.lsp"

(unless (fboundp 'get-file-and-dir-list)
	(load "file-selector-dialog"))

;;; file-selector-dialog.lsp file is from 
;; Author:         Bernhard Walter 
;; First version:  21.2.1995
;; modified:       22.2.1995:  added :xless key
;;
;; for any ideas, bugs, etc, please contact
;;
;; walter@pollux.edv.agrar.tu-muenchen.de
;;

(defun system-as-boolean (command)
  (= 0 (system command)))

(defun need-compile (basefilename)
"checks if .lsp is newer than .fsl and compiles if needed"
(let ((file-exists (system-as-boolean
		    (format nil "test -e ~a.lsp" basefilename)))
      need)
  (when file-exists
	(setf need (system-as-boolean 
			    (format nil "test ~a.fsl -ot ~a.lsp"
				    basefilename basefilename)))
	need)))

(defun compile-dir (&key (dirpath (get-working-directory))
			 (confirm nil)
			 (force nil)
			 (load nil)
			 (print *compile-print*)
			 (verbose *compile-verbose*))
"This function will help in automatic recompilation of the .lsp files
 in a directory.
 KEY arguments: (dirpath (get-working-directory))
		(confirm nil)
		(force nil)
		(load nil)
		(print *compile-print*)
		(verbose *compile-verbose*)
 If FORCE is T, all the files will be compiled
    if not, only files with no .fsl
                       or with .lsp newer than .fsl will be compiled.
 If CONFIRM is T, before compiling any file
                  a dialog will ask for confirmation.
 LOAD, PRINT and VERBOSE keys are passed to the 'compile-file call."
(let ((files (second (get-file-and-dir-list "*.lsp" dirpath)))
      fullname must)
  (labels ((trim-last-four (string)
			   (subseq string 0 (- (length string) 4)))
	   (do-it (fil)
		  (setq fullname (concatenate 'string 
					      dirpath "/" fil))
		  (if confirm
		      (setq must (ok-or-cancel-dialog
				  (format nil "Compile ~%~a.lsp ?"
					  fullname)))
		    (setq must t))
		  (when (and must (or force (need-compile fullname)))
			(format t ";;;~%~a.lsp will be compiled" fullname)
			(compile-file fullname :load load 
				      :verbose verbose :print print)
			)))
	  (if (> (length files) 0)
	      (progn (setq files (mapcar #'trim-last-four files))
		     (mapcar #'do-it files)
		     t)))))


