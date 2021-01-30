;;;
;;; file: EDIT.LSP
;;;

;;;(setq *breakenable* t)
;;;(setq *tracenable* t)

;;;
;;; This variable is the default file to edit
;;;

(defvar edit-file 'nil)

(defvar editor "emacs")

;;;
;;; edit a file using the specified editor
;;; if the file editted was a lisp file (.lsp) load it
;;;

(defun edit (&optional file)
    (read-char) ; get rid of newline sitting in input stream
    (and (not file)
         (null edit-file)
         (setq file (get-filename)))
    (if (not (stringp file))
	(setq file edit-file))
    (if (not (stringp editor))
	(setq editor (get-editor)))
    (system (concatenate  'string editor " " file ))
    (setq edit-file file)
    (let ((len (length edit-file)))
	(if (string= (string-downcase (subseq edit-file (- len 4))) ".lsp")
	    (load edit-file))))

(defun get-filename ()
	(princ "File: ")
	(read-line))

(defun get-editor ()
	(princ "Editor: ")
	(read-line))

