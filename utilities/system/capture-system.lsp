(defun capture-system (str)
(system (concatenate 'string str " > /tmp/my_xlisp"))
(let (
     (result "")
     )
(with-open-file (fname "/tmp/my_xlisp" :direction :input)
(loop 
(let (
     (line (read-line fname nil))
     )
(if line 
        (setf result (concatenate 'string result line (string #\newline)))
	(return result))
)))
)) 