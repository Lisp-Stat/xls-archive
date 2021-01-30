(defun ls2html ()
  (let ((lines nil)
        (dname nil))
    (with-open-stream (instream (popen "pwd"))
                      (setf dname
                            (strip-left-from-string "code"
                                                    (read-line instream))))
    (with-open-stream (instream (popen "ls -1F"))
                  (loop
                   (if (peek-char nil instream)
                       (setf lines (append lines (list (read-line instream))))
                     (return))))
    (with-open-file (outstream "index.html"
                    :direction :output)
                    (format outstream "<HTML>~%")
                    (format outstream "<HEAD>~%")
                    (format outstream "<TITLE>~a</TITLE>~%" dname)
                    (format outstream 
                            "<LINK REV=\"OWNER\" HREF=\"mailto:deleeuw@stat.ucla.edu\">~%")
                    (format outstream "</HEAD>~%")
                    (format outstream "<BODY>~%")
                    (format outstream "<H1>UCLA Xlisp-Stat Archive</H1>~%")
                    (format outstream "<P>~%<HR>~%<P>~%")
                    (format outstream "<H2>~a</H2>~%<P>~%" dname)
                    (format outstream "<H3>Files</H3>~%")
                    (format outstream "<DL>~%")
                    (dolist (str lines)
                        (format outstream "<DT>~%")
                        (cond
                         ((char-equal #\/ (last-char str))
                            (format outstream
                                    "<A HREF=\"~a\">~a</A>~%" str
                                    (but-last-char str)))
                         ((char-equal #\~ (last-char str)))
                         ((char-equal #\# (last-char str)))
                         (t (format outstream
                                  "~a~%" str)))
                      (format outstream "</DT>~%")
                      (format outstream "<DD>Text goes here</DD>~%"))
                    (format outstream "</DL>~%") 
                    (format outstream "<P>~%<P>~%<HR>~%")
                    (format outstream "<ADDRESS>Jan de Leeuw<BR>~%UCLA Statistics Program<BR>~%deleeuw@stat.ucla.edu~%</ADDRESS>~%")
                    (format outstream "</BODY>~%")
                    (format outstream "</HTML>~%"))
    
    )
  )

(defun last-char (str)
  (elt str (1- (length str))))

(defun but-last-char (str)
  (select str (iseq (1- (length str)))))

(defun strip-left-from-string (str1 str2)
 "Args: str1 str2
Looks for str1 in str2, then removes str1 and
everything preceding str1 from str2."
 (let ((indx (search str1 str2))
       (nlgt (length str1)))
   (if indx
       (subseq str2 (+ indx nlgt)) str2))
 )