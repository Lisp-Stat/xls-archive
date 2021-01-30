(defun read-file (fname fstring &key (peek nil))
  "Args: (string list)
Reads file STRING into a list of lists of characters.
Then reads the list of strings containing fortran
type formats, and uses these formats to write the
characters into LIST, a list of lists of data items."
  (let ((flist (handle-format-list fstring))
	(m (count-vars fstring)))
    (with-open-file (f fname)
      (do ((result nil)
	   (rec (read-line f nil) (read-line f nil))
	   (i 0 (+ i 1)))
	  ((null rec) (nreverse result))
	  (when peek (format t "~d~a" i (code-char 13)))
	  (let ((counter 0)
		(y nil))
	    (dolist (fr flist)
	      (let* ((ch (first fr))
		     (rp (second fr)))
		(case ch
		  (#\t (setf counter (- counter rp)))
		  (#\x (setf counter (+ counter rp)))
		  (t (let ((fw (third fr)))
		       (dotimes (k rp)
		         (let ((start counter)
			       (end (+ counter fw)))
			   (setf counter end)
			   (case ch
			     ((#\i #\f) (push (read-subseq rec start end) y))
			     (#\a (push (subseq rec start end) y))))))))))
	    (push (nreverse y) result))))))

(defun handle-format-list (flist)
  "Args: (flist)
FLIST is a format list, i.e. a list of strings of the
fortran format type, with f, a, i, t, and x formats allowed.
These strings are translated to short lists, which
are easier to handle."
  (mapcar #'(lambda (x)
	      (cond ((find #\f x) (f-format x))
		    ((find #\x x) (x-format x))
		    ((find #\t x) (t-format x))
		    ((find #\a x) (a-format x))
		    ((find #\i x) (i-format x))
		    (t (error "no f, x, t, a, i format"))))
	  flist))

(defun count-vars (flist)
  "Args: list
LIST is a format list, i.e. a list of strings of the
fortran format type, with f, a, i, t, and x formats allowed.
This function counts the number of variables described
by the list."
  (let ((s 0))
    (dolist (x flist s)
      (cond ((find #\f x) (setf s (+ s (elt (f-format x) 1))))
	    ((find #\a x) (setf s (+ s (elt (a-format x) 1))))
	    ((find #\i x) (setf s (+ s (elt (i-format x) 1))))))))

(defun f-format (fl)
  "Args: list
This function converts a LIST of characters, representing a
fortran-type f-format, to a string containing the
character #\f, the repeat, the width of the field, 
and the number of decimals."
  (let ((times 1)
	(decimal 0)
	(field 0)
	(lpos (length fl))
	(fpos (position #\f fl))
	(dpos (position #\. fl)))
    (if (not dpos) (error "f-format has no decimal point"))
    (if (> fpos 0) (setf times (read-subseq fl 0 fpos)))
    (setf field (read-subseq fl (+ 1 fpos) dpos))
    (setf decimal (read-subseq fl (+ 1 dpos) lpos))
    (list #\f times field decimal)))

(defun x-format (fl)
  "Args: list
This converts a LIST of characters, representing a
fortran-type x-format, to a number: the width of the field."
  (let ((lpos (length fl))
	(xpos (position #\x fl))
	(field 0))
    (if (> xpos 0) (error "x not in first place in x-format"))
    (setf field (read-subseq fl (+ 1 xpos) lpos))
    (list #\x field)))

(defun t-format (fl)
  "Args: list
This converts a LIST of characters, representing a
fortran-type t-format, to a number: the number of places to go back."
  (let ((lpos (length fl))
	(tpos (position #\t fl))
	(field 0))
    (if (> tpos 0) (error "t not in first place in t-format"))
    (setf field (read-subseq fl (+ 1 tpos) lpos))
    (list #\t field)))

(defun a-format (fl)
  "Args: list
This converts a LIST of characters, representing a
fortran-type a-format, to two numbers: the repeat and
the width of the field."
  (let ((times 1)
	(field 0)
	(lpos (length fl))
	(apos (position #\a fl)))
    (if (> apos 0) (setf times (read-subseq fl 0 apos)))
    (setf field (read-subseq fl (+ 1 apos) lpos))
    (list #\a times field)))

(defun i-format (fl)
  "Args: list
This converts a LIST of characters, representing a
fortran-type i-format, to two numbers: the repeat and
the width of the field."
  (let ((times 1)
	(field 0)
	(lpos (length fl))
	(ipos (position #\i fl)))
    (if (> ipos 0) (setf times (read-subseq fl 0 ipos)))
    (setf field (read-subseq fl (+ 1 ipos) lpos))
    (list #\i times field)))

(defun read-subseq (x start end)
"Args: list
reads a subsequence of a string as a lisp expression."
  (read-from-string x t nil :start start :end end))
