;	n.lsp
;	w.hatch Sun Aug 25 17:14:19 EDT 1991
;	uunet!bts!bill
;
(defun gt-select (data value)
	"----------------------------------------------------------------
	gt-select (data value) data is a list of lists; the component lists
	are assumed to be of the same length with the first list being
	time tags.  The returned object is a list of the original lists
	containing all elements having time tags greater than given value.
	-----------------------------------------------------------------"
	(let ((my-time 0) (n 0) (m 0) (value-index 0) (now-list 0) (out-list 0)
		(chop-tag 0))
		;----------------------------------------------------------
		; find the chop time tag
		;----------------------------------------------------------
		(block gt
			(setf m (length data))
			(setf my-time (car data ))
			(setf n (length my-time))
			(block find-chop-point
				(dotimes (i n)
					(setf find-index i)
					(if (greater-than (nth i my-time) value)
						(return-from find-chop-point)
					)
				)	
			) ; END find-chop-point
			;------------------------------------------------------
			; the chop tag is the tag prior to the find-index
			;------------------------------------------------------
			(if (eql find-index 0)
				(setf chop-tag 0)
				(setf chop-tag  find-index)
			)
			;------------------------------------------------------
			; chop beginning of each of the component list
			;------------------------------------------------------
			(dotimes (i m)
				(setf now-list (select data i))
				(setf now-list (chop-first now-list chop-tag))
				(if (eql i 0)
					(setf out-list now-list)
					(setf out-list (list out-list now-list))
				)
			)
			(return-from gt out-list)
		) ; END gt
	)
)
(defun lt-select (data value)
	"----------------------------------------------------------------
	lt-select (data value) data is a list of lists; the component lists
	are assumed to be of the same length with the first list being
	time tags.  The returned object is a list of the original lists
	containing all elements having time tags less than given value.
	-----------------------------------------------------------------"
	(let ((my-time 0) (n 0) (m 0) (value-index 0) (now-list 0) (out-list 0)
		(chop-tag 0))
		;----------------------------------------------------------
		; find the chop time tag
		;----------------------------------------------------------
		(block gt
			(setf m (length data))
			(setf my-time (car data ))
			(setf n (length my-time))
			(block find-chop-point
				(dotimes (i n)
					(setf find-index i)
					(if (>= (nth i my-time) value)
						(return-from find-chop-point)
					)
				)	
			) ; END find-chop-point
			;------------------------------------------------------
			; the chop tag is the tag prior to the find-index
			;------------------------------------------------------
			(if (eql find-index 0)
				(setf chop-tag 0)
				(setf chop-tag  (- n find-index))
			)
			;------------------------------------------------------
			; chop beginning of each of the component list
			;------------------------------------------------------
			(dotimes (i m)
				(setf now-list (select data i))
				(setf now-list (chop-last now-list chop-tag))
				(if (eql i 0)
					(setf out-list now-list)
					(setf out-list (list out-list now-list))
				)
			)
			(return-from gt out-list)
		) ; END gt
	)
)
(defun chop-first (x &optional ( n 1))
	"---------------------------------------------------------------
	chop-first (x &optional (n 1)) returns x with first n elements 
	removed
	----------------------------------------------------------------"
	(block chopping-first-block
		(let ( (new-list 0) )
			(setf new-list x)
			(dotimes (i n)
				(setf new-list (cdr new-list))
			)
			(return-from chopping-first-block new-list)

		)  ; END let
	) ; END chopping-first-block
)
(defun chop-last (x &optional ( n 1))
	"---------------------------------------------------------------
	chop-last (x &optional (n 1)) returns x with last n elements 
	removed
	----------------------------------------------------------------"
	(block chopping-last-block
		(let ( (new-list 0) )
			(setf new-list (reverse x))
			(dotimes (i n)
				(setf new-list (cdr new-list))
			)
			(setf new-list (reverse new-list))
			(return-from chopping-last-block new-list)
		)  ; END let
	) ; END chopping-last-block
)
(defun assert-list (x name)
	"----------------------------------------------------------------
	assert-list (x name) print result of assertion that x is a list
	name is the symbol that is printed
	-----------------------------------------------------------------"
	(print "given object ")
	(prin1 name)
	(prin1 " -- ")
	(if (listp x)
		(prin1 " IS A LIST\n")
		(prin1 " IS A NOT LIST\n")
	)
)
(defun test-pass (x name-x)
	(def name-x 0)
	(prin1 name-x)
	(prin1  x)
	(prin1 "\n")

)
(defun range-delete (data &key (min-y minus-huge) (max-y huge))
	"----------------------------------------------------------------
	range-delete (data &key (min-y minus-huge) (max-y huge))
	data is a list of lists; there are assumed
	to be 2 component lists of the same length. Additional component
	lists are ignored.  The first list is assumed to be 
	time tags and the second list y data values that correspond
	to the time tags.  range-delete returns a like list of lists with all
	data points having values greater than min-y and less than max-y
	-----------------------------------------------------------------"
	(block range-delete-tag
	(let ((n 0) (m 0) (new-y-data 0) (time-tags 0) (new-tags 0)
		(y-data 0))
		(setf m (length data))
		;--------------------------------------------------------
		; make sure data contains at least 2 lists
		;--------------------------------------------------------
		(if (< m 2)
			(return-from range-delete-tag 
				(my-error 
				  "range-delete: less than 2 component lists")
			)
		)
		;--------------------------------------------------------
		; select first 2 component lists
		;--------------------------------------------------------
		(setf time-tags (select data 0))
		(setf y-data    (select data 1))
		(setf n  (length time-tags))
		;------------------------------------------------------
		; verify that 2 given lists are the same length
		;------------------------------------------------------
		(if (/= n (length y-data))
			(return-from range-delete-tag 
				(my-error "
			  range-delete: component list lengths not equal")
			)
		)
		;------------------------------------------------------
		; search for y-data elements greater than the given value
		; if found then remove the offending elements from both
		; time-tags and y-data
		;------------------------------------------------------
		(dotimes (i n)
			(when (and (> (nth i y-data) min-y)
					(< (nth i y-data) max-y))
				(when (listp new-tags)
					(setf new-tags 
					   (combine new-tags (nth i time-tags))
					)
					(setf new-y-data 
					   (combine new-y-data (nth i y-data))
					)
				)
				(when (not (listp new-tags))
					(setf new-tags (list (nth i time-tags)))
					(setf new-y-data (list (nth i y-data)))

				)
			)
		)
		;------------------------------------------------------
		; return a compound list of the edited data
		;------------------------------------------------------
		(setf new-data (list new-tags new-data))
	) ; END let
	) ; END block range-delete-tag
)
(defun my-error (msg)
	"---------------------------------------------------------------
	 my-error (msg)  prints msg and returns nil
	 ---------------------------------------------------------------"
	( block my-error-tag
		(print msg)
		(prin1 "\n")
		(return-from my-eror-tag nil)
	) ; END my-error-tag
)
(defun remove-from-list (x i)
	"---------------------------------------------------------------
	remove-from-list (x i)  returns list x with ith element removed
	if i is out of range the given list x is returned unchanged
	 ---------------------------------------------------------------"
	(block remove-tag
	(let ((n 0) (pre-list 0) (post-list 0) (out-list 0))
		(setf n (length x))
		;------------------------------------------------------
		; if i out of range then return the given list
		;------------------------------------------------------
		(if (< i 0) (return-from remove-tag x))
		(if (>= i n) (return-from remove-tag x))
		;------------------------------------------------------
		; handle the boundry conditions
		;------------------------------------------------------
		(if (= i 0) (return-from remove-tag (chop-first x)))
		(if (= i (- n 1)) (return-from remove-tag (chop-last x)))
		;------------------------------------------------------
		; handle the interior conditions
		;------------------------------------------------------
		(setf pre-list (chop-last x (- n i)))
		(setf post-list (chop-first x (+ 1 i)))
		(setf out-list (combine pre-list post-list))
	) ; END let
	) ; END remove-tag
)
(def max-int (- 2147483647 1))
(def min-int (- max-int))
(def max-short (- 32767 1))
(def min-short (- max-short))
(defun itoa (n)
	"----------------------------------------------------------------
	itoa(n) return integer converted to a string
	-----------------------------------------------------------------"
	(if (integerp n)
		(with-output-to-string (s) (princ n s))
		(print "not an integer\n")
	)
)
(defun ntoa (n)
	"----------------------------------------------------------------
	ntoa(n) return number converted to a string
	-----------------------------------------------------------------"
	(if (numberp n)
		(with-output-to-string (s) (princ n s))
		(print "not a number\n")
	)
)
(defun gmt-hhmmss (gmt)
	"----------------------------------------------------------------
	gmt-hhmmss (gmt) given gmt in seconds convert to hhmmss string
	-----------------------------------------------------------------"
	(let ((hr 0) (min 0) (sec 0) (size 0) (result 0) (hhmmss "") 
		(iresult "") (n 0))
		(setf hr (truncate (/ gmt 3600)))
		(setf min (truncate (/ (- gmt (* hr 3600)) 60) ))
		(setf sec (- gmt (* 3600 hr)))
		(setf sec (truncate (- sec (* 60 min))))
		(setf result (+ (* 10000 hr) (* 100 min) sec))
		(setf iresult (itoa result))
		(setf size (strlen iresult))
		(setf n (- 6 size))
		(dotimes (j n iresult)
			(setf iresult (strcat "0" iresult))
		)
	) ; END let
)
(defun vec-gmt-hhmmss (gmt)
	"----------------------------------------------------------------
	vec-gmt-hhmmss (gmt) given gmt in seconds convert to hhmmss string
	-----------------------------------------------------------------"
	( if (compound-data-p gmt) 
		( map-elements #'vec-gmt-hhmmss gmt)
		(gmt-hhmmss gmt)
	)
)
(defun strlen (s)
	"----------------------------------------------------------------
	strlen (s) used in gmt-hhmmss
	----------------------------------------------------------------"
	(length s)
)
(defun vec-const (x n)
	"----------------------------------------------------------------
	vec-const (x n) return a list of length n, each list element
	is equal to x
	-----------------------------------------------------------------"
	(let ((out-list 0))
		(dotimes (i n out-list)
			(if (= i 0)
				(setf out-list (list x))
				(setf out-list (combine out-list x))
			)
		)
	) ; END let
)
(defun chinorm (s df)
	"-------------------------------------------------------------
	chinorm (s df) returns transformation of chi-square
	s with df degrees of freedom to approximate N(0,1) deviate 
	-------------------------------------------------------------"
	(/ (- s (float df)) (sqrt (* 2 df)))
)
(defun trim-mean (x &key (high .95) (low .05))
	"---------------------------------------------------------------
	trim-mean (x &key (high .95) (low .05)) return trimmed mean of x
	high and low specify the quantiles to be trimmed from x
	----------------------------------------------------------------"
	(let ((high-q HUGE) (low-q MINUS-HUGE) (new-x 0) (t-mean 0))
		(if (< high 1)
			(def high-q (quantile x high))
		)
		(if (> low 0)
			(def low-q (quantile x low))
		)
		(setf new-x (remove-if #'(lambda (c) (< high-q c)) x))
		(setf new-x (remove-if #'(lambda (c) (> low-q c)) new-x))
		;(prin1 "new-x ")
		;(prin1 new-x)
		;(prin1 "\n")
		(setf t-mean (/ (sum new-x) (length new-x)))
	) ; END let
)
