(defun checknump (num? &optional integer?)
  "Is argument <num?> a valid number.  If <integer?> <num?> must be an
integer. (vector-reducing).
"
  (let ((out   (if integer?
		   (if (compound-data-p num?) (every #'integerp (element-seq num?))
		     (integerp num?))
		 (if (compound-data-p num?) (every #'numberp (element-seq num?))
		   (numberp num?)))
	       ))
    (format t "checknump: ~S ~S --> (~S, ~S ~S) ~S 5=~S~%" num? integer?
	    (compound-data-p num?) (integerp num?) (numberp num?) out
	    (eql num? 5))
    out))

