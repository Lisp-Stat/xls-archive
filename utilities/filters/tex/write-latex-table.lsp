(defun write-latex-table (x &key (title "table.tex") (how "r") (rule nil)
	(form "full") (size "normalsize") (caption "caption"))
"Args: x
Takes a matrix of numbers or strings, and writes it to a file
in a LaTeX table. Optional parameters for the title of the file,
for the caption, for the alignment (r, c, l),
for the format (full, upper, lower), for rules between the lines,
and for the LaTeX size (normalsize, tiny, LARGE, etc). Returns nil."
(let* (
     (n (first (array-dimensions x)))
     (m (second (array-dimensions x)))
     (s (with-output-to-string (form)
                               (format form "~a" "|")
                               (dotimes (i m)
                                 (format form "~a~a" "|" how)
                                 (if (= i (1- m)) (format form "~a"
"||")))))
     )
     (with-open-file (texfile title :direction :output)
            (format texfile "~a[ht]~%" "\\begin{table}")
            (format texfile "~a~%" "\\centering")
            (format texfile "\\~a~%" size)
	    (format texfile "~a{~a}\\hline~%" "\\begin{tabular}" s)
            (dotimes (i n)
              (dotimes (j m)
(if (or (string= form "full")
    (and (string= form "upper") (>= j i))
    (and (string= form "lower") (<= j i)))
                (format texfile "~a" (aref x i j)))
                (if (= j (1- m))
                    (if (= i (1- n)) (format texfile " \\\\\\hline~%")
	              (if rule (format texfile " \\\\\\hline~%")
                      (format texfile " \\\\~%")))
                  (format texfile " & "))))
            (format texfile "~a~%" "\\end{tabular}")
            (format texfile "~a{~a}~%" "\\caption" caption)
            (format texfile "~a~%" "\\end{table}"))
     ))
