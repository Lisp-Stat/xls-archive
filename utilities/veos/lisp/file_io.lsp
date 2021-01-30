;; save partition of grouplespace to given file
;; specify partition with pattern arg

(defun save-gspace (file-name pattern)

  (let (fp)
    (cond ((setq fp (open file-name :direction :output))

	   (print (vcopy pattern) fp)
	   (close fp)
	   t))))



;; load file into partition of grouplespace
;; specify partition with pattern arg

(defun load-gspace (file-name pattern)

  (let (err fp)
    (cond ((setq fp (open file-name :direction :input))

	   (setq err (vput (read fp) pattern))
	   (close fp)
	   err))))



