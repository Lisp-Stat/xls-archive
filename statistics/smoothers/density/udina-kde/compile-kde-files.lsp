

;;;function from cmpsys.lsp in Lisp Sources, xls-3-44 distribution
(defun compile-if-needed (file &optional (load t))
         (let ((lspfile (merge-pathnames file ".lsp"))
               (fslfile (merge-pathnames file ".fsl")))
           (unless (and (probe-file lspfile)
                        (probe-file fslfile)
                        (< (file-write-date lspfile)
                           (file-write-date fslfile)))
                   (compile-file file :load load))))

(def kde-lisp-files '("runkde"
		      "kde"
		      "calckde"
		      "binning"
		      "wkde"
		      "kdehisto"
		      "transfor"
		      "funnorms"
		      "plotline"
		      "goldsear"
		      "distrobj"
		      "getfile"))

(mapcar #'compile-if-needed kde-lisp-files)


