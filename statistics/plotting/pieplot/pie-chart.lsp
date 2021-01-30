;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  A pie chart maker.
;;;
;;;  Uses LaTeX, dvips, and Timothy Van Zandt's pstricks package
;;;  Somewhat inspired by the sh/awk version of Denis Girou, and
;;;  the discussion on pages 42-47 of the pstricks manual (version
;;;  0.93a, march 1993).
;;;  
;;;  Typical call
;;;  
;;;  (pie-chart (list 14 3 10 8 8) (list "US" "NL" "JP" "FR" "GB")
;;;              :values (list "14" "3" "10" "8" "8"))
;;;
;;;  Jan de Leeuw, 02-23-95
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun pie-chart (data labels 
    &key (header t) (values nil) (colors nil) (fillings nil)
         (size 5) (radius 4.5) (vrad 2.5))
(let* (
       (sd (* 360 (/ data (sum data))))
       (sc (append '(0) (cumsum sd)))
       (nn (length data))
       )
(with-open-file (pie "pie.tex" :direction :output)
(if header (progn
(format pie "\\documentclass{article}~%")
(format pie "\\usepackage{pstricks}~%")
(format pie "\\begin{document}~%")))
(format pie "\\psset{unit=1cm}~%")
(format pie "\\begin{pspicture}(-~3,1f,-~3,1f)(~3,1f,~3,1f)~%" 
        size size size size)
;(format pie "\\newgray{mygray}{0}~%")
;(format pie "\\psset{fillstyle=solid,fillcolor=mygray}~%")
(format pie "\\SpecialCoor~%")
(format pie "\\psset{framesep=1.5pt}~%")
(dotimes (i nn)
(format pie "\\newgray{mygray}{~3,2f}~%" (/ i (1- nn)))
(format pie "\\pswedge[fillstyle=solid,fillcolor=mygray]{~3,1f}{~3,0f}{~3,0f}~%"
        radius (elt sc i) (elt sc (1+ i)))
(format pie "\\uput{~3,1f}[~3,0f](0,0){~a}~%"
        size (/ (+ (elt sc i) (elt sc (1+ i))) 2) (elt labels i))
(if values
(format pie "\\rput(~3,0f;~3,0f){\\psframebox*{\\small ~a}}~%"
        vrad (/ (+ (elt sc i) (elt sc (1+ i))) 2) (elt values i)))
)
(format pie "\\end{pspicture}~%")
(if header
(format pie "\\end{document}~%"))
)
))