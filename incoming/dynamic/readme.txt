Readme file for Parallel-FEDF, FEDF-ScatterPlot, FEDF-StarPlot written in XLISP-STAT.
These plots are suggested for exploring multidimensional data suggested in 
"Journal of Computational and Graphical Statistics", Vol. 4, No. 4, pp.335-343.
97/07/18


Parallel-FEDF _________________________________________________

To run the program, first open XLISP-STAT listner. 
Then load F-paral.lsp using the load of File menu.
Format for the function is as follows:

(parallel-fedf data &key var-labels labels np category 
                       (color nil) (title "FEDFs") (symbol 'disk))

" data: the only required parameter.
  var-labels : labels for variables
  labels : labels for observations
  np: number of plots /horizon
  category: variable numer for categorical data
  data structure:
  ((x-list, y-list, #points), (x-list, y-list, #points),...)"

ParaDemo.lsp is an example file.


FEDF-ScatterPlot_____________________________________

To run the program, first open XLISP-STAT listner. 
Then load F-scatt.lsp using the load of File menu.
Format for the function is as follows:

(fedf-scatter data 
   &key (var-labels nil) (scale '(.7 .6)) (symbol 'dot3) (title "FEDF scatterplot"))

" data: the only required parameter.
  var-labels : labels for variables
  scale : x-y scale for the plot 
  symbol : symbols for the data points
  title : title for the plot"

ScatDemo.lsp is an example file


FEDF StarPlot___________________________________________________

To run the program, first open XLISP-STAT listner. 
Then load F-star.lsp using the load of File menu.
Format for the function is as follows:

(defun star-plot (data &key var-labels labels  (height .27) 
                      (title "FEDF star-plot") (symbol 'disk))

" data: the only required parameter.
  var-labels : labels for variables
  height : height of the FEDF on each starPlot axis (0 - 1).  
  symbol : symbols for the data points
  title : title for the plot"

StarDemo.lsp is an example file

_____________________________________________________________________

Any questions or suggesttions are welcome to the following address:

	myhuh@yurim.skku.ac.kr

______________________________________________________________________
Huh, Moon Yul
Statistics Reasearch Institute of Sung Kyun Kwan University
 
