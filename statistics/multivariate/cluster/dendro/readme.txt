Readme file for dendrogram written in XLISP-STAT.
Runs for WIN32 XLS Release 3.41 and above.
97/07/15

_________________________________________________
This program is for producing dendrogram for agglomerative cluster. 
To run the program, first open XLISP-STAT listner. Then load dend.fsl using the load of File menu.
Format for dendrogram function is as follows:

(dendrogram <dataname> [:linkage <linkage> :proximity <proximity measure>
			:labels <labels> :draw <t|nil>])

Only <dataname> is mandatory. 
:linkage keyword is for selecting one of the linkage methods from
	average, centroid, complete, gower, single, weighted, ward
	default is single.
:proximity keyword is for selecting one of the proximity measures from 
	euclidean, std-euclidean, mahalanobis, cityblock, correlation, cosine
	default is euclidean.
:labels keyword is for the labels of each observation, and :draw keyword is for drawing labels on the dendrogram. 
When :labels is not specified and :draw is T, order of the observations is given as labels.

demo.lsp is an example file.

Any questions or suggesttions are welcome to the following address:

	myhuh@yurim.skku.ac.kr

___________________________________________________
Lee, Kyungmi & Huh, Moon Yul
Statistics Reasearch Institute of Sung Kyun Kwan University
 
