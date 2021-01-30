;;; Library of Distributions



(require :el-conjugate (strcat ElToY-directory "conj" *d-sep*  "conjugate.lsp"))
(require :el-Norm-Norm (strcat ElToY-directory "Conj-Lib" *d-sep*  "Norm-Norm.lsp"))
(require :el-Beta-Binomial (strcat ElToY-directory "Conj-Lib" *d-sep* 
				   "Beta-Binomial.lsp"))
(require :el-Gamma-Poisson (strcat ElToY-directory "Conj-Lib" *d-sep* 
				   "Gamma-Poisson.lsp"))


(new-provide :el-conj-lib)

