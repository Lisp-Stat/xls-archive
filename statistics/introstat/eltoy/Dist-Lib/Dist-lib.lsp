
;;; Library of Distributions

(require :el-family (strcat ElToY-directory "fam" *d-sep*  "family.lsp"))
(require :el-normal (strcat ElToY-directory "Dist-Lib" *d-sep*  "Normal.lsp"))
(require :el-cauchy (strcat ElToY-directory "Dist-Lib" *d-sep*  "Cauchy.lsp"))
(require :el-chisq (strcat ElToY-directory "Dist-Lib" *d-sep*  "Chisq.lsp"))
(require :el-gamma (strcat ElToY-directory "Dist-Lib" *d-sep*  "Gamma.lsp"))
(require :el-beta (strcat ElToY-directory "Dist-Lib" *d-sep*  "Beta.lsp"))
(require :el-student-t (strcat ElToY-directory "Dist-Lib" *d-sep*  "Student-t.lsp"))
(require :el-F (strcat ElToY-directory "Dist-Lib" *d-sep*  "Snedecor-F.lsp"))
(require :el-binomial (strcat ElToY-directory "Dist-Lib" *d-sep*  "Binomial.lsp"))
(require :el-negbinomial (strcat ElToY-directory "Dist-Lib" *d-sep* 
				 "NegBinomial.lsp"))
(require :el-Poisson (strcat ElToY-directory "Dist-Lib" *d-sep*  "Poisson.lsp"))
(require :el-hypergeometric (strcat ElToY-directory "Dist-Lib" *d-sep* 
				    "Hypergeometric.lsp"))
(require :el-uniform (strcat ElToY-directory "Dist-Lib" *d-sep*  "Uniform.lsp"))
(require :el-bootstrap (strcat ElToY-directory "Dist-Lib" *d-sep*  "Bootstrap.lsp"))



(new-provide :el-dist-lib)
