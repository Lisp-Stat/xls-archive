(require "sliders")

(def l (list
	(list "mu-1" '(0 10) "(lambda(x) (format t \"mu-1=~g~%\" x))")
	(list "mu-2" '(0 10) "(lambda(x) (format t \"mu-2=~g~%\" x))")
	(list "sigma-1" '(1 10) "(lambda(x) (format t \"sigma-1=~g~%\" x))")
	(list "sigma-2" '(1 10) "(lambda(x) (format t \"sigma-2=~g~%\" x))")
	(list "rho12" (rseq 0 1 9)
	      "(lambda(x) (format t \"rho-12=~g~%\" x))")))

(make-sliders l :cols 2 :fname "code.lsp")
(exit)
