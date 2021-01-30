(load "gen-km-scat-data")
(load "scat-km1try")
(load "get-init-km-scat")
(load "read-col")

(undef (variables))
(def can nil)
(get-init-km-scat)

(if (eql can nil)
    (list (cond ((= dattype 0) (read-col filena numcov))
            ((= dattype 1) (gen-km-scat-data 100 0.1)))
         
          (setf skm1 (scat-km1try time1 stat1 numcov covdata))))

