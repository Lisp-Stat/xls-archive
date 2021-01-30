(setq self (vinit))


(defun go (other) 
  (read-time)
  (do ((times 0))
      ((> times 1000)
       (printf1 "total time: ") (printf (read-time)))
      
      (vthrow `(,other) `(vthrow '(,self) "message"))
      (do ()
	  ((vcatch))
	  (vtask))

      (setq times (1+ times))
      ))


(defun gone ()
  (loop 
   (vtask)
   (eval (vcatch))
   ))


