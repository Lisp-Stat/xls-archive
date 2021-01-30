(let* (
       (to0 (send text-item-proto :new
                   "mu-1"))
       (vo0 (send text-item-proto :new
                   "" :text-length 10))
       (so0 (send interval-scroll-item-proto :new
                   '(0 10)
                   :text-item vo0
                   :action
                      #'(lambda(x) (format t "mu-1=~g~%" x))))
       (to1 (send text-item-proto :new
                   "mu-2"))
       (vo1 (send text-item-proto :new
                   "" :text-length 10))
       (so1 (send interval-scroll-item-proto :new
                   '(0 10)
                   :text-item vo1
                   :action
                      #'(lambda(x) (format t "mu-2=~g~%" x))))
       (to2 (send text-item-proto :new
                   "sigma-1"))
       (vo2 (send text-item-proto :new
                   "" :text-length 10))
       (so2 (send interval-scroll-item-proto :new
                   '(1 10)
                   :text-item vo2
                   :action
                      #'(lambda(x) (format t "sigma-1=~g~%" x))))
       (to3 (send text-item-proto :new
                   "sigma-2"))
       (vo3 (send text-item-proto :new
                   "" :text-length 10))
       (so3 (send interval-scroll-item-proto :new
                   '(1 10)
                   :text-item vo3
                   :action
                      #'(lambda(x) (format t "sigma-2=~g~%" x))))
       (to4 (send text-item-proto :new
                   "rho12"))
       (vo4 (send text-item-proto :new
                   "" :text-length 10))
       (so4 (send sequence-scroll-item-proto :new
                   '(0.0 0.122 0.25 0.377 0.5 0.622 0.75 0.877 1.0)
                   :text-item vo4
                   :action
                      #'(lambda(x) (format t "rho-12=~g~%" x)))))
  (send dialog-proto :new
        (list
          (list to0 vo0 to1 vo1)
          (list so0 so1)
          (list to2 vo2 to3 vo3)
          (list so2 so3)
          (list to4 vo4)
          so4.)))
