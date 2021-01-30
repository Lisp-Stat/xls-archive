(defmeth multi-variable-proto :save ()
(let (
     (file (send display-window-proto :new))
     )
(send file :paste-string
      (format nil "(setf data '~a)~%"
              (with-output-to-string (s) (prin1 (send self :data) s))))
(send file :paste-string
      (format nil "(setf var (make-variable data))~%"))
(send file :paste-string
      (format nil "(send var :case-labels '~a)~%"
              (with-output-to-string (s) (prin1 (send self :case-labels) s))))
(send file :paste-string
      (format nil "(send var :title ~a)~%" 
              (with-output-to-string (s) (prin1 (send self :title) s))))
(send file :paste-string
      (format nil "(send var :code ~a)~%" 
              (with-output-to-string (s) (prin1 (send self :code) s))))
(send file :paste-string
      (format nil "(send var :process ~a)~%"
              (with-output-to-string (s) (prin1 (send self :process) s))))
(send file :paste-string
      (format nil "(send var :level ~a)~%" 
              (with-output-to-string (s) (prin1 (send self :level) s))))
))
