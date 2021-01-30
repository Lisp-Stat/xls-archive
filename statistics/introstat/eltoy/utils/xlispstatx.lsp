;;; Extensions to xlispstat to 

(defmeth dialog-proto :destruct (&rest args)
  (send self :dispose))

(new-provide :el-xlispstatx)

