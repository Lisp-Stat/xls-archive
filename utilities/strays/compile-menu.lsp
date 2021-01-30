;; This installs a compile menu on the macintosh

(defun compile-menu ()
(if (find :macintosh *features*)
(let* (
      (fmen (elt *standard-menu-bar* 1))
      (fitm (send fmen :items))
      (citm (send menu-item-proto :new "Compile" :key #\K
       :action #'(lambda() (compile-file (open-file-dialog)))))
      (titm (concatenate 'list (list (first fitm) citm) (rest fitm)))
      )
(mapcar #'(lambda (x) (send fmen :delete-items x)) fitm)
(mapcar #'(lambda (x) (send fmen :append-items x)) titm)
nil)))
