(load "uclacore")
(load "uclainterface")
(load "actions/ucladescribe")
(load "actions/uclaimpute")
;(load "actions/uclarecode")
;(load "actions/uclaregress")
;(load "actions/uclaresample")
;(load "actions/uclatest")
;(load "actions/uclatransform")
;(load "actions/uclaplot")
(load "actions/uclaparameters")
(load "actions/uclasave")
(load "actions/uclaedit")
(load "utils/uclautility")
;(load "utils/uclaplotmethods")

(defun file-dialog ()
(let* (
      (d (read-data-file "datalist"))
      (l (send text-item-proto :new "Read Data from File"))
      (m (send list-item-proto :new d
          :action #'(lambda (x)
                      (if x
                       (let* (
                             (k (send m :selection))
                             (f (elt d k))
                             )
                         (load (if (string= f "other") (other-file) f))
                         (send cdiag :modal-dialog-return nil))))))
      (cancel (send modal-button-proto :new "Cancel"))
      (cdiag (send modal-dialog-proto :new
        (list
        (list (list l m))
        (list cancel))))
      )
(send cdiag :modal-dialog)
))

(defun other-file ()
(if (find ':macintosh *features*)
    (open-file-dialog "Select a file") 
    (get-string-dialog "Give the
          name of an existing file"))
)

(file-dialog)
