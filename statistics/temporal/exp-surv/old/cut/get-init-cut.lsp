(provide 'get-init-cut)
;
;MEG 8/91
;This function gets the info to run the program such as filenames and options
;
(defun GET-INIT-CUT ()
  (let* ((te1 (send text-item-proto :new "CHOOSING CUT"))
         (te2 (send text-item-proto :new "IN COVARIATE"))
         (te3 (send text-item-proto :new "DEMONSTRATION"))
         (op (send text-item-proto :new "Options:"))
         (pdatas (send text-item-proto :new "Data Source:"))
         (pfilen (send text-item-proto :new "Filename:"))
         (plwid (send toggle-item-proto :new "Use thick lines for point density"))
         (datas (send choice-item-proto :new (list "Read data from file" "Generate data") :value 1))
         (filen (send edit-text-item-proto :new "censored" :text-length))
         (den (send toggle-item-proto :new "Density estimation plot")))
    
    (defun COLL ()
      (def datype (send datas :value))
      (def lwid (send plwid :value))
      (def filena (send filen :text))
      (def de (send den :value)))
    
    (setf ok (send modal-button-proto :new "OK" :action #'coll))
    (setf cancel (send modal-button-proto :new "CANCEL" 
                       :action #'(lambda () (def can t))))

    (setf cut-dialog
          (send modal-dialog-proto :new
                (list
                 (list
                  (list te1 pdatas datas  ok)
                  (list te2 pfilen filen op den plwid cancel)))))

    (send cut-dialog :modal-dialog)))
         
