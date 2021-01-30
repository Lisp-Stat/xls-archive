(provide 'get-init-km-scat)
(load "change-to-values")
;
;MEG 8/91
;This function gets the filenames, options, and number of covariates
;
(defun GET-INIT-KM-SCAT ()
  (let* ((ye1 (send text-item-proto :new "KAPLAN-MEIER PLOT AND"))
         (ye2 (send text-item-proto :new "SCATTERPLOT MATRIX"))
         (spa (send text-item-proto :new " "))
         (pdatas (send text-item-proto :new "Data source:"))
         (pfilen (send text-item-proto :new "Filename:"))
         (filen (send edit-text-item-proto :new "censored" :text-length 10))
         (datas (send choice-item-proto :new (list "Read data from file" "Generate data") :value 1))
         (pnum (send text-item-proto :new "# covariates:"))
         (num (send edit-text-item-proto :new "3" :text-length 2)))
        
    (defun COOL ()
      (def dattype (send datas :value))
      (def filena (send filen :text))
      (def numcov (change-to-values (send num :text))))
      
    (setf ok (send modal-button-proto :new "OK" :action #' cool))
    (setf cancel (send modal-button-proto :new "CANCEL" 
                       :action #'(lambda () (def can t))))

    (setf km-dialog
          (send modal-dialog-proto :new
                (list
                 (list
                  (list ye1 pdatas datas spa ok)
                  (list ye2 pfilen filen pnum num cancel)))))
    (send km-dialog :modal-dialog)))
                  

         

         
                   
