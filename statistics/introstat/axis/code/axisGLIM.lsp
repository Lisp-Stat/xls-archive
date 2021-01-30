;;;;
;;;;     axisGLIM.lsp © Robert A. Stine, 1994
;;;;
;;;;             Add in module for augmenting Axis with interface
;;;;             to the glim.lsp code of Tierney.
#||
    17 Apr 94 ... Created to extend Axis.
||#

(require "glim")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                         ;;
;;         Add commands to the statistics menu.            ;;
;;                                                         ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun ADD-GLIM-ITEMS ()
  (send *statistics-menu* :append-items
        (send stat-menu-item-proto :new
              "Logistic Regr" glim-cmd-proto)
        ))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                         ;;
;;          Generalized Linear Model Command               ;;
;;                                             17 Apr 94   ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defproto GLIM-CMD-PROTO
  '(joinToggle)
  '(classIDCode)       ;; required for every "type" of command
  commando-proto)


(defmeth glim-cmd-proto :ISNEW ()
  (send self :init-slots
        "Generalized Linear Model"
        'binomialreg-model
        (list "Response   ->"
              (list "Covariates ->"))  )
  (call-next-method))

(defmeth glim-cmd-proto :COMMAND ()
  (let* ((txt  (send self :text))   
         (form (send self :form))
         ( x   (select form 1))    )
    (cons
     (slot-value 'cmdForm)
     `( (list ,@x)
        ,(select form 0)
        1
        :response-name ,(first txt)
        :predictor-names (list ,@(second txt))
        :case-labels ^cases
        :included ^filter)
     )))


(defmeth glim-cmd-proto :DOIT ()
  (call-next-method)
  (setf *regression* (slot-value 'target))
  (send *regression* :display-error-rate)
  )

#|
  (def gc (send glim-cmd-proto :new))
|#


(defmeth glim-cmd-proto :BUTTON-ITEMS ()
  (let ((print-but (send button-item-proto :new "Print"
                         :action #'(lambda ()
                                     (send (slot-value 'target) :display))))
        (resid-but (send button-item-proto :new "Plot Resids"
                         :action #'(lambda ()
                                     (send (slot-value 'target)
                                           :plot-residuals))))
        (save-but  (send button-item-proto :new "Save Data"
                         :action #'(lambda ()
                                     (saver (slot-value 'target)
                                            (send self :dataset)))))
        )
    (list (list print-but resid-but save-but))
    ))
        

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                         ;;
;;         Extensions to Generalized Linear Model          ;;
;;                                             17 Apr 94   ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmeth glim-proto :DISPLAY-ERROR-RATE ()
  (let ((y (send self :yvar))
        (f (send self :fit-means))  )
    (format t "Error rate = ~6,3f~%"
            (/ (sum (abs (- y (round f))))
               (length y)))
    ))
                 
(defmeth glim-proto :SAVE-ALIST ()
  ; Association list of method-selectors and names for save dialog.
  (list
   '(residual  . :raw-residuals)
   '(deviance  . :deviances)
   '(fitted    . :fit-means)
   '(stanres   . :studentized-residuals)
   '(StudRes   . :externally-studentized-residuals)
   '(Leverage  . :leverages)
   '(CookD     . :cooks-distances)
   '(weights   . :weights)))

                                       

(add-glim-items)
