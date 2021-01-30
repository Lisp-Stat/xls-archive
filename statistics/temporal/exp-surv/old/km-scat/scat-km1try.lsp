;
; generates a scatterplot matrix linked to
; a Kaplan-Meier estimate.  Also installs the covariate menu so that
; the save data points option can be executed to save points in scatterplot
;
(load "kmest")
(load "km-plot")
(provide 'scat-km1try)
;

(defun SCAT-KM1try (time status numcov covars)

  (let* ((km (send graph-proto :new 2))
         (scat (scatterplot-matrix covars))
         (data (km-plot time status (kmest time status)))
         (current-points (iseq (length (first covars))))
         (dash-item (send dash-item-proto :new))  ;menu definitions
         (save-point-item (send menu-item-proto :new "Save Data Points"
                :action #'(lambda () (setf saving t)))))
           
    (def saving nil)
    (send km :start-buffering)    
    (send km :variable-label '(0 1) (list "Time" "Proportion Surviving"))
    (send km :location 310 200) (send km :size 245 230)
    (send km :add-lines (list (first data) (second data)))
    (send km :use-color t)
    (def net (send km :num-lines))
    (send km :linestart-color (iseq 0 (- net 1)) 'red)
    (send km :x-axis t t 5) (send km :y-axis t t 5)
    (send km :title "Kaplan-Meier Curve")
    (send km :range 1 0 1)

    (let ((ti1 (format nil"To save points to a file, click on the ~%~
'Save Data Points' option in the Covariate menu~%~
and then select the points to be saved.  ~%~
You will be prompted for the file name.")))
      (ok-or-cancel-dialog ti1))
    
    (setf cov-men (send menu-proto :new "Covariates"))
 
    (send scat :use-color t)
    (send scat :size 245 230) (send scat :location 40 200)
    (def nump (send scat :num-points))
    (send scat :point-color (iseq 0 (- nump 1)) 'blue)
    (send scat :point-state (iseq (length (first covars))) 'hilited)    
    (send scat :title "Covariates")
        
;------ make sure all menus close properly when plot closes -------
    (defmeth km :close ()
      (if (not (eql (send save-point-item :menu) NIL))
          (send cov-men :delete-items dash-item save-point-item))
      (undef (variables))
      (send cov-men :remove)
      (call-next-method))
    
    (defmeth scat :close ()
      (if (not (eql (send save-point-item :menu) NIL))
          (send cov-men :delete-items dash-item save-point-item))
      (send cov-men :remove)
      (call-next-method))


    (defmeth scat :unselect-all-points ()
      (setf current-points nil)
      (setf points-save nil)
      (call-next-method))

    (defmeth scat :adjust-points-in-rect (x1 y1 w h s)
      (if saving
          (let* ((p-t-s (send self :points-in-rect x1 y1 w h)))
            (setf points-save
                  (case (send self :mouse-mode)
                    (brushing p-t-s)
                    (selecting (union p-t-s points-save))))
            (if points-save            ;saving points to a new file or data set
                 (list
               (with-open-file (f (get-string-dialog "Filename:" :initial "point-file") :direction :output)
                                 (dolist (mm points-save)
                                         (dolist (nn covars)
                                                 (format f "~g " (select nn mm)))
                                         (terpri f)))
                (setf saving nil))))    ;set saving to original value
            
          (let* ((p-i-r (send self :points-in-rect x1 y1 w h)))
            (setf current-points
                  (case (send self :mouse-mode)
                    (brushing p-i-r) 
                    (selecting (union p-i-r current-points))))
            (if current-points                
                (prog* 
                 ((points (sort-data current-points))          
                  (time-sel (select time points))               
                  (status-sel (select status points))           
                  (new-plot (km-plot time-sel status-sel        
                                     (kmest time-sel status-sel)))
                  (lennew (length (first new-plot)))
                  (ind1 (iseq 0 (- lennew 1)))
                  (end1 (iseq lennew (- net 1))))

	;---- adjusts km curve to selected points --------
  
                 (send km :linestart-coordinate 0 ind1 (select (first new-plot) ind1))
                 (send km :linestart-coordinate 1 ind1 (select (second new-plot) ind1))
                 (send km :linestart-coordinate 0 end1 (select (first new-plot) (- lennew 1)))
                 (send km :linestart-coordinate 1 end1 0)
                 (send km :redraw-content)))))
                         
            (call-next-method x1 y1 w h s))

    (send cov-men :append-items dash-item save-point-item)
    (send cov-men :install)
    (send km :buffer-to-screen) scat))
