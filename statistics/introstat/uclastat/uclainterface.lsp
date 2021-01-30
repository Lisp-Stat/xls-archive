(defmeth multi-variable-proto :data-dialog ()
(let* (
       (sp (if (find ':macintosh *features*) 
               (string (code-char 18))
               (string (code-char 62))))
       (tp (string #\Space))
       (l1 (list "Describing" "Testing" "Plotting" "Regressing" "Edit" 
                 "Imputing" "Resampling" "Reverting" "Saving" "Parameters"))
       (l2 (send self :variable-labels))
       (l3 (repeat tp (length l2)))
       (l4 (list "legend"))
       (lc (send list-item-proto :new l4
           :action #'(lambda (x)
                       (if x (send self :show-legend)))))
       (la (send list-item-proto :new l2 
                 :action #'(lambda (x)
                             (let* (
                                   (lr (send la :selection))
                                   )
                             (if x (send ls :set-text lr 
                               (if (string= (elt l3 lr) sp) 
                                   (setf (elt l3 lr) tp)
                                   (setf (elt l3 lr) sp))))))))
       (ls (send list-item-proto :new l3
                 :size (list 20 (second (send la :slot-value 'size)))))
       (lb (send list-item-proto :new l1 
                 :action #'(lambda (x)
                             (if x (send self :do-the-action
                                   (send lb :selection)
                                   (which (map-elements #'string= sp l3))))
                                    )))
       (ok (send modal-button-proto :new "Enough !"
                 :action #'(lambda () (send the-dialog :close))))
       (lt (send text-item-proto :new (send self :title)))
       (lw (send text-item-proto :new " "))
       (lu (send text-item-proto :new "Actions"))
       (lv (send text-item-proto :new "Case Labels"))
       (the-dialog (send modal-dialog-proto :new
                         (list
                          (list (list lw ls)
                                (list lt la lc) 
                                (list lu lb ok)))
                         :default-button ok))
)
))

(defmeth multi-variable-proto :do-the-action (i varlist)
(print varlist)
(case i
  (0 (send self :describe))
  (1 (send self :test))
  (2 (send self :plot))
  (3 (send self :regress))
  (4 (send self :select))
  (5 (send self :impute))
  (6 (send self :resample))
  (7 (send self :revert))
  (8 (send self :save))
  (9 (send self :parameters))
  (t "Other")
))

(defmeth multi-variable-proto :show-legend ()
  (if (find ':macintosh *features*)
      (send (send display-window-proto :new :title (send self :title))
            :paste-string (send self :legend))
(message-dialog (send self :legend))
))
