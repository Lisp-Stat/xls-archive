(new-section "Objects" "objects.scr")

(load "abrasion")

(def times '(9 13 13 18 23 28 31 34 45 48 161
             5 5 8 8 12 16 23 27 30 33 43 45))
(def status '(1 1 0 1 1 0 1 1 0 1 0 1 1 1 1 1 0 1 1 1 1 1 1))

(defun make-steps (x y)
  (let* ((n (length x))
         (i (iseq (+ (* 2 n) 1))))
    (list (append '(0) (repeat x (repeat 2 n)))
          (select (repeat (append '(1) y) (repeat 2 (+ n 1))) i))))

(setf *current-model* (regression-model (list hardness tensile-strength)
                                        abrasion-loss
                                        :predictor-names 
                                        (list (string 'hardness)
                                              (string 'tensile-strength))
                                        :print nil))

(defmeth *current-model* :r-squared ()
  (if (send self :intercept) (call-next-method)))

(defmeth *current-model* :plot-residuals (&rest args)
  (let ((p (apply #'call-next-method args))
         (n (send self :num-cases)))
    (send p :point-showing (iseq n) (send self :included))
    (send p :adjust-to-data)
    (defmeth p :adjust-to-included ()
      (let* ((included (make-array n))
             (showing (send self :points-showing))
             (vals (repeat t (length showing)))
             (i (iseq n)))
        (setf (select included showing) vals)
        (send *current-model* :included included)
        (send self :point-coordinate
              0 i (send *current-model* :fit-values))
        (send self :point-coordinate 
              1 i (send *current-model* :residuals))
        (send self :adjust-to-data)))
    (defmeth p :show-all-points ()
      (call-next-method)
      (send self :adjust-to-included))
    (defmeth p :erase-selection ()
      (call-next-method)
      (send self :adjust-to-included))
    (defmeth p :focus-on-selection ()
      (call-next-method)
      (send self :adjust-to-included))))

(setf *responses* (list 'abrasion-loss '(log abrasion-loss)))
(setf *predictors* (list 'hardness 'tensile-strength
                         '(^ hardness 2) '(^ tensile-strength 2)
                         '(* hardness tensile-strength)))

(defun string-of (x) 
  (let ((s (format nil "~s" x)))
    (if (<= (length s) 22)
        s
        (concatenate 'string (select (coerce s 'list) (iseq 19)) "..."))))

(setf *regdialog*
      (let ((x-text "Predictors")
            (y-text "Response")
            (intercept (send toggle-item-proto :new "Intercept" :value t))
            (resp-item (send choice-item-proto :new 
                             (mapcar #'string-of *responses*)))
            (pred-items (mapcar #'(lambda (x y) 
                                    (send toggle-item-proto :new
                                          (string-of x)
                                          :value y))
                                *predictors*
                                '(t t nil nil nil))))
        (flet ((adjust-model ()
                 (send *current-model* :intercept (send intercept :value))
                 (send *current-model* :y 
                       (eval (select *responses* (send resp-item :value))))
                 (let ((preds (select
                               *predictors*
                               (which (mapcar #'(lambda (x) (send x :value))
                                              pred-items)))))
                   (when (null preds)
                         (message-dialog "Must have at least one predictor!")
                         (send (first pred-items) :value t)
                         (setf preds (list (first *predictors*))))
                   (send *current-model* :x 
                         (apply #'bind-columns (mapcar #'eval preds)))
                   (send *current-model* :predictor-names
                         (mapcar #'string-of preds)))))
          (let ((ok (send modal-button-proto :new "OK" 
                          :action #'adjust-model))
                (cancel (send modal-button-proto :new "Cancel")))
            (send modal-dialog-proto :new 
                  (list (list (cons x-text pred-items) 
                              (list y-text resp-item intercept))
                        (list ok cancel))
                  :show nil)))))

(setf *regmenu* (send menu-proto :new "Abrasion-Loss"))

(send *regmenu* :append-items
      (send menu-item-proto :new "Display" :action
            #'(lambda () (send *current-model* :display)))
      (send menu-item-proto :new "Plot Residuals" :action
            #'(lambda () (send *current-model* :plot-residuals)))
      (send dash-item-proto :new)
      (send menu-item-proto :new "Change Model..." :action
            #'(lambda () (send *regdialog* :modal-dialog)))
      (send dash-item-proto :new)
      (send menu-item-proto :new "Remove Menu" :action
            #'(lambda () (send *regmenu* :remove))))

(define-section-item "Regression"
  (if (send *regmenu* :installed-p)
      (send *regmenu* :remove)
      (send *regmenu* :install)))

