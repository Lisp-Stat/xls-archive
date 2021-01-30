(defmeth multi-variable-proto :edit-dialog (ivar)
(let* (
     (dt (elt (send self :data) ivar))
     (pt (first (send self :precedence-list)))
     (l2 (make-array (list 1 3) :initial-contents
               (list (list "Lables") (list "Weights") 
                     (list (elt (send self :variable-labels) ivar)))))
     (la (send list-item-proto :new l2 :columns 3)) 
     (ok (send modal-button-proto :new "Enough !"
               :action #'(lambda () (send the-dialog :close))))
     (cl (send self :case-labels))
     (wl (send self :weights))
     (pm (transpose (make-array (list 3 (length dt)) :initial-contents
         (list cl wl (to-string-list dt)))))
     (ll (send list-item-proto :new pm :columns 3
          :action #'(lambda (x)
                      (if x (send self :click-edit ivar 
                                  (send ll :selection)
                                  (send ll :replace-cell))))))
     (the-dialog (send modal-dialog-proto :new
       (list
       (list la)
       (list ll) 
       (list ok)
       ) :default-button ok))
)))

(defmeth multi-variable-proto :click-edit (ivar ind new)
(let (
     (k1 (first ind))
     (k2 (second ind))
     (aa (elt (send self :data) ivar))
     )
(cond ((= k2 0) (setf (elt (send self :case-labels) k1) new))
      ((= k2 1) (setf (elt (send self :weights) k1) new))
      (t (setf (elt aa k1) new) 
))))

(defmeth list-item-proto :update (var)
(dotimes (i (length var))
         (send self :set-text (list i 1) (elt var i)))
)

(defmeth list-item-proto :edit-cell (ind)
(let* (
       (k1 (first ind))
       (k2 (second ind))
       (h (if (= k2 0) (get-string-dialog "Enter a label:")
              (first (get-value-dialog "Enter an expression:"))))
       )
(cond 
     ((stringp h) h)    
     ((numberp h) (num-to-string h))
     (t (message-dialog "Must be a Number or String")))
))

(defmeth list-item-proto :replace-cell ()
(let* (
       (kk (send self :selection))
       (aa (send self :edit-cell kk))
       )
(send self :set-text kk 
      (if (stringp aa) aa (num-to-string aa)))
aa))

(defmeth multi-variable-proto :change-element (i x)
 (setf (elt (send self :data) i) x))

(defmeth multi-variable-proto :revert ()
(let (
     (ls (copy-list (send self :backup)))
     )
(send self :data ls)
))

