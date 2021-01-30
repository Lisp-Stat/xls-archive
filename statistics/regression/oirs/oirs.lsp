(require "../revised/regmodel")
(require "../revised/auxil")

;; The following three functions generate all permutations
;; of (0 1 ...n-1)
;;
(defun build-perm (l)
  (let ((z nil))
    (dolist (i l z)
	    (setf z (append (insert-into-list i) z)))))
(defun insert-into-list (l)
  (let* ((z nil)
	 (n (length l))
	 (is (iseq 0 (1- n))))
    (dotimes (i (1+ n) z)
	     (setf z (cons 
		      (append (select l (which (< is i))) 
			      (list n)
			      (select l (which (>= is i)))) z)))))
(defun perm (n)
"Args: (n) Returns all permutations of (0 1 ...n-1)"
  (let ((z '((0))))
    (dotimes (i (1- n) z)
	     (setf z (build-perm z)))))

(defun regression-analyzer (r reg-path &key (cycle t))
  "Args: (r reg-path) 
Returns reg-model resulting from data analysis of regression model
r following path in reg-path, cycling until acceptable if cycle is T"
  (let ((ctr 0)
	(lrgp (length reg-path)))
    (dolist (i reg-path)
	    (if (send r i)
		(setf ctr 0)
	      (setf ctr (1+ ctr))))
    (if cycle
	(do ((i 0 (rem (1+ i) lrgp))
	     (ic 0 (1+ ic)))
	    ((or (= ctr lrgp) (> ic (* 3 lrgp))) r)
	    (if (send r (nth i reg-path))
		(setf ctr 0)
	      (setf ctr (1+ ctr))))
      r)))

(defun all-perm-eda (x y actions 
		       &key (vb-select (repeat t (length x))) (print t) 
		       (cycle t) predictor-names response-name case-labels)
"Args: (x y actions &key (vb-select (repeat t (length x))) (print t) 
		       (cycle t) predictor-names response-name case-labels)
Given predictors X, response Y, list of RAPs ACTIONS, 
optionally list of droppable predictors VB-SELECT,
progress printing PRINT, CYCLE through RAPs until,
acceptable, returns a list of regression models analyzed
by all possible permutions of actions"
  (let ((z nil) (r nil)
	(allp (perm (length actions))))
    (dolist (i allp z)
	    (if print
		(format t "~a~%" i))
	    (setf r (reg-model x y :vb-select vb-select
			       :print nil
			       :predictor-names predictor-names
			       :response-name response-name
			       :case-labels case-labels))
	    (push (regression-analyzer r (select actions i) :cycle cycle) z))))

(defun rlistmap (rl method &optional arg)
  (if arg
      (mapcar (lambda (x) (send x method arg)) rl)
    (mapcar (lambda (x) (send x method)) rl)))

;; Leave out one jacknife data analysis on x y

(defun quant-inf (x y reg-path &key (vb-select (repeat t (length x))))
  (let* ((r (reg-model x y :print nil :vb-select vb-select))
	 (icook (send r :cooks-distances))
	 (fr (regression-analyzer r reg-path :cycle t))
	 (ffv (send fr :prediction (transp x)))
	 (p (send fr :num-coefs))
	 (sh (^ (send r :sigma-hat) 2))
	 (fcook (send fr :cooks-distances))
	 (z nil))
    (dotimes (i (length y) (list icook fcook (/ (reverse z) (* sh p))))
             (let* ((im (reg-model (mapcar (lambda (z) (rmel i z)) x)
				   (rmel i y) 
				   :vb-select vb-select
				   :print nil))
		    (final-model (regression-analyzer im reg-path :add-actions t))
		    (d (- (nth i ffv) 
			  (send final-model :prediction 
				(mapcar (lambda (z) (nth i z)) x)))))
	       (push (car (^ d 2)) z)))))

(defun qual-inf (x y reg-path &key (vb-select (repeat t (length x))) (cycle t))
"Args: (x y reg-path &key (vb-select (repeat t (length x))) (cycle t))
Predictors X, response Y, list of RAP's REG-PATH, returns
list of final model, leave-out-one final models, vector containing
no. of transformations which differ and vector containing no. of
weights that differ"
  (let* ((r (reg-model x y :print nil :vb-select vb-select))
	 (fr (regression-analyzer r reg-path :cycle cycle))
	 (ft (send fr :tran-list))
	 (fw (which (= 0 (send fr :weights))))
	 (z1 nil) (z2 nil) (z0 nil))
    (dotimes (i (length y) (list fr (reverse z0) (reverse z1) (reverse z2)))
             (let* ((im (reg-model (mapcar (lambda (z) (rmel i z)) x)
				   (rmel i y) 
				   :vb-select vb-select
				   :print nil))
		    (final-model (regression-analyzer im reg-path :cycle cycle))
		    (d1 (length (which 
				 (mapcar #'null 
					 (mapcar #'equal ft 
						 (send final-model :tran-list))))))
		    (w (which (= 0 (send final-model :weights))))
		    (aw (mapcar (lambda (z) (if (>= z i) (1+ z) z)) w))
		    (s1 (length (set-difference fw aw)))
		    (s2 (length (set-difference aw fw))))
	       (push final-model z0)
	       (push d1 z1)
	       (push (+ s1 s2) z2)))))

(defun nice-history (r)
"Message args: (r)
Displays the history of the analysis for model r"
  (let ((h (cdr (send r :history))))
    (format t "~%")
    (dolist (i h)
	    (format t "~21a ~a~%" (car i) (cadr i)))
    (format t "~%")))

(defun unique-models (rl)
"Args: (rl)
Returns list of list of unique models and list of occurences
for list of regression models rl"
  (let* ((ul (list (car rl)))
	 (al (list (send (car ul) :adjusted-rsquared)))
	 (reps (list 1)))
    (dolist (i (cdr rl) (list ul reps))
	    (let* ((ar (send i :adjusted-rsquared))
		   (j (which (= ar al))))
	      (if j
		  (let ((k (1+ (nth (car j) reps))))
		    (setf (select reps (car j)) k))
		(progn
		  (push ar al)
		  (push i ul)
		  (push 1 reps)))))))