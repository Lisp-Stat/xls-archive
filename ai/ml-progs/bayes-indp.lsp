;;;; A simple Bayes independence classification system. Uses a small epsilon instead of
;;;; zero for conditionals.  This frequently helps quite a bit.
;;;; See Weiss & Kulikowski Computer Systems that Learn book from Morgan Kaufman Pub.

;;;; Copyright (c) 1990 by Raymond Joseph Mooney. This program may be freely copied, used, or
;;;; modified provided that this copyright notice is included in each copy of this code
;;;; and parts thereof.

(in-package user)
(provide "bayes-indp")
(require "data-utilities"); (ml-progs-file "data-utilities"))

(setf (get 'bayes-indp 'expect-training-error) t) ; expect training error with bayes 
(setf (get 'bayes-indp 'parameters) '(*bayes-epsilon*))

(defparameter *bayes-epsilon* 0.001 "To be used instead of 0 in conditional probabilities")

;;; Instance descriptions are ordered sequences of feature values (lists or arrays allowed,
;;; arrays tend to be more efficient)

(defun train-bayes-indp (examples)
  (setf examples (make-ordered-examples examples))
  (let* ((num-examples (length examples))
	 (priors (calculate-class-priors examples num-examples)))
    (list priors
	  (calculate-conditionals examples num-examples priors))))

(defun calculate-class-priors (examples num-examples)
  (let ((priors (make-array (length *categories*) :element-type 'single-float :initial-element 0.0)))
    (dolist (ex examples)
      (incf (aref priors (position (first ex) *categories*))))
    (dotimes (i (length *categories*) priors)
      (setf (aref priors i) (if (zerop num-examples) 0.0 (/ (aref priors i) num-examples))))))

(defun calculate-conditionals (examples num-examples priors)
  (let* ((num-features (length *feature-names*))
	 (conditionals (make-array num-features :element-type 'list)))
    (dotimes (feature-num (length *feature-names*))
      (setf (aref conditionals feature-num)
	    (mapcar #'(lambda (val) (declare (ignore val))
			(make-array (length *categories*) :element-type 'single-float :initial-element 0.0))
		    (feature-domain feature-num))))
    (dolist (ex examples conditionals)
      (dotimes (feature-num num-features)
	(let* ((val (elt (second ex) feature-num))
	       (val-seq (elt conditionals feature-num))
	       (cat-seq (unless (eq val *missing-value*)
			  (elt val-seq (position val (feature-domain feature-num))))))
	  (when cat-seq (incf (aref cat-seq (position (first ex) *categories*)))))))
    (dotimes (f (length *feature-names*) conditionals)
      (dolist (ca (aref conditionals f))
	(dotimes (c (length *categories*))
	  (setf (aref ca c) (if (and (zerop (aref ca c)) (zerop (aref priors c)))
				0.0
				(/ (aref ca c) (* num-examples (aref priors c))))))))))

(defun test-bayes-indp (example train-result)
  (setf example (make-ordered-example example))
  (let* ((priors (first train-result))
	 (conditionals (second train-result)))
    (maximum-category-label
      (mapcar #'(lambda (cat)
		  (list cat (* (aref priors (position cat *categories*))
			       (multiply-conditionals (second example) cat conditionals))))
	      *categories*)
      *categories*)))

(defun maximum-category-label (count-alist &optional tie-breaker-list)
  "Returns the label in count-alist ((label . count) ...)
   with the maximum count.  Break ties according to *tie-breaker*"
  (let (max-labels (max-count 0))
    (dolist (count-cons count-alist)
      (cond ((> (second count-cons) max-count)
	     (setf max-count (second count-cons))
	     (setf max-labels (list (car count-cons))))
	    ((= (second count-cons) max-count)
	     (push (first count-cons) max-labels))))
    (if (or (eq *tie-breaker* 'random) (null tie-breaker-list))
	(pick-one max-labels)
	(dolist (item tie-breaker-list)
	  (when (member item max-labels)
	    (return item))))))


(defun multiply-conditionals (instance cat conditionals)
  (let ((product 1))
    (setf product (coerce product 'double-float))
    (dotimes (feature-num (length instance) product)
	(let* ((val (elt instance feature-num))
	       (val-seq (elt conditionals feature-num))
	       (cat-seq (unless (eq val *missing-value*)
			  (elt val-seq (position val (feature-domain feature-num)))))
	       (p (when cat-seq (aref cat-seq (position cat *categories*)))))
	  (when p (setf product (* product (if (= p 0) *bayes-epsilon* p))))))))


(make-variant bayes-indp0 bayes-indp ((*bayes-epsilon* 0)))
