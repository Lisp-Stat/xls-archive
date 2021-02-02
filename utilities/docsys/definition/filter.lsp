;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: :Definitions -*-
;;;  File Name          :  filter.lisp
;;;  Version Number     :  1.2
;;;  Last Changed Date  :  93/08/20 At 19:49:54
;;;  Created Date       :  8/20/93
;;;
;;; Copyright 1991. John Alan McDonald. All Rights Reserved. 
;;;
;;; Use and copying of this software eparation of derivative works
;;; based upon this software are permitted.  Any distribution of this
;;; software or derivative works must comply with all applicable United
;;; States export control laws.
;;; 
;;; This software is made available AS IS, and no warranty---about the
;;; software, its performance, or its conformity to any
;;; specification---is given or implied.
;;; 
;;;=======================================================

(in-package :Definitions)

;;;=======================================================
;;; filtering
;;;=======================================================

(defun exported-symbol? (s)
  "Is the symbol <s> exported from its package?"
  (declare (type Symbol s)
	   (:returns (type (Member t nil))))
  (multiple-value-bind
      (s1 status) (find-symbol (symbol-name s) (symbol-package s))
    (declare (ignore s1))
    (eq :external status)))

(defun exported-definition? (def)
  "Has the <definition-symbol> of <def> been exported?"
  (declare (type Definition def)
	   (:returns (type (Member t nil))))
  (if (kind-of-p def df:package-definition) t ;Packages are always "exported"
    (exported-symbol? (definition-symbol def))))

